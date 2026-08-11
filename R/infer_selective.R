#' Inference based on selective inference
#'
#' A wrapper for the `selectiveInference` functions on `selector` objects
#'
#' @param object a `selector` object
#' @param data data must be passed to infer
#' @param nonselection  A character string specifying how to handle variables
#'   not selected by model selection procedure. One of "ignored",
#'   "confident_nulls" or "uncertain_nulls" supported
#' @param conf.level .95 by default
#' @param use_cv_sigma estimate Sigma via CV (if FALSE, uses SI defaults)
#' @param ... arguments passed to `selectiveInference` function(s)
#'
#' @importFrom broom tidy
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom selectiveInference fs fsInf fixedLassoInf
#'
#' @return `inferrer` object
#' @rdname infer
#' @export
#'
infer_selective <- function(
    object,
    data,
    nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
    conf.level = .95,
    use_cv_sigma = FALSE,
    ...
  ){

  # Check method supported
  supported <- c("glmnet", "stepwise_ic")
  type <- attr(object, "name")


  if(!(type %in% supported))
    stop("Currently SI only supported for stepwise IC or `glmnet`")

  if (type == "glmnet") {
    alpha <- attr(object, "meta")[["alpha"]]

    if (is.null(alpha) || alpha != 1) {
      stop("Currently SI only supported for glmnet with alpha = 1 (lasso penalty)")
    }
  }

  nonselection <- match.arg(nonselection)

  # grab useful components from model
  X <- bake(attr(object, "recipe_obj"), new_data = data, all_predictors())
  y <- bake(attr(object, "recipe_obj"), new_data = data, all_outcomes())[[1]]
  beta <- coef(object)
  meta <- attr(object, "meta")

  sig <- NULL

  if(type == "stepwise_ic") {
    if(meta$direction != "forward")
      stop("Only forward stepwise with IC currently supported")

    if(meta$family != "gaussian")
      stop("Only Gaussian supported for selective inference with stepwise IC (try glmnet?)")

    ## Run stepwise IC for purpose of SI, using fs function
    fs_result <- fs(as.matrix(X), y)

    # Determine sigma
    p <- ncol(X)
    n_obs <- length(y)

    if(use_cv_sigma) {
      sig <- selectiveInference::estimateSigma(as.matrix(X), y)$sigmahat
      warning("use_cv_sigma with stepwise_ic may yield unexpected results")
    } else if(p > n_obs / 2) {
      sig <- selectiveInference::estimateSigma(as.matrix(X), y)$sigmahat
      message("p > n/2: using estimateSigma() for sigma estimate in fsInf")
    } else {
      sig <- NULL  # let fsInf use its default (sd(y) is fine when p <= n/2)
    }

    sel_vars_stepaic <- names(beta)[names(beta) != "(Intercept)"]

    if(length(sel_vars_stepaic) == 0) {
      # Intercept-only model: there is no selection event to condition on, so
      # there is nothing for fsInf() to do. Fall back to the unadjusted fit of
      # the intercept-only model; the nonselection handling below then fills in
      # every predictor according to `nonselection`.
      message("No variables selected: falling back to unadjusted inference on the intercept-only model")

      empty_model <- tidy(infer_upsi(object, data = data))

      inferences <- data.frame(term = empty_model$term, selected = 1,
                               estimate = empty_model$estimate,
                               ci_low = empty_model$ci_low,
                               ci_high = empty_model$ci_high,
                               p_value = empty_model$p_value)
      # list(), not NULL: as_inferrer() sets attributes on this object, and
      # structure(NULL, ...) is deprecated in R.
      res <- list()

    } else {
      # Get IC-based selection with confidence intervals
      mult <- ifelse(meta$penalty == "AIC", 2, log(length(y)))

      res <- selectiveInference::fsInf(
        fs_result,
        sigma = sig,
        type = "aic",
        mult = mult,
        alpha = (1 - conf.level) / 2,
        ...
      )
      names(res$vars) <- names(X)[res$vars]
      bb <- res$sign* as.vector(res$vmat %*% y)

      # fsInf(type = "aic") re-runs its own AIC/BIC stopping rule, which does not
      # always terminate at the same step as MASS::stepAIC() did. When the two
      # disagree, fall back to type = "active" and pin the step count to the
      # number of variables select_stepwise_ic() actually kept, so the inference
      # conditions on the model the user was given. (`mult` is not used by
      # type = "active" and so is not passed here.)
      if(length(sel_vars_stepaic) != length(res$vars)) {
        res <- selectiveInference::fsInf(
          fs_result,
          sigma = sig,
          type = "active",
          k = length(beta) - 1,
          alpha = (1 - conf.level) / 2,
          ...
        )
        names(res$vars) <- names(X)[res$vars]
        bb <- res$sign* as.vector(res$vmat %*% y)
      }

      inferences <- data.frame(term = names(res$vars), selected = 1, estimate = bb,
                               ci_low = res$ci[,1], ci_high = res$ci[,2],
                               p_value = res$pv)

    }
  }

  ## Run selective inference on glmnet
  if(type == "glmnet") {
    n<- nrow(X)
    sig <- NULL
    b <- coef(object, use_native = TRUE, s=meta$lambda_used,
              exact = TRUE, x = X, y = y)[-1]

    if(use_cv_sigma) {
      # Similar to estimateSigma, but no new CV required
      nz = sum(b != 0)
      rss <- min(object$cvm) * n
      sig <- sqrt(rss/(n - nz - 1))
    }

    # fixed lasso function requires no intercept in beta vector
    if(all(b == 0)) {
      warning("No betas selected at that value of lambda")
      res <- list(
        vmat = rbind(rep(NA, length(y))),
        vars = c(None = NA),
        ci = cbind(NA, NA),
        pv = NA
      )

    } else {
      res <- selectiveInference::fixedLassoInf(
        x = as.matrix(X),
        y = y,
        beta = b,
        lambda = meta$lambda_used * n,
        family = meta$family,
        alpha = (1 - conf.level) / 2,
        sigma = sig,
        ...
      )
    }
    bb <- res$vmat %*% y
    inferences <- data.frame(term = names(res$vars), selected = 1, estimate = bb,
                             ci_low = res$ci[,1], ci_high = res$ci[,2],
                             p_value = res$pv)
  }


  # Handle non-selections
  term_to_col <- tibble(
    term =  colnames(X),
    col  = colnames(X))

  results <- fill_in_nonselections(inferences, object,
                                   nonselection = nonselection, X = X, y = y,
                                   conf.level = conf.level, term_to_col = term_to_col )

  # Return inferrer class
  as_inferrer(
    res, "selective", label = "Selective",
    nonselection = nonselection,
    conf.level = conf.level, selector = object, meta = list(),
    inferences = results)
}

