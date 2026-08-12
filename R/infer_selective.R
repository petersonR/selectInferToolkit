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
#' @param sigma optional known or pre-computed residual standard deviation. When
#'   supplied it is passed straight through to the `selectiveInference` function
#'   and no sigma is estimated. The automatic estimate used when `p > n/2`
#'   calls `selectiveInference::estimateSigma()`, which runs its own unseeded
#'   cross-validation, so intervals will differ between calls on identical data
#'   unless you `set.seed()` first.
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
    sigma = NULL,
    ...
  ){

  if (!inherits(object, "selector"))
    stop("`object` must be a `selector`. ",
         "Did you pass an `inferrer` by mistake? Use a select_* function first.")

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
  # recorded on the returned inferrer so callers can tell when the numbers are
  # not actually selection-adjusted
  si_meta <- list()

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

    if(!is.null(sigma)) {
      sig <- sigma
    } else if(use_cv_sigma) {
      sig <- selectiveInference::estimateSigma(as.matrix(X), y)$sigmahat
      warning("use_cv_sigma with stepwise_ic may yield unexpected results")
    } else if(p > n_obs / 2) {
      sig <- selectiveInference::estimateSigma(as.matrix(X), y)$sigmahat
      message(paste("p > n/2: sigma estimated using cross-validation, so set.seed()",
                    "or supply `sigma` for reproducible intervals."))
    } else {
      sig <- NULL  # let fsInf use its default (sd(y) is fine when p <= n/2)
    }

    sel_vars_stepaic <- names(beta)[names(beta) != "(Intercept)"]

    if(length(sel_vars_stepaic) == 0) {
      # Intercept-only model: there is no selection event to condition on, so
      # there is nothing for fsInf() to do. Fall back to the unadjusted fit of
      # the intercept-only model; the nonselection handling below then fills in
      # every predictor according to `nonselection`.

      # warning(), not message(): knitr, suppressMessages() and simulation loops
      # all swallow messages, and these numbers are NOT selection-adjusted.
      warning(paste("No variables selected: falling back to unadjusted (non-selective)",
                    "inference on the intercept-only model"))
      si_meta$unadjusted_fallback <- TRUE

      empty_model <- tidy(infer_upsi(object, data = data, conf.level = conf.level))

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
        # stop at the first IC increase, which is what MASS::stepAIC() does.
        # fsInf's default (ntimes = 2) walks past it
        ntimes = 1,

        # not to be confused with the other alpha...
        alpha = 1 - conf.level,
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
      si_meta$conditioning <- "aic"
      if(length(sel_vars_stepaic) != length(res$vars)) {
        # type = "active" conditions on the first k steps as though k had been
        # fixed in advance, so it does not adjust for the IC stopping rule.
        # Record that so the weaker guarantee is visible on the result.
        si_meta$conditioning <- "active"
        res <- selectiveInference::fsInf(
          fs_result,
          sigma = sig,
          type = "active",
          k = length(beta) - 1,
          alpha = 1 - conf.level,
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
    p <- ncol(X)
    sig <- NULL
    b <- coef(object, use_native = TRUE, s=meta$lambda_used,
              exact = TRUE, x = X, y = y)[-1]

    if(!is.null(sigma)) {
      sig <- sigma
    } else if(use_cv_sigma) {
      # Similar to estimateSigma, but no new CV required.
      nz = sum(b != 0)
      rss <- min(object$cvm) * n
      sig <- sqrt(rss/(n - nz - 1))

    } else if(p > n / 2) {

      sig <- selectiveInference::estimateSigma(as.matrix(X), y)$sigmahat
      message(paste("p > n/2: sigma estimated with cross-validation, so set.seed()",
                    "or supply `sigma` for reproducible intervals."))
    }

    # fixed lasso function requires no intercept in beta vector
    if(all(b == 0)) {
      warning("No betas selected at that value of lambda")
      si_meta$unadjusted_fallback <- TRUE
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
        alpha = 1 - conf.level,
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
    conf.level = conf.level, selector = object, meta = si_meta,
    inferences = results)
}

