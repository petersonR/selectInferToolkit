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

    if(use_cv_sigma) {
      sig <- selectiveInference::estimateSigma(as.matrix(X), y)$sigmahat
      warning("use_cv_sigma with stepwise_ic may yield unexpected results")
    }

    ## Run stepwise IC for purpose of SI, using fs function
    fs_result <- fs(as.matrix(X), y)

    # Get IC-based selection with confidence intervals
    mult <- ifelse(meta$penalty == "AIC", 2, log(nrow(n)))

    res <- selectiveInference::fsInf(
      fs_result,
      sigma = sig,
      type = "aic",
      mult = mult,
      alpha = (1 - conf.level) / 2,
      ntimes = 1,
      ...
    )
    names(res$vars) <- names(X)[res$vars]
  }

  ## Run selective inference on glmnet
  if(type == "glmnet") {
    n<- nrow(X)
    sig <- NULL
    if(use_cv_sigma)
      sig <- min(sqrt(object$cvm))

    b <- coef(object, use_native = TRUE, s=meta$lambda_used,
              exact = TRUE, x = X, y = y)[-1]

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

  }

  bb <- res$vmat %*% y
  inferences <- data.frame(term = names(res$vars), selected = 1, estimate = bb,
                           ci_low = res$ci[,1], ci_high = res$ci[,2],
                           p_value = res$pv)

  # Handle non-selections
  results <- fill_in_nonselections(inferences, object,
                                   nonselection = nonselection, X = X, y = y,
                                   conf.level = conf.level)

  # Return inferrer class
  as_inferrer(
    res, "selective", label = "Selective",
    nonselection = nonselection,
    conf.level = conf.level, selector = object, meta = list(),
    inferences = results)
}

