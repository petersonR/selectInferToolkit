#' Lasso or elastic net selection via `glmnet`
#'
#' A wrapper for [glmnet::cv.glmnet()] (or [glmnet::glmnet()] at a fixed
#' `lambda`). Cross-validation is random, so set a seed for reproducibility.
#' Predictors are standardized first; see [tidy.selector()].
#'
#' @details
#' With `lambda = "best"` the penalty is `lambda.min`, the value minimizing
#' cross-validated error; with `"compact"` it is `lambda.1se`, the largest
#' value whose cross-validated error is within one standard error of the
#' minimum, which gives a sparser model.
#'
#' Only the lasso (`alpha = 1`) is supported by [infer_selective()].
#'
#' @param formula a formula
#' @param data a data frame
#' @param family outcome distributional family
#' @param lambda `"best"` (default) or `"compact"`, both chosen by
#'   cross-validation (see Details), or a single number giving a fixed
#'   penalty.
#' @param alpha the elastic net mixing parameter, passed to `glmnet`: 1
#'   (default) is the lasso
#' @param ... additional arguments passed to [glmnet::cv.glmnet()] or
#'   [glmnet::glmnet()], e.g. `nfolds`, `penalty.factor`
#' @param fitted_selector a previously fit `selector`, used by [reselect()]
#'
#' @importFrom magrittr %>%
#' @importFrom glmnet cv.glmnet glmnet
#'
#' @return a `selector` wrapping the `cv.glmnet` (or `glmnet`) fit; see
#'   [tidy.selector()]. Its default inference method is [infer_selective()].
#' @export

select_glmnet <- function(
  formula, data, family = c("gaussian", "binomial", "poisson"),
  lambda = c("best", "compact"),
  fitted_selector = NULL,
  alpha = 1,
  ...){

  family = match.arg(family)
  dots <- list(...)

  # If this has never been fit before, check args
  if(is.null(fitted_selector)) {
    family = match.arg(family)
    # `lambda` is either a keyword ("best"/"compact", both of which run CV) or a
    # single numeric penalty.
    if(is.character(lambda)) {
      lambda <- match.arg(lambda)
    } else if(!(is.numeric(lambda) && length(lambda) == 1)) {
      stop('`lambda` must be "best", "compact", or a single numeric value')
    }
    if(missing(formula))
      stop("Must supply formula")

  } else {
    meta <- attr(fitted_selector, "meta")
    family <- meta$family
    lambda <- meta$lambda_used

    # alpha must be carried over too for re-selection
    if(!is.null(meta$alpha))
      alpha <- meta$alpha

    # Same for `...`: anything originally passed through `...`
    # (penalty.factor, weights, nlambda, intercept, ...) must be replayed from
    # meta or every bootstrap replicate quietly fits a different estimator.
    if(!length(dots) && length(meta$ellipses))
      dots <- meta$ellipses

    if(missing(formula))
      formula <- attr(fitted_selector, "recipe_obj")
  }

  # Initial pre-processing
  rec_obj <- formula

  # If a typical formula is supplied, will center/scale
  if(!inherits(rec_obj, "recipe")) {

    rec_obj <- recipe(formula, data = data) %>%
      step_logical_to_indicator(data) %>%
      step_dummy(all_factor_predictors(),
                 naming = function(...) dummy_names(..., sep = "")) %>%
      step_zv(all_predictors()) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) %>%
       prep()

      # bake first observation for names
      y1 <- bake(rec_obj, new_data = data[1,,drop = FALSE], all_outcomes())
      X1 <- bake(rec_obj, new_data = data[1,,drop=FALSE], all_predictors())
      df1 <- bake(rec_obj, new_data = data[1,,drop = FALSE])


      # collect all terms
      formula_full <- as.formula(paste0(names(y1), " ~ ", paste0(names(X1), collapse = " + ")))
      mm <- model.matrix(formula, data = data)
      all_terms <- make.names(colnames(mm))
      if(all_terms[1] =="X.Intercept.") all_terms[1] = "(Intercept)"

      # add additional zero variance step
      rec_obj <- rec_obj %>%
        step_zv() %>%
        prep()

      attr(rec_obj, "prepped_selector_recipe") <- TRUE
  } else if(!attr(rec_obj, "prepped_selector_recipe")) {
    stop("custom recipes not yet supported")
  } else {
    # May need to read additional things here if this is re-called later
    all_terms <- attr(fitted_selector, "all_terms")
  }

  # Create model matrix
  X <- bake(rec_obj, new_data = data, all_predictors())
  y <- bake(rec_obj, new_data = data, all_outcomes())
  df <- bake(rec_obj, new_data = data)

  if(is.character(lambda)) {
    fit <- do.call(cv.glmnet, c(
      list(x = as.matrix(as.data.frame(X)), y = as.numeric(y[[1]]),
           family = family, keep = TRUE, standardize = FALSE, alpha = alpha),
      dots))
    lambda_used <- if(lambda == "best") fit[["lambda.min"]] else fit[["lambda.1se"]]
    cv_used <- TRUE
    ll <- ifelse(lambda == "best", "lambda.min", "lambda.1se")
    b <- as.matrix(coef(fit, s = ll))

  } else {
    fit <- do.call(glmnet, c(
      list(x = as.matrix(as.data.frame(X)), y = as.numeric(y[[1]]),
           family = family, lambda = lambda, standardize = FALSE, alpha = alpha),
      dots))
    lambda_used <- lambda
    cv_used <- FALSE
    b <- as.matrix(coef(fit, s = lambda))
  }

  selected_coefs <- b[b!=0]
  names(selected_coefs) <- rownames(b)[b!=0]

  meta_information <- list(
    family = family,
    lambda = lambda,
    lambda_used = lambda_used,
    cv_info = list(cv_used = cv_used, foldid = fit$foldid),
    ellipses = dots,
    alpha = alpha
  )

  as_selector(fit, "glmnet", label = "Penalized `glmnet`-based",
              all_terms = all_terms, recipe_obj = rec_obj,
               orig_formula =formula,
              selected_terms = names(selected_coefs),
              selected_coefs = selected_coefs,
              default_infer = "selective", meta = meta_information)
}
