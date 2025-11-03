#' Fit an (relaxed) lasso or elastic net penalized regression via glmnet
#'
#' A wrapper for `glmnet` and `cv.glmnet`. It runs CV by default so remember to
#' set your seed for reproducibility.
#'
#' @param formula a formula
#' @param data data set
#' @param family outcome distributional family
#' @param lambda can be `best`, `compact` (which use CV), or a numeric vector. See details.
#' @param ... Additional arguments that can be passed to `glmnet`, e.g. `alpha`
#' @param fitted_selector a previously fit `selector`, used for resampling
#'
#' @importFrom magrittr %>%
#' @importFrom glmnet cv.glmnet glmnet
#'
#' @return A `selector` object wrapping `glmnet` containing:
#' \item{beta}{a tibble containing term names and coefficients}
#' \item{std}{Was desing matrix standadrized}
#' \item{penalty}{penalty used (lasso or MCP)}
#' \item{lambda}{are the coefficeint associated with "lambda.min" or "lambda.1se"}
#' \item{lambda.select}{numeric value of selected lambda}
#' \item{fold}{Which fold each observation belongs to. By default the observations are randomly assigned.}
#' \item{x}{ the model dataframe used}
#' \item{y}{repsonse used}
#' \item{alpha}{selected alpha for model fitting}
#' @export

select_glmnet <- function(
  formula, data, family = c("gaussian", "binomial", "poisson"),
  lambda = c("best", "compact"),
  fitted_selector = NULL,
  ...){

  family = match.arg(family)

  # If this has never been fit before, check args
  if(is.null(fitted_selector)) {
    family = match.arg(family)
    lambda <- match.arg(lambda)
    if(missing(formula))
      stop("Must supply formula")

  } else {
    meta <- attr(fitted_selector, "meta")
    family <- meta$family
    lambda <- meta$lambda_used
    if(missing(formula))
      formula <- attr(fitted_selector, "recipe_obj")
  }

  # Initial pre-processing
  rec_obj <- formula

  # If a typical formula is supplied, will center/scale
  if(!inherits(rec_obj, "recipe")) {

    rec_obj <- recipe(formula, data = data) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) %>%
      step_dummy(all_factor_predictors(),
                 naming = function(...) dummy_names(..., sep = "")) %>%
      prep()

      # bake first observation for names
      y1 <- bake(rec_obj, new_data = data[1,,drop = FALSE], all_outcomes())
      X1 <- bake(rec_obj, new_data = data[1,,drop=FALSE], all_predictors())
      df1 <- bake(rec_obj, new_data = data[1,,drop = FALSE])


      # collect all terms
      formula_full <- as.formula(paste0(names(y1), " ~ ", paste0(names(X1), collapse = " + ")))
      all_terms <- colnames(model.matrix(formula_full, data = df1))

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
    fit <- cv.glmnet(x = as.matrix(as.data.frame(X)), y = as.numeric(y[[1]]),
                     family = family, keep = TRUE, ...)
    lambda_used <- if(lambda == "best") fit[["lambda.min"]] else fit$lambda[which(fit$cve < min(fit$cve + fit$cvse))[1]]
    cv_used <- TRUE
    ll <- ifelse(lambda == "best", "lambda.min", "lambda.1se")
    b <- as.matrix(coef(fit, s = ll))

  } else {
    fit <- glmnet(x = as.matrix(as.data.frame(X)), y = as.numeric(y[[1]]),
                  family = family, lambda = lambda, ...)
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
    ellipses = list(...)
  )

  as_selector(fit, "glmnet", label = "Penalized `glmnet`-based",
              all_terms = all_terms, recipe_obj = rec_obj,
              selected_coefs = selected_coefs,
              default_infer = "selective", meta = meta_information)
}
