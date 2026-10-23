#' MCP, SCAD or lasso selection via `ncvreg`
#'
#' A wrapper for [ncvreg::cv.ncvreg()]. The penalty is ncvreg's default, MCP,
#' unless `penalty = "SCAD"` or `"lasso"` is passed through `...`.
#' Cross-validation is random, so set a seed for reproducibility. Predictors
#' are standardized first; see [tidy.selector()].
#'
#' @details
#' With `lambda = "best"` the penalty is `lambda.min`, the value minimizing
#' cross-validated error; with `"compact"` it is `lambda.1se`, the largest
#' value whose cross-validated error is within one standard error of the
#' minimum, which gives a sparser model.
#'
#' @param formula a model formula, as for [lm()]. It may include
#'   interactions (`x * z`) and in-formula transformations (`log(x)`,
#'   `poly(x, 2)`); factor, logical and character predictors are
#'   treatment-coded. Ordered factors and missing values are not supported.
#'   See [tidy.selector()] for how the columns are standardized and named.
#' @param data a data frame
#' @param family outcome distributional family
#' @param lambda `"best"` (default) or `"compact"`, both chosen by
#'   cross-validation (see Details)
#' @param ... additional arguments passed to [ncvreg::cv.ncvreg()], e.g.
#'   `penalty`, `gamma`, `alpha`, `nfolds`
#' @param fitted_selector a previously fit `selector`, used by [reselect()]
#'
#' @importFrom magrittr %>%
#' @importFrom ncvreg cv.ncvreg ncvreg
#'
#' @return a `selector` wrapping the `cv.ncvreg` fit; see [tidy.selector()].
#'   Its default inference method is [infer_boot()].
#' @export

select_ncvreg <- function(
  formula, data, family = c("gaussian", "binomial", "poisson"),
  lambda = c("best", "compact"),
  fitted_selector = NULL,
  ...){

  dots <- list(...)

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
    lambda_seq <- meta$lambda_seq
    formula <- attr(fitted_selector, "orig_formula")

    # Replay `...` (penalty, gamma, penalty.factor, ...) from meta, or every
    # bootstrap replicate quietly fits a different estimator: without this a
    # penalty = "lasso" selector was refit with ncvreg's default, MCP
    if(!length(dots) && length(meta$ellipses))
      dots <- meta$ellipses
  }

  design <- if (is.null(fitted_selector)) design_build(formula, data) else
    attr(fitted_selector, "design")
  X <- design_X(design, data)
  y <- design_y(design, data)

  if(is.character(lambda)) {
    fit <- do.call(cv.ncvreg, c(
      list(X = X, y = as.numeric(y), family = family), dots))
    lambda_used <- if(lambda == "best") fit[["lambda.min"]] else
      fit$lambda[which(fit$cve < min(fit$cve + fit$cvse))[1]]
    cv_used <- TRUE
    ll <- ifelse(lambda == "best", "lambda.min", "lambda.1se")
    b <- as.matrix(coef(fit, lambda = lambda_used))
    lambda_seq <- fit$lambda
    penalty <- fit$fit$penalty

  } else {
    fit <- do.call(ncvreg, c(
      list(X = X, y = as.numeric(y), family = family, lambda = lambda_seq),
      dots))
    lambda_used <- lambda
    cv_used <- FALSE
    b <- as.matrix(coef(fit, lambda = lambda))
    penalty <- fit$penalty
  }

  selected_coefs <- b[b!=0]
  names(selected_coefs) <- rownames(b)[b!=0]

  meta_information <- list(
    family = family,
    penalty = penalty,
    lambda = lambda,
    lambda_used = lambda_used,
    lambda_seq = lambda_seq,
    cv_info = list(cv_used = cv_used, foldid = fit$foldid),
    ellipses = dots
  )

  as_selector(fit, "ncvreg", label = "Penalized `ncvreg`-based",
              design = design,
             orig_formula =formula,
              selected_terms =  names(selected_coefs),
              selected_coefs = selected_coefs,
              default_infer = "boot", meta = meta_information)
}

