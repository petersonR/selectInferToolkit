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
#' Cross-validated error is nearly flat over the stretch of the path where the
#' selected model doesn't change, so the chosen penalty can sit at the top of
#' that stretch, near the smallest selected effect rather than the noise
#' level (see GitHub issue #34). The selector also stores
#' `lambda_eff`, the smallest penalty on the path next to the chosen one that
#' gives the same selected model (in `attr(x, "meta")`). Predictions and
#' `coef()` use the chosen penalty; [infer_boot()] refits its resamples at
#' `lambda_eff` and pads with it.
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
    # "compact": the largest lambda whose CV error is within one SE of the
    # minimum (the 1-SE rule)
    lambda_used <- if(lambda == "best") fit[["lambda.min"]] else
      fit$lambda[which(fit$cve <= fit$cve[fit$min] + fit$cvse[fit$min])[1]]
    cv_used <- TRUE
    ll <- ifelse(lambda == "best", "lambda.min", "lambda.1se")
    b <- as.matrix(coef(fit, lambda = lambda_used))
    lambda_seq <- fit$lambda
    penalty <- fit$fit$penalty
    lambda_eff <- ncvreg_lambda_eff(fit$fit, lambda_used, lambda_seq)

  } else {
    # Fit the path only down to `lambda` (at least two values, as ncvreg
    # needs): warm starts run from the top, so the fit at `lambda` is the same,
    # and the path below it, where a binomial fit is likelier to saturate, is
    # never needed
    lambda_fit <- lambda_seq[lambda_seq >= lambda]
    if (length(lambda_fit) < 2) lambda_fit <- lambda_seq[1:2]
    fit <- do.call(ncvreg, c(
      list(X = X, y = as.numeric(y), family = family, lambda = lambda_fit),
      dots))
    lambda_used <- lambda
    # ncvreg ends the path early when the fit saturates (e.g. a binomial fit
    # with separation) or passes `dfmax`. If that happens above `lambda`,
    # take the fit at the smallest lambda reached
    if (lambda < min(fit$lambda)) {
      lambda_used <- min(fit$lambda)
      warning(structure(class = c("ncvreg_short_path", "warning", "condition"),
        list(message = paste0("The path ended before lambda = ",
                              signif(lambda, 3), " (the fit saturated or ",
                              "reached dfmax); using the smallest lambda ",
                              "reached, ", signif(lambda_used, 3), "."),
             call = NULL)))
    }
    cv_used <- FALSE
    b <- as.matrix(coef(fit, lambda = lambda_used))
    penalty <- fit$penalty
    # not known: the path below `lambda` isn't fit, and the original
    # selection's lambda_eff belongs to other data. infer_boot() then refits
    # at lambda_used
    lambda_eff <- NULL
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
  meta_information$lambda_eff <- lambda_eff

  as_selector(fit, "ncvreg", label = "Penalized `ncvreg`-based",
              design = design,
             orig_formula =formula,
              selected_terms =  names(selected_coefs),
              selected_coefs = selected_coefs,
              default_infer = "boot", meta = meta_information)
}


# The smallest lambda on the path, in the run of lambdas next to
# `lambda_used`, whose active set is the same as at `lambda_used` (issue #34).
# Only the contiguous run counts: MCP and SCAD paths are not monotone, and a
# lambda further down where the same set reappears is a different model.
# Only lambdas in `lambda_seq` count: cv.ncvreg() drops the lambdas at which
# a fold's fit is not finite (e.g. a saturated binomial fit), and resamples
# are refit on lambda_seq.
ncvreg_lambda_eff <- function(path, lambda_used, lambda_seq) {
  active <- path$beta[-1, , drop = FALSE] != 0
  i <- which.min(abs(path$lambda - lambda_used))
  L <- length(path$lambda)
  # lambda decreases along the path
  while (i < L && path$lambda[i + 1] %in% lambda_seq &&
         identical(active[, i + 1], active[, i])) i <- i + 1
  path$lambda[i]
}
