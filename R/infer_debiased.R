
#' Debiased (de-sparsified) lasso inference
#'
#' Wraps `hdi::lasso.proj()` using the full standardized numeric design
#' associated with a `select_glmnet()` selector fitted with `alpha = 1`.
#'
#' The selector determines which terms are marked as selected.
#' `hdi` fits its own initial lasso for bias correction.
#'
#' Inference targets coefficients of the assumed full regression model,
#' not coefficients in the selected submodel.
#'
#' @param object A fitted selector returned by select_glmnet()
#'   with alpha = 1. Other selectors are rejected.
#' @param data Data used for the selector. The stored design is reused.
#' @param conf.level Two-sided marginal confidence level, in (0, 1).
#' @param betainit Initial estimator used by hdi::lasso.proj().
#'   Either "cv lasso" (default) or "scaled lasso".
#' @param robust Use robust standard errors in hdi::lasso.proj().
#' @param ... Additional arguments forwarded to hdi::lasso.proj(),
#'   such as parallel, ncores, Z, return.Z, and verbose.
#'
#' @details
#' Requires the optional hdi package.
#'
#' Gaussian and binomial families are supported; Poisson is not.
#'
#' Inference is reported for ALL candidate predictor coefficients,
#' including coefficients not selected by the original lasso.
#' The selected column remains descriptive metadata.
#'
#' The intercept receives NA inference because hdi::lasso.proj()
#' does not provide an intercept confidence interval.
#'
#' Reported p-values are marginal, not Holm-adjusted.
#' Intervals are not selection-conditional.
#'
#' Predictors are represented using the standardized design.
#' tidy(fit, scale_coef = FALSE) rescales predictor slopes.
#'
#' @return An inferrer object compatible with tidy.inferrer().
#'
#' @references
#' Dezeure R, Buehlmann P, Meier L, Meinshausen N (2015).
#' High-Dimensional Inference: Confidence Intervals, p-Values
#' and R-Software hdi. Statistical Science 30(4):533-558.
#'
#' @seealso [infer_upsi()], [infer_fcr()]
#' @export
infer_debiased <- function(
    object,
    data,
    conf.level = 0.95,
    betainit = "cv lasso",
    robust = FALSE,
    ...
) {

  # Validate selector
  if (!inherits(object, "selector")) {
    stop("`object` must be a fitted `selector`.",
         call. = FALSE)
  }

  # Only glmnet lasso selectors
  if (!identical(attr(object, "name"), "glmnet")) {
    stop(
      "`infer_debiased()` requires a `select_glmnet()` selector ",
      "with `alpha = 1`.",
      call. = FALSE
    )
  }

  alpha <- attr(object, "meta")$alpha

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha != 1) {
    stop(
      "`infer_debiased()` requires `select_glmnet(alpha = 1)` ",
      "(lasso); elastic net is not supported.",
      call. = FALSE
    )
  }

  if (missing(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }

  if (!requireNamespace("hdi", quietly = TRUE)) {
    stop(
      "`infer_debiased()` requires the optional `hdi` package. Unfortunetly it is not aviaalble on CRAN
      anymore. But it can be donwlaoded from Github",
      call. = FALSE
    )
  }

  # Validate confidence level
  if (!is.numeric(conf.level) ||
      length(conf.level) != 1L ||
      !is.finite(conf.level) ||
      conf.level <= 0 ||
      conf.level >= 1) {
    stop(
      "`conf.level` must be a finite number in (0, 1).",
      call. = FALSE
    )
  }

  # Validate initial estimator
  if (!is.character(betainit) ||
      length(betainit) != 1L ||
      !betainit %in% c("cv lasso", "scaled lasso")) {
    stop(
      "`betainit` must be 'cv lasso' or 'scaled lasso'.",
      call. = FALSE
    )
  }

  if (!is.logical(robust) ||
      length(robust) != 1L ||
      is.na(robust)) {
    stop(
      "`robust` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  # Recover existing design
  design <- attr(object, "design")
  family <- attr(object, "meta")$family

  if (!family %in% c("gaussian", "binomial")) {
    stop(
      "`infer_debiased()` supports gaussian and binomial, not ",
      family, ".",
      call. = FALSE
    )
  }

  X <- design_X(design, data)
  y <- design_y(design, data)

  if (!is.matrix(X) ||
      !is.numeric(X) ||
      ncol(X) < 2L ||
      nrow(X) < 4L) {
    stop(
      "Debiasing requires a numeric matrix with >=2 columns ",
      "and >=4 observations.",
      call. = FALSE
    )
  }

  if (any(!is.finite(X))) {
    stop(
      "The design contains non-finite values.",
      call. = FALSE
    )
  }

  # Constant columns can make nodewise regression undefined
  column_sd <- apply(X, 2L, stats::sd)

  if (any(!is.finite(column_sd)) ||
      any(column_sd <= 0)) {
    stop(
      "The full design contains constant columns on `data`.",
      call. = FALSE
    )
  }

  # Validate outcome
  if (family == "binomial") {

    if (is.factor(y)) {

      if (nlevels(y) != 2L) {
        stop(
          "Binomial response must have two levels.",
          call. = FALSE
        )
      }

      y <- as.integer(y) - 1L
    }

    if (!is.numeric(y) ||
        anyNA(y) ||
        !all(y %in% c(0, 1)) ||
        length(unique(y)) != 2L) {
      stop(
        "Binomial response must contain both 0 and 1.",
        call. = FALSE
      )
    }

  } else if (!is.numeric(y) ||
             any(!is.finite(y))) {
    stop(
      "Gaussian response must be finite numeric.",
      call. = FALSE
    )
  }

  # Fit de-sparsified lasso using the entire predictor matrix
  #
  # Do not subset X to the selected variables.
  # The initial lasso and nodewise regressions are handled by hdi.
  backend <- hdi::lasso.proj(
    x = X,
    y = y,
    family = family,
    standardize = FALSE,
    betainit = betainit,
    robust = robust,
    suppress.grouptesting = TRUE,
    multiplecorr.method = "holm",
    ...
  )

  cols <- colnames(X)

  # Check backend results
  if (!all(cols %in% names(backend$bhat)) ||
      !all(cols %in% names(backend$se)) ||
      !all(cols %in% names(backend$pval))) {
    stop(
      "`hdi::lasso.proj()` did not return all expected columns.",
      call. = FALSE
    )
  }

  # Debiased estimates, SEs, and marginal p-values
  estimate <- unname(backend$bhat[cols])
  se <- unname(backend$se[cols])
  p_value <- unname(backend$pval[cols])

  if (any(!is.finite(estimate)) ||
      any(!is.finite(se)) ||
      any(se <= 0)) {
    stop(
      "Debiased estimates or standard errors were non-finite; ",
      "check the design and nodewise regressions.",
      call. = FALSE
    )
  }


  # Obtain confidence intervals directly from hdi
  ci <- stats::confint(
    backend,
    level = conf.level
  )

  # Match confidence intervals to design-matrix columns
  ci <- ci[cols, , drop = FALSE]

  # Assemble debiased inference for ALL predictors
  infer_cols <- tibble::tibble(
    term = cols,
    estimate = unname(backend$bhat[cols]),
    ci_low = unname(ci[, "lower"]),
    ci_high = unname(ci[, "upper"]),
    p_value = unname(backend$pval[cols])
  )


  # Preserve the selector's terms and selection indicators
  selector_tab <- broom::tidy(
    object,
    scale_coef = TRUE
  )

  if (!setequal(
    selector_tab$term[
      selector_tab$term != "(Intercept)"
    ],
    cols
  ) || anyNA(selector_tab$selected)) {
    stop(
      "Selector term map does not match the full design.",
      call. = FALSE
    )
  }

  # Join ALL debiased predictor estimates
  # Nonselected coefficients are NOT replaced with NA
  results <- dplyr::left_join(
    selector_tab,
    infer_cols,
    by = "term"
  )

  # Intercept inference remains NA
  # Other coefficients retain their debiased inference

  as_inferrer(
    backend,
    name = "debiased",
    label = "De-sparsified Lasso Inference (hdi)",
    nonselection = "uncertain_nulls",
    inferences = results,
    conf.level = conf.level,
    selector = object,
    meta = list(
      family = family,
      backend = "hdi::lasso.proj",
      betainit = betainit,
      robust = robust,
      target = "full_model"
    )
  )
}
