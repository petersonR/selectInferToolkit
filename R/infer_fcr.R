#' False coverage-statement rate (FCR) adjusted confidence intervals
#'
#' Constructs confidence intervals for selected *full-model* coefficients
#' using the Benjamini--Yekutieli selection-count adjustment. The selector
#' and the full-model fit use the same observations.
#'
#' @param object A fitted `selector`.
#' @param data The data on which `object` was fitted, with the same rows.
#' @param q Target false coverage-statement rate, strictly between 0 and 1.
#' @param dependence `"standard"` uses q * R / p; `"arbitrary"` uses
#'   q * R / (p * sum(1 / seq_len(p))). The standard adjustment requires
#'   assumptions on selection/dependence; the arbitrary option is conservative.
#' @param nonselection How to report nonselected coefficients: `"ignored"`
#'   (NA), `"confident_nulls"` (zero/degenerate interval), or
#'   `"uncertain_nulls"` (unadjusted full-model marginal intervals).
#'   Only the selected coefficient intervals belong to the FCR-adjusted family.
#' @return An `inferrer`, compatible with `tidy.inferrer()`.
#' @details Requires an estimable full model with positive residual degrees
#'   of freedom. Counts model-matrix coefficient columns (not formula terms),
#'   excluding the intercept. The intercept receives an ordinary marginal
#'   interval at level 1-q when R > 0, and no reported interval when R = 0.
#'   The `p_value` column contains unadjusted full-model p-values.
#'   Like other package methods, inference is on the standardized design scale
#'   by default; the shared intercept back-transformation issue when
#'   `scale_coef = FALSE` is not addressed here.Dependence = "standard" uses the conventional
#'   BY FCR adjustment. dependence = "arbitrary" applies the harmonic correction from
#'   Theorem 4 of Benjamini and Yekutieli (2005), providing a more conservative adjustment
#'    for arbitrary dependence among the parameter estimators, assuming valid monotone marginal confidence intervals.
#' @references Benjamini Y, Yekutieli D (2005). JASA 100(469):71--81.
#'   doi:10.1198/016214504000001907.
#' @export
infer_fcr <- function(object, data, q = 0.05,
                      dependence = c("standard", "arbitrary"),
                      nonselection = c("ignored", "confident_nulls",
                                       "uncertain_nulls")) {
  if (!inherits(object, "selector"))
    stop("`object` must be a `selector`.", call. = FALSE)
  if (missing(data))
    stop("`data` must be supplied.", call. = FALSE)
  if (!is.numeric(q) || length(q) != 1L || !is.finite(q) ||
      q <= 0 || q >= 1)
    stop("`q` must be one finite number strictly between 0 and 1.", call. = FALSE)

  dependence <- match.arg(dependence)
  nonselection <- match.arg(nonselection)
  design <- attr(object, "design")
  family <- attr(object, "meta")$family
  if (!family %in% c("gaussian", "binomial", "poisson"))
    stop("Unsupported family for `infer_fcr()`.", call. = FALSE)

  X <- design_X(design, data)
  y <- design_y(design, data)
  map <- design$map
  # A missing/constant design column would not have an estimable full-model
  # coefficient, so the target family cannot be constructed as requested.
  if (any(!map$kept))
    stop("The full model has zero-variance design columns; ",
         "FCR full-model inference requires all candidate columns estimable.",
         call. = FALSE)

  p <- nrow(map)
  if (p < 1L) stop("The model has no candidate coefficients.", call. = FALSE)
  if (nrow(X) <= p + 1L)
    stop("Full-model FCR inference requires n > p + 1 ",
         "(including the intercept), to have residual degrees of freedom.",
         call. = FALSE)

  fit_full <- fit_on_columns(X, y, colnames(X), family)
  if (fit_full$rank < (p + 1L) || stats::df.residual(fit_full) <= 0)
    stop("The full-model design is rank deficient or has no residual ",
         "degrees of freedom.", call. = FALSE)
  if (anyNA(stats::coef(fit_full)))
    stop("Some full-model coefficients are not estimable.", call. = FALSE)
  if (family == "binomial" && !isTRUE(fit_full$converged))
    stop("Full-model binomial GLM failed to converge.", call. = FALSE)
  if (family == "poisson" && !isTRUE(fit_full$converged))
    stop("Full-model Poisson GLM failed to converge.", call. = FALSE)

  selector_tab <- broom::tidy(object, scale_coef = TRUE)
  candidates <- selector_tab[selector_tab$term != "(Intercept)", , drop = FALSE]
  if (nrow(candidates) != p ||
      !setequal(candidates$term, map$column) ||
      anyNA(candidates$selected))
    stop("Selector terms do not match the full-model design columns.",
         call. = FALSE)
  selected <- candidates$term[candidates$selected == 1]
  R <- length(selected)
  harmonic <- if (dependence == "arbitrary") sum(1 / seq_len(p)) else 1
  alpha_used <- if (R) q * R / (p * harmonic) else NA_real_
  adjusted_level <- if (R) 1 - alpha_used else NA_real_

  # Tidy the full fit at the ordinary level for optional nonselections
  # and the intercept. Do not reuse UPSI or selected-model refitting.
  base <- tidy_on_columns(fit_full, 1 - q)
  if (R) {
    adjusted <- tidy_on_columns(fit_full, adjusted_level)
    idx <- match(selected, base$term)
    adj_idx <- match(selected, adjusted$term)
    if (anyNA(idx) || anyNA(adj_idx))
      stop("Selected coefficients missing from the full-model fit.", call. = FALSE)
    base$ci_low[idx] <- adjusted$ci_low[adj_idx]
    base$ci_high[idx] <- adjusted$ci_high[adj_idx]
  }

  results <- selector_tab
  result_idx <- match(results$term, base$term)
  if (anyNA(result_idx))
    stop("Full-model coefficients do not match selector terms.", call. = FALSE)
  results$estimate <- base$estimate[result_idx]
  results$ci_low <- base$ci_low[result_idx]
  results$ci_high <- base$ci_high[result_idx]
  results$p_value <- base$p_value[result_idx]

  nonselected <- results$selected == 0 & results$term != "(Intercept)"
  if (nonselection == "ignored") {
    results[nonselected, c("estimate", "ci_low", "ci_high", "p_value")] <- NA_real_
  } else if (nonselection == "confident_nulls") {
    results[nonselected, c("estimate", "ci_low", "ci_high")] <- 0
    results$p_value[nonselected] <- 1
  }
  # For uncertain_nulls, preserve full-model marginal results at level 1-q.
  # fill_in_nonselections() is deliberately NOT used: it refits a different
  # selected-plus-one model and therefore changes the estimand.
  if (R == 0L) {
    # No selected intervals are reported; the intercept is not in the
    # adjustment family, and no interval is returned for it in this case.
    is_intercept <- results$term == "(Intercept)"
    results[is_intercept, c("estimate", "ci_low", "ci_high", "p_value")] <- NA_real_
  }

  as_inferrer(
    fit_full,
    name = "fcr",
    label = "FCR-adjusted full-model intervals (BY)",
    nonselection = nonselection,
    inferences = results,
    conf.level = if (R) adjusted_level else 1 - q,
    selector = object,
    meta = list(q = q, p = p, R = R,
                adjusted_level = adjusted_level,
                dependence = dependence,
                full_model = TRUE)
  )
}
