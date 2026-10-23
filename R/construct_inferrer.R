#' Constructor for `inferrer`s
#'
#' Create an `inferrer` wrapper around a post-selection inference method so
#' its results can be harmonized across selection algorithms. Users don't call
#' this directly; the `infer_*` functions do. It is exported for writing new
#' inference methods.
#'
#' @param x a slot for the main inferential object
#' @param name brief name for method
#' @param label label for method
#' @param nonselection how non-selections were/are handled
#' @param inferences a slot for post-selection inferences that should
#'   have confidence intervals (at least)
#' @param conf.level the confidence level used for intervals (1-`sig.level`)
#' @param meta a list containing important meta-information from the method
#' @param selector a carried-forward `selector` object
#'
#' @return `x` with class `inferrer` prepended and the other arguments
#'   stored as attributes.
#'
#' @keywords internal
#' @export

as_inferrer <- function(
    x,
    name,
    label = name,
    nonselection,
    inferences,
    conf.level,
    selector,
    meta = list()) {

  stopifnot(is.list(meta))

  structure(x,
            class = c("inferrer", class(x)),
            name = name,
            label = label,
            nonselection = nonselection,
            inferences = inferences,
            conf.level = conf.level,
            selector = selector,
            meta = meta)
}

#' Methods for `inferrer` objects
#'
#' The objects returned by the `infer_*` functions support `tidy()`, which
#' returns the inferences as a tibble with one row per candidate term, and
#' `print()`, which summarizes them.
#'
#' @section Scale of the coefficients:
#' Before selection, every `select_*` function standardizes the predictors
#' with a `recipes` recipe: factors are dummy-coded, then all numeric
#' predictors (dummies included) are centered and divided by their standard
#' deviation. The outcome is not transformed. So, by default
#' (`scale_coef = TRUE`), a coefficient is the change in the outcome per
#' standard deviation of that predictor. With `scale_coef = FALSE`, slopes are
#' divided by the predictor's standard deviation, giving the usual per-unit
#' change. (A factor kept whole by
#' `select_stepwise_ic(select_factors_together = TRUE)` is not scaled, so it
#' is the same either way.)
#'
#' The intercept is never unscaled: because the predictors are centered, it is
#' the predicted outcome (on the link scale) at the predictor means, not the
#' intercept `lm()` would report on the raw data.
#'
#' Intervals and `null_radius` are converted along with the estimates.
#'
#' @return `tidy()` returns a tibble with one row per candidate term (plus the
#'   intercept):
#'
#' * `term`, `selected`, `coef`: as in `tidy()` for the `selector` (see
#'   [tidy.selector()]). `coef` is the selector's own estimate.
#' * `estimate`: the inference method's estimate, which can differ from
#'   `coef`: a refit of the selected model for [infer_upsi()], the
#'   selective-inference estimate for [infer_selective()], and the mean over
#'   resamples for [infer_boot()].
#' * `ci_low`, `ci_high`: the confidence interval at the inferrer's
#'   `conf.level`.
#' * `p_value`: from [infer_upsi()], [infer_selective()] and [infer_pipe()].
#' * `prop_selected`: from [infer_boot()], the proportion of resamples in
#'   which the term was selected.
#' * `null_radius`: from [infer_boot()], the padding radius (see its `pad`
#'   argument).
#'
#' How non-selected terms are filled in depends on the method's
#' `nonselection` (see [infer_upsi()]) or, for [infer_boot()],
#' `inference_target`.
#'
#' `print()` returns, invisibly, a summary of the CI lengths.
#'
#' @param x an `inferrer`
#' @param scale_coef if `TRUE` (default), estimates are per standard deviation
#'   of each predictor; if `FALSE`, per unit. See "Scale of the coefficients".
#' @param ... additional parameters (not yet used)
#'
#' @importFrom tibble rownames_to_column
#' @importFrom broom tidy
#' @importFrom dplyr left_join transmute case_when
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @rdname inferrer-methods
#'
#' @export
tidy.inferrer <- function(x, scale_coef = TRUE, ...) {

  inferences <- attr(x, "inferences")
  selector_obj <- attr(x, "selector")
  results <- tidy(selector_obj, scale_coef = scale_coef)
  rec_obj <- attr(selector_obj, "recipe_obj")

  scale_step_idx <- which(tidy(rec_obj)$type == "scale")

  if(length(scale_step_idx)) {
    sds <- tidy(rec_obj, number = scale_step_idx) %>%
      select(term = "terms", sd = "value")
  } else {
    stop("a scaling step is required")
  }

  tidy_inferences <- inferences %>%
    left_join(sds, by = "term") %>%
    transmute(.data$term, .data$sd,
              estimate_scaled = .data$estimate,
              ci_low_scaled   = .data$ci_low,
              ci_high_scaled  =  .data$ci_high
    ) %>%
    mutate(
      estimate_unscaled = case_when(
        .data$term == "(Intercept)"  ~ estimate_scaled,
        is.na(.data$estimate_scaled) ~ NA_real_,
        !is.na(.data$sd)             ~ estimate_scaled / sd,
        # sd is NA whenever a term never went through the scale step, i.e. a
        # factor kept whole by select_factors_together. Unscaled == scaled there.
        is.na(.data$sd)              ~ estimate_scaled
      ),
      ci_low_unscaled = case_when(
        .data$term == "(Intercept)" ~ .data$ci_low_scaled,
        is.na(ci_low_scaled)  ~ NA_real_,
        !is.na(sd)           ~ .data$ci_low_scaled / .data$sd,
        # sd is NA whenever a term never went through the scale step, i.e. a
        # factor kept whole by select_factors_together. Unscaled == scaled there.
        is.na(sd)            ~ .data$ci_low_scaled
      ),
      ci_high_unscaled = case_when(
        term == "(Intercept)"  ~ .data$ci_high_scaled,
        is.na(ci_high_scaled)  ~ NA_real_,
        !is.na(sd)             ~ .data$ci_high_scaled / .data$sd,
        # sd is NA whenever a term never went through the scale step, i.e. a
        # factor kept whole by select_factors_together. Unscaled == scaled there.
        is.na(sd)              ~ .data$ci_high_scaled
      )
    )


  results <- results %>%
    left_join(tidy_inferences, by = c("term"))

  if(scale_coef) {
    results <- results %>%
      select("term", "selected", "coef", estimate = "estimate_scaled",
             ci_low = "ci_low_scaled", ci_high = "ci_high_scaled")
  }  else {
    results <- results %>%
      select("term", "selected", "coef", estimate = "estimate_unscaled",
             ci_low = "ci_low_unscaled", ci_high = "ci_high_unscaled")
  }

  # Check for other goodies, add if available
  to_add <- which(names(inferences) %in% c("p_value", "prop_selected", "null_radius"))
  if(length(to_add)) {
    results <- results %>%
      left_join(inferences[,c(1, to_add)], by = "term")
  }

  # the bootstrap's null-zone radius is on the scaled metric like the CIs
  if(!scale_coef && "null_radius" %in% names(results)) {
    sd_term <- sds$sd[match(results$term, sds$term)]
    results$null_radius <- ifelse(is.na(sd_term), results$null_radius,
                                  results$null_radius / sd_term)
  }


  results
}

#' @importFrom rlang .data
#' @rdname inferrer-methods
#' @export
print.inferrer <- function(x, ...) {

  selector_obj <- attr(x, "selector")

  cat(attr(x, "label"), "inference applied post",
      attr(selector_obj, "label"), "Selection \n")

  meta <- attr(x, "meta")

  beta <- coef(selector_obj)[-1]

  n_selections <- length(beta)
  p <- length(attr(selector_obj, "all_terms")) - 1

  cat(n_selections, "of", p, "variables selected.", "\n")

  cat("Nonselections were considered", attr(x, "nonselection"), "\n")

  cat("Meta information:", format_meta(meta), "\n")

  results <- tidy(x, scale_coef = TRUE)
  precision <- results %>%
    mutate(ci_ln =  .data$ci_high -  .data$ci_low) %>%
    summarize(med = median( .data$ci_ln, na.rm = TRUE),
              mean = mean( .data$ci_ln, na.rm = TRUE),
              iqr_low = quantile( .data$ci_ln, .25, na.rm = TRUE),
              iqr_high = quantile( .data$ci_ln, .75, na.rm = TRUE),
              max = max( .data$ci_ln, na.rm = TRUE))

  pretty_median <- round(precision$med, 1)
  pretty_mean <- round(precision$mean, 1)
  pretty_max <- round(precision$max, 1)


  cat("Median CI length = ", pretty_median,"; mean = ", pretty_mean, "; max = ",
      pretty_max, "\n", sep = "")

  invisible(precision)
}

