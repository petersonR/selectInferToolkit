#' Unadjusted post-selection inference (UPSI)
#'
#' UPSI is sometimes referred to as "hybrid-OLS", but essentially we
#' re-fit the selected model to the data as though we never used that
#' same data to fit the model. It is common, easy, and ill-advised.
#'
#' @param object a `selector` object
#' @param data the data used to fit the selector
#' @param nonselection how to handle variables the selector did not select:
#'
#'   * `"ignored"` (default): no inference; their `estimate`, `ci_low`,
#'     `ci_high` and `p_value` are `NA`.
#'   * `"confident_nulls"`: treated as exactly zero, with `estimate = 0`, a
#'     degenerate interval `[0, 0]` and `p_value = 1`.
#'   * `"uncertain_nulls"`: each non-selected variable is added, one at a time,
#'     to the selected model, which is refit on `data`; its estimate, interval
#'     and p-value come from that fit. These are not adjusted for selection.
#'
#'   Partial names are matched (e.g. `"uncertain"`).
#' @param conf.level .95 by default
#'
#' @return an `inferrer` object; see [tidy.inferrer()]
#'
#' @importFrom dplyr right_join group_by summarize bind_rows pull filter if_else
#' @importFrom rlang .data
#'
#' @export
#'

infer_upsi <- function(
    object,
    data,
    nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
    conf.level = .95){

  if (!inherits(object, "selector"))
    stop("`object` must be a `selector`. ",
         "Did you pass an `inferrer` by mistake? Use a select_* function first.")

  nonselection <- match.arg(nonselection)

  design <- attr(object, "design")
  meta <- attr(object, "meta")
  X <- design_X(design, data)
  y <- design_y(design, data)

  selected_cols <- setdiff(names(coef(object)), "(Intercept)")
  fit_selected <- fit_on_columns(X, y, selected_cols, meta$family)
  results_selected <- tidy_on_columns(fit_selected, conf.level) %>%
    select("term", "estimate", "ci_low", "ci_high", "p_value")

  results <- tidy(object, scale_coef = TRUE) %>%
    left_join(results_selected, by = "term")

  if(nonselection == "confident_nulls") {
    results <- results %>%
      mutate(estimate = ifelse(is.na(.data$estimate), 0, .data$estimate),
             ci_low = ifelse(is.na(.data$ci_low), 0, .data$ci_low),
             ci_high = ifelse(is.na(.data$ci_high), 0, .data$ci_high),
             p_value = ifelse(is.na(.data$p_value), 1, .data$p_value)
      )
  }
  if(nonselection == "uncertain_nulls") {
    results <- fill_in_nonselections(results, object, nonselection,
                                     X = X, y = y, conf.level = conf.level)
  }

  as_inferrer(
    fit_selected,
    name = "upsi",
    label = "Unadjusted Post-Selection Inference (UPSI)",
    nonselection = nonselection,
    inferences = results,
    conf.level = conf.level,
    selector = object,
    meta = list()
  )
}
