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

  if( attr(object, "name") == "stepwise_ic"){
    selected_vars <-   attr(object, "selected_terms")
    selected_vars <- selected_vars [! selected_vars  %in% c("(Intercept)")]
  } else{selected_vars <- names(coef(object)[-1])}

  rec_obj <- attr(object, "recipe_obj")
  meta <- attr(object, "meta")
  outcome_name <- rec_obj$var_info %>%
    filter(.data$role == "outcome") %>%
    pull("variable")
  all_terms <- attr(object, "all_terms")

  df <- bake(rec_obj, new_data = data)

  selected_formula <- formula(paste0(outcome_name, "~", paste0(c(1, selected_vars), collapse = "+")))
  fit_selected <- fit_lm_or_glm(selected_formula, df, meta$family)
  results_selected <- tidy(fit_selected, conf.int = TRUE, conf.level = conf.level) %>%
    select("term", "estimate", ci_low = "conf.low",
           ci_high = "conf.high", p_value = "p.value")
  results_selected$term <- make.names(results_selected$term )
  if(results_selected$term[1] =="X.Intercept.") results_selected$term[1] = "(Intercept)"


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

    rec_obj <- attr(object, "recipe_obj")

    if( attr(object, "name") == "stepwise_ic"){
       if( attr(object, "meta")$select_factors_together ){
      rec_obj <- rec_obj  %>%
        step_dummy(all_factor_predictors(),
                   naming = function(...) dummy_names(..., sep = "")) %>%
        prep()
        }
    }

    X <- bake(rec_obj, new_data = data, all_predictors())
    y <- bake(rec_obj, new_data = data, all_outcomes())[[1]]


    term_to_col <- tibble(
      term = results$term,
      col  = colnames(X)[
        match(make.names(results$term),
              make.names(colnames(X)))]
       )%>%filter(!is.na(col))

    results <- fill_in_nonselections(results, object, nonselection,
                                     X = X, y = y, conf.level = conf.level,
                                     term_to_col = term_to_col     )

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
