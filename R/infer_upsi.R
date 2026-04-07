#' Unadjusted post-selection inference (UPSI)
#'
#' UPSI is sometimes referred to as "hybrid-OLS", but essentially we
#' re-fit the selected model to the data as though we never used that
#' same data to fit the model. It is common, easy, and ill-advised.
#'
#' @param object a `selector` object
#' @param data data must be passed to `infer` methods
#' @param nonselection  A character string specifying how to handle variables
#'   not selected by model selection procedure. One of "ignored",
#'   "confident_nulls" or "uncertain_nulls" supported
#' @param conf.level .95 by default
#'
#' @return `inferrer` object
#'
#' @importFrom dplyr right_join group_by summarize bind_rows pull filter if_else
#' @importFrom rlang .data
#'
#' @rdname infer
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
    pull(.data$variable)
  all_terms <- attr(object, "all_terms")

  df <- bake(rec_obj, new_data = data)

  selected_formula <- formula(paste0(outcome_name, "~", paste0(c(1, selected_vars), collapse = "+")))
  fit_selected <- glm(selected_formula, data = df, family = meta$family)
  results_selected <- tidy(fit_selected, conf.int = TRUE, conf.level = conf.level) %>%
    select(.data$term, .data$estimate, ci_low = .data$conf.low,
           ci_high = .data$conf.high, p_value = .data$p.value)
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
