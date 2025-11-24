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
#' @importFrom dplyr right_join group_by summarize bind_rows pull
#'
#' @rdname infer
#' @export
#'
infer_upsi <- function(
    object,
    data,
    nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
    conf.level = .95){

  nonselection <- match.arg(nonselection)

  selected_vars <- names(coef(object)[-1])
  rec_obj <- attr(object, "recipe_obj")
  meta <- attr(object, "meta")
  outcome_name <- rec_obj$var_info %>%
    filter(role == "outcome") %>%
    pull(variable)
  all_terms <- attr(object, "all_terms")

  df <- bake(rec_obj, new_data = data)

  selected_formula <- formula(paste0(outcome_name, "~", paste0(c(1, selected_vars), collapse = "+")))
  fit_selected <- glm(selected_formula, data = df, family = meta$family)
  results_selected <- tidy(fit_selected, conf.int = TRUE, conf.level = conf.level) %>%
    select(term, estimate, ci_low = conf.low, ci_high = conf.high, p_value = p.value)

  results <- tidy(object, scale_coef = TRUE) %>%
    left_join(results_selected, by = "term")


  if(nonselection == "confident_nulls") {
    results <- results %>%
      mutate(estimate = ifelse(is.na(estimate), 0, estimate),
             ci_low = ifelse(is.na(ci_low), 0, ci_low),
             ci_high = ifelse(is.na(ci_high), 0, ci_high),
             p_value = ifelse(is.na(p_value), 1, p_value)
      )
  }
  if(nonselection == "uncertain_nulls") {

    X <- bake(attr(object, "recipe_obj"), new_data = data, all_predictors())
    y <- bake(attr(object, "recipe_obj"), new_data = data, all_outcomes())[[1]]

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

