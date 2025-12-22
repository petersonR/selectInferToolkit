#' Select a full model
#'
#' This function simply attempts a full model and returns a `selector` object
#' as a useful comparison.
#'
#' @param formula a formula.
#' @param data data set
#' @param family outcome distributional family
#' @param ... Additional arguments
#' @param fitted_selector a previously fit `selector`, used for resampling
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#'
#' @import recipes
#' @return A  `selector` object
#'
#' @export

select_full_model <- function(
    formula, data, family = c("gaussian", "binomial", "poisson"),
    fitted_selector = NULL,
    ...) {

  if(is.null(fitted_selector)) {
    family = match.arg(family)
    if(missing(formula))
      stop("Must supply formula")

  } else {
    meta <- attr(fitted_selector, "meta")
    family <- meta$family

    if(missing(formula))
      formula <- attr(fitted_selector, "recipe_obj")
  }

  # Initial pre-processing
  rec_obj <- formula

  # If a typical formula is supplied, will center/scale
  if(!inherits(rec_obj, "recipe")) {

    rec_obj <- recipe(formula, data = data) %>%
      step_dummy(all_factor_predictors(),
                 naming = function(...) dummy_names(..., sep = "")) %>%
      step_zv(all_predictors()) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) %>%
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
    attr(rec_obj, "formula_full") <- formula_full

  } else if(!attr(rec_obj, "prepped_selector_recipe")) {
    stop("custom recipes not yet supported")
  } else {
    # May need to read additional things here if this is re-called later
    rec_obj <- rec_obj %>%
      step_zv() %>%
      prep()

    y1 <- bake(rec_obj, new_data = data[1,,drop = FALSE], all_outcomes())
    X1 <- bake(rec_obj, new_data = data[1,,drop=FALSE], all_predictors())
    df1 <- bake(rec_obj, new_data = data[1,,drop = FALSE])

    # collect all terms
    formula_full <- as.formula(paste0(names(y1), " ~ ", paste0(names(X1), collapse = " + ")))
    all_terms <- colnames(model.matrix(formula_full, data = df1))

  }

  # Create model matrix
  X <- bake(rec_obj, new_data = data, all_predictors())
  y <- bake(rec_obj, new_data = data, all_outcomes())
  df_for_fit <- bake(rec_obj, new_data = data)


  fit <- glm(formula_full, data = df_for_fit, family = family,
             x = TRUE, y = TRUE, model = TRUE)


  meta_information <- list(family = family)
  selected_coefs <- coef(fit)
  selected_terms <- rownames(fit[["coefficients"]])

  as_selector(fit, name = "full_model", label = "Full model (no selection)",
              all_terms = all_terms, recipe_obj = rec_obj,
              orig_formula = formula,
              selected_terms=selected_terms,
              selected_coefs = selected_coefs,
              default_infer = "upsi",
              meta = meta_information)
}
