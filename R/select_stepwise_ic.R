#' Stepwise forward/backward/bidirectional selection with AIC/BIC
#'
#'
#' @description This function implements forward/backward/bidirectional stepwise regression,
#' for use in the `selectInferToolkit` package
#'
#' @param formula a formula.
#' @param data data set
#' @param family outcome distributional family
#' @param penalty AIC, BIC
#' @param select_factors_together should categorical variables be jointly selected?
#' @param direction the mode of step wise search, can be one of "both", "backward", or "forward", with a default of "forward"
#' @param ... Additional arguments
#' @param trace passed to MASS::stepAIC
#' @param fitted_selector a previously fit `selector`, used for resampling
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#' @importFrom stats lm  model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @import recipes
#' @return A  `selector` object
#' @export

select_stepwise_ic <- function(
    formula, data, family = c("gaussian", "binomial"),
    select_factors_together = TRUE,
    penalty = c("AIC", "BIC"),
    direction = c("forward", "backward", "both"),
    trace = 0,
    fitted_selector = NULL,
    ...) {



  # If this has never been fit before, check args
  if(is.null(fitted_selector)) {
    family = match.arg(family)
    penalty = match.arg(penalty)
    direction = match.arg(direction)
    if(missing(formula))
      stop("Must supply formula")

  } else {
    meta <- attr(fitted_selector, "meta")
    family <- meta$family
    penalty <- meta$penalty
    direction <- meta$direction

    if(missing(formula))
      formula <- attr(fitted_selector, "recipe_obj")
  }

  k_val <- if(penalty == "AIC") 2 else log(nrow(data))

  # Initial pre-processing
  rec_obj <- formula

  # If a typical formula is supplied, will center/scale
  if(!inherits(rec_obj, "recipe")) {

    # if(select_factors_together ==T) {
    #   rec_obj <- recipe(formula, data = data)
    # }else{
    #   rec_obj <- recipe(formula, data = data) %>%
    #     step_dummy(all_factor_predictors(),
    #                naming = function(...) dummy_names(..., sep = ""))
    # }

    rec_obj <- recipe(formula, data = data) %>%
      step_dummy(all_factor_predictors(),
                 naming = function(...) dummy_names(..., sep = "")) %>%
      step_zv(all_predictors()) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) %>%
      prep()

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
    all_terms <- attr(fitted_selector, "all_terms")
    formula_full <- attr(rec_obj, "formula_full")
  }

  # Create model matrix
  X <- bake(rec_obj, new_data = data, all_predictors())
  y <- bake(rec_obj, new_data = data, all_outcomes())
  df_for_fit <- bake(rec_obj, new_data = data)

  # formulae for use with MASS::stepAIC
  formula_null <- update(formula_full, . ~ 1)
  if(direction == "backward") {
    formula_start <- formula_full
  } else {
    formula_start <- formula_null
  }

  model_start <- glm(formula_start, data = df_for_fit, family = family,
                     x = TRUE, y = TRUE, model = TRUE)

  # crucial for reselection operation
  model_start$call$data <- df_for_fit

  selected_model <- MASS::stepAIC(
    model_start,
    scope = list(lower = formula_null, upper = formula_full),
    direction = direction,
    k = k_val,
    trace = trace
  )

  meta_information <- list(
    family = family,
    select_factors_together = select_factors_together,
    penalty = penalty,
    direction = direction
  )

  selected_coefs <- coef(selected_model)

  as_selector(selected_model, name = "stepwise_ic", label = "Stepwise IC-based",
              all_terms = all_terms, recipe_obj = rec_obj, default_infer = "boot",
              selected_coefs = selected_coefs, meta = meta_information)
}
