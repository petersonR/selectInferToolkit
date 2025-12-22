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
#
# select_stepwise_ic <- function(
#     formula, data, family = c("gaussian", "binomial"),
#     select_factors_together = TRUE,
#     penalty = c("AIC", "BIC"),
#     direction = c("forward", "backward", "both"),
#     trace = 0,
#     fitted_selector = NULL,
#     ...) {
#
#
#
#   # If this has never been fit before, check args
#   if(is.null(fitted_selector)) {
#     family = match.arg(family)
#     penalty = match.arg(penalty)
#     direction = match.arg(direction)
#     if(missing(formula))
#       stop("Must supply formula")
#
#   } else {
#     meta <- attr(fitted_selector, "meta")
#     family <- meta$family
#     penalty <- meta$penalty
#     direction <- meta$direction
#
#     if(missing(formula))
#       formula <- attr(fitted_selector, "recipe_obj")
#   }
#
#   k_val <- if(penalty == "AIC") 2 else log(nrow(data))
#
#   # Initial pre-processing
#   rec_obj <- formula
#
#   # If a typical formula is supplied, will center/scale
#   if(!inherits(rec_obj, "recipe")) {
#
#
#     rec_obj <- recipe(formula, data = data) %>%
#       step_zv(all_predictors()) %>%
#       step_center(all_numeric_predictors()) %>%
#       step_scale(all_numeric_predictors())
#
#     # Dummy coding ONLY if we want per-level selection
#     if (!select_factors_together) {
#       rec_obj <- rec_obj %>%
#         step_dummy(all_factor_predictors(),
#                    naming = function(...) dummy_names(..., sep = ""))
#     }
#
#     # add additional zero variance step
#     rec_obj <- rec_obj %>%
#       step_zv() %>%
#       prep()
#
#     df  <- bake(rec_obj, new_data = data)
#     y1 <- names(bake(rec_obj, data[1,, drop = FALSE], all_outcomes()))
#     x1 <- names(bake(rec_obj, data[1,, drop = FALSE], all_predictors()))
#
#     x_terms <- ifelse(
#       grepl("[^a-zA-Z0-9._]", x1),
#       paste0("`", x1, "`"),
#       x1
#     )
#
#     formula_full <- as.formula(
#       paste0(y1, " ~ ", paste(x_terms, collapse = " + "))
#     )
#
#
#     attr(rec_obj, "prepped_selector_recipe") <- TRUE
#     attr(rec_obj, "formula_full") <- formula_full
#
#   } else if(!attr(rec_obj, "prepped_selector_recipe")) {
#     stop("custom recipes not yet supported")
#   } else {
#     # May need to read additional things here if this is re-called later
#     all_terms <- attr(fitted_selector, "all_terms")
#     formula_full <- attr(rec_obj, "formula_full")
#     y1 <- names(bake(rec_obj, data[1,, drop = FALSE], all_outcomes()))
#     df  <- bake(rec_obj, new_data = data)
#
#   }
#
#
#   # formulae for use with MASS::stepAIC
#   formula_null <- as.formula(paste0(y1, " ~ 1"))
#
#   if(direction == "backward") {
#     formula_start <- formula_full
#   } else {
#     formula_start <- formula_null
#   }
#
#   model_start <- glm(formula_start, data = df, family = family,
#                      x = TRUE, y = TRUE, model = TRUE)
#
#   # crucial for reselection operation
#   model_start$call$data <- df
#
#   selected_model <- MASS::stepAIC(
#     model_start,
#     scope = list(lower = formula_null, upper = formula_full),
#     direction = direction,
#     k = k_val,
#     trace = trace,
#     ...
#   )
#
#   mm <- model.matrix(formula_full, df)
#   all_terms <- setdiff(colnames(mm), "(Intercept)")
#   selected_coefs <- coef(selected_model)
#   selected_terms <- attr(terms(selected_model), "term.labels")
#
#   meta_information <- list(
#     family = family,
#     select_factors_together = select_factors_together,
#     penalty = penalty,
#     direction = direction
#   )
#
#   out <- as_selector(selected_model, name = "stepwise_ic", label = "Stepwise IC-based",
#               all_terms = all_terms, recipe_obj = rec_obj, default_infer = "boot",
#               formula_full =formula_full,
#               selected_terms=selected_terms,
#               selected_coefs = selected_coefs, meta = meta_information)
#
#
# }



select_stepwise_ic <- function(
    formula, data,
    family = c("gaussian", "binomial"),
    select_factors_together = TRUE,
    penalty = c("AIC", "BIC"),
    direction = c("forward", "backward", "both"),
    trace = 0,
    fitted_selector = NULL,
    ...
) {


  if (is.null(fitted_selector)) {
    family    <- match.arg(family)
    penalty   <- match.arg(penalty)
    direction <- match.arg(direction)
    if (missing(formula))
      stop("Must supply formula")
  } else {
    meta      <- attr(fitted_selector, "meta")
    family    <- meta$family
    penalty   <- meta$penalty
    direction <- meta$direction
    formula   <- attr(fitted_selector, "orig_formula")
    select_factors_together <- meta$select_factors_together
  }

  k_val <- if (penalty == "AIC") 2 else log(nrow(data))

  # build recipe
  rec_spec <- recipe(formula, data = data) %>%
    step_zv(all_predictors()) %>%
    step_center(all_numeric_predictors()) %>%
    step_scale(all_numeric_predictors())

  if (!select_factors_together) {
    rec_spec <- rec_spec %>%
      step_dummy(all_factor_predictors(),
                 naming = function(...) dummy_names(..., sep = ""))
  }

  ## prep on current data (important for reffiting)
  rec_obj <- prep(rec_spec, training = data)
  df <- bake(rec_obj, new_data = data)

  y_name <- names(bake(rec_obj, data[1, , drop = FALSE], all_outcomes()))
  x_names <- names(bake(rec_obj, data[1, , drop = FALSE], all_predictors()))

  # term name cleaning
  x_terms <- ifelse(grepl("[^a-zA-Z0-9._]", x_names),paste0("`", x_names, "`"),
    x_names)

  # collect all terms
  formula_full <- as.formula( paste0(y_name, " ~ ", paste(x_terms, collapse = " + ")))
  formula_null <- as.formula(paste0(y_name, " ~ 1"))
  formula_start <- if (direction == "backward") formula_full else formula_null

  model_start <- glm( formula_start, data = df, family = family, x = TRUE,
    y = TRUE, model = TRUE )

  model_start$call$data <- df

  selected_model <- MASS::stepAIC(
    model_start,
    scope = list(lower = formula_null, upper = formula_full),
    direction = direction,
    k = k_val,
    trace = trace,
    ...
  )

  ## bookkeeping
  mm <- model.matrix(formula_full, df)
  all_terms <- setdiff(colnames(mm), "(Intercept)")
  selected_terms <- attr(terms(selected_model), "term.labels")
  selected_coefs <- coef(selected_model)

  meta_information <- list(
    family = family,
    select_factors_together = select_factors_together,
    penalty = penalty,
    direction = direction
  )

  orig_formula =formula

  ## output
  as_selector(
    selected_model,
    name = "stepwise_ic",
    label = "Stepwise IC-based",
    all_terms = all_terms,
    recipe_obj = rec_obj,
    default_infer = "boot",
    #formula_full = formula_full,
    orig_formula = formula,
    selected_terms = selected_terms,
    selected_coefs = selected_coefs,
    meta = meta_information
  )
}

