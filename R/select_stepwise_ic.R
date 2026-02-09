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
    step_center(all_numeric_predictors())

  if (!select_factors_together) {
    rec_spec <- rec_spec %>%
      step_dummy(all_factor_predictors(),
                 naming = function(...) dummy_names(..., sep = ""))
  }

  rec_spec <-  rec_spec  %>% step_scale(all_numeric_predictors())


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
  mm <- model.matrix(formula, data = data)
  all_terms <- make.names(colnames(mm))
  if(all_terms[1] =="X.Intercept.") all_terms[1] = "(Intercept)"

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
