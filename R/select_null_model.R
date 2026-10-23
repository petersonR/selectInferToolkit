#' Select a null model
#'
#' This function simply attempts a null model and returns a `selector` object
#' as a useful comparison.
#'
#' @param formula a model formula, as for [lm()]. It may include
#'   interactions (`x * z`) and in-formula transformations (`log(x)`,
#'   `poly(x, 2)`); factor, logical and character predictors are
#'   treatment-coded. Ordered factors and missing values are not supported.
#'   See [tidy.selector()] for how the columns are standardized and named.
#' @param data a data frame
#' @param family outcome distributional family
#' @param ... Additional arguments
#' @param fitted_selector a previously fit `selector`, used by [reselect()]
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#'
#' @return A  `selector` object
#'
#' @export

select_null_model <- function(
    formula, data, family = c("gaussian", "binomial", "poisson"),
    fitted_selector = NULL,
    ...) {

  if(is.null(fitted_selector)) {
    family = match.arg(family)
    if(missing(formula))
      stop("Must supply formula")
    design <- design_build(formula, data)
  } else {
    family <- attr(fitted_selector, "meta")$family
    design <- attr(fitted_selector, "design")
    formula <- attr(fitted_selector, "orig_formula")
  }

  # the design still records every candidate column, for tidy()
  X <- design_X(design, data)
  y <- design_y(design, data)
  fit <- fit_on_columns(X, y, character(0), family,
                        lm_for_gaussian = FALSE)
  selected_coefs <- coef_on_columns(fit)

  as_selector(fit, name = "null_model", label = "Null model (no selection)",
              design = design,
              orig_formula = formula,
              selected_terms = names(selected_coefs),
              selected_coefs = selected_coefs,
              default_infer = "upsi",
              meta = list(family = family))
}
