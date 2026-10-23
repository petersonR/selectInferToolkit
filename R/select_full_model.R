#' Select a full model
#'
#' This function simply attempts a full model and returns a `selector` object
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

select_full_model <- function(
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

  X <- design_X(design, data)
  y <- design_y(design, data)
  fit <- fit_on_columns(X, y, colnames(X), family,
                        lm_for_gaussian = FALSE)

  # aliased columns (e.g. a factor level absent from a resample) have NA
  # coefficients: they were not estimated, so they are not selected
  selected_coefs <- drop_aliased(coef_on_columns(fit))

  as_selector(fit, name = "full_model", label = "Full model (no selection)",
              design = design,
              orig_formula = formula,
              selected_terms = names(selected_coefs),
              selected_coefs = selected_coefs,
              default_infer = "upsi",
              meta = list(family = family))
}
