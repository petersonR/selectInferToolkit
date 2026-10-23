#' Constructor for `selector`s
#'
#' Create a `selector` wrapper around a selection algorithm so it can be used
#' with the `selectInferToolkit` package. Users don't call this directly; the
#' `select_*` functions do. It is exported for writing new selectors.
#'
#' @param x an object from a selection algorithm
#' @param name name of the selector
#' @param label label of the selector (for pretty printing)
#' @param design the selector's design (internal): how the formula and data
#'   become the standardized predictor matrix, and the term map behind
#'   `tidy()`
#' @param orig_formula Original formula provided by user
#' @param selected_terms names of selected variables
#' @param selected_coefs a vector of only selected coefficients
#' @param default_infer the root string of the default infer method
#' @param meta a list containing important meta-information
#'
#' @return `x` with class `selector` prepended and the other arguments
#'   stored as attributes.
#'
#' @keywords internal
#' @export

as_selector <- function(x, name, label = name, design, orig_formula,
                        selected_terms, selected_coefs,
                        default_infer, meta = list()) {

  stopifnot(is.list(meta))
  stopifnot(inherits(design, "selector_design"))

  structure(x,
            class = c("selector", class(x)),
            name = name,
            label = label,
            all_terms = design_terms(design),
            design = design,
            orig_formula=  orig_formula,
            selected_terms=selected_terms,
            selected_coefs = selected_coefs,
            default_infer = default_infer,
            meta = meta)
}

#' Methods for `selector` objects
#'
#' @description
#' The objects returned by the `select_*` functions support:
#'
#' * `predict()`: predictions on new data, using the selected model.
#' * `tidy()`: a tibble with one row per candidate term.
#' * `coef()`: the selected coefficients (including the intercept).
#' * `print()`: a short summary of the selection.
#'
#' @section Scale of the coefficients:
#' Before selection, every `select_*` function builds the model matrix as
#' [model.matrix()] would (factors, logicals and character predictors are
#' treatment-coded, so the columns are named e.g. `g3mid`, `am1`, `fTRUE`),
#' then centers each column and divides it by its standard deviation, dummies
#' included. The outcome is not transformed. So, by default
#' (`scale_coef = TRUE`), a coefficient is the change in the outcome per
#' standard deviation of that column. With `scale_coef = FALSE`, slopes are
#' divided by the column's standard deviation, giving the usual per-unit
#' change. The centers and standard deviations come from the data the
#' selector was first fit to, and are reused by [reselect()] and `predict()`.
#'
#' The intercept is never unscaled: it is the predicted outcome (on the link
#' scale) at the means of the predictors, not the intercept `lm()` would
#' report on the raw data. ([select_stepwise_ic()] leaves dummies uncentered,
#' so there it is the prediction at each factor's reference level.)
#'
#' Ordered factors are not supported (convert them to numeric scores or
#' unordered factors first), nor are missing values in the outcome or
#' predictors.
#'
#' @return
#' * `tidy()`: a tibble with columns `term` (every candidate term, plus the
#'   intercept), `selected` (1 if the selector kept the term, else 0; the
#'   intercept is always 1) and `coef` (the selector's estimate, e.g. the
#'   penalized estimate for [select_glmnet()] or [select_ncvreg()]; 0 when not
#'   selected).
#' * `coef()`: a named vector of the selected coefficients, including the
#'   intercept, always on the scaled metric.
#' * `predict()`: a vector of predictions for `newdata`.
#'
#' @param object,x a `selector`
#' @param newdata a data frame to predict on
#' @param ... for `coef()`, passed to the native `coef` method when
#'   `use_native = TRUE`; otherwise not used
#'
#' @rdname selector-methods
#' @export

predict.selector <- function(object, newdata, ...) {

  if(missing(newdata))
    stop("Please provide newdata")

  beta <- coef(object)
  X <- design_X(attr(object, "design"), newdata, allow_na = TRUE)
  XX <- cbind(1, X[, names(beta)[-1], drop = FALSE])
  as.vector(XX %*% beta)
}




#' @param scale_coef if `TRUE` (default), coefficients are per standard
#'   deviation of each predictor; if `FALSE`, per unit. See "Scale of the
#'   coefficients".
#'
#' @importFrom tibble rownames_to_column
#' @importFrom broom tidy
#' @importFrom dplyr left_join filter select mutate if_else
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @rdname selector-methods
#'
#' @export
tidy.selector <- function(x, scale_coef = TRUE, ...) {

  design <- attr(x, "design")
  beta <- coef(x)
  terms <- design_terms(design)

  coef <- unname(beta[terms])
  if (!scale_coef) {
    # per-unit slopes; the intercept stays as is (see "Scale of the
    # coefficients")
    sds <- design_sds(design)[terms]
    slope <- terms != "(Intercept)"
    coef[slope] <- coef[slope] / sds[slope]
  }

  tibble(
    term = terms,
    selected = as.numeric(terms %in% names(beta)),
    coef = ifelse(is.na(coef), 0, coef)
  )
}



#' @rdname selector-methods
#' @export
print.selector <- function(x, ...) {

  cat(attr(x, "label"), "selector\n")

  meta <- attr(x, "meta")
  beta <- attr(x, "selected_coefs")[-1]

  beta_names <- names(beta)[order(abs(beta))]
  pretty_names <- if(length(beta_names) > 7) paste0(c(beta_names[1:7], "..."), collapse = ", ") else paste0(beta_names, collapse = ", ")

  n_selections <- length(beta)
  p <- length(attr(x, "all_terms")) - 1

  cat(n_selections, "of", p, "variables selected:", pretty_names, "\n")

  cat("Meta information:", format_meta(meta), "\n")

  cat("Default `infer()` method:", attr(x, "default_infer"))

  # NextMethod()  # prints with the original class' print() method
}

#' @param use_native if `TRUE`, call the underlying fit's own `coef` method
#'   (e.g. `coef.cv.glmnet`) instead
#'
#' @rdname selector-methods
#'
#' @export
coef.selector <- function(object, use_native = FALSE, ...) {
  if(use_native)
    return(NextMethod())
  attr(object, "selected_coefs")
}


#' Re-run a selection on new data
#'
#' Re-does the selection, possibly on new data, with the same settings as the
#' original. The pre-processing is not re-estimated: the design of
#' `selector_obj` (its columns, factor levels, centers and scales) is applied
#' to `newdata`. This is what [infer_boot()] calls on each resample.
#'
#' @param selector_obj a `selector`
#' @param newdata a data frame (or the same one)
#'
#' @return a new `selector`
#'
#' @export
#'
reselect <- function(selector_obj, newdata) {
  stopifnot(inherits(selector_obj, "selector"))

  original_fn <- paste0("select_", attr(selector_obj, "name"))
  do.call(original_fn, args = list(data = newdata, fitted_selector = selector_obj))
}
