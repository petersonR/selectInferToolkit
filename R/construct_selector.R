#' Constructor for `selector`s
#'
#' Create a `selector` wrapper around selection algorithms so they
#' can be used with the `selectInferToolkit` package. This is not a user-facing
#' function.
#'
#' @param x an object from a selection algorithm
#' @param all_terms a slot containing names of all terms
#' @param meta a list containing important meta-information
#' @param selected_coefs a numeric vector of selected coefficient estimates
#'
#' @return An S3 object (list) of class `selector` containing:'
#'   x
#'   all_terms
#'   meta
#'
#' @rdname selector
#'
#' @export

as_selector <- function(x, name, label = name, all_terms, recipe_obj,
                        selected_coefs,
                        default_infer, meta = list()) {

  stopifnot(is.list(meta))
  stopifnot(is.character(all_terms))

  structure(x,
            class = c("selector", class(x)),
            name = name,
            label = label,
            all_terms = all_terms,
            recipe_obj = recipe_obj,
            selected_coefs = selected_coefs,
            default_infer = default_infer,
            meta = meta)
}

#' predict method for `selector` object
#'
#' Returns predictions for `selector` objects on new data
#'
#' @rdname selector
#' @export
predict.selector <- function(object, newdata = NULL, ...) {

  if(missing(newdata))
    stop("Please provide newdata")

  rec_obj <- attr(object, "recipe_obj")

  newdata <- bake(rec_obj, newdata) %>%
    as.data.frame()

  beta <- attr(object, "selected_coefs")
  XX <- cbind(1, as.matrix(newdata[,names(beta)[-1]]))
  return(as.vector(XX %*% beta))
}

#' tidy method for `selector` object
#'
#' Returns a tibble with all candidate variables, estimates (scaled & unscaled)
#' @param x a `selector`
#' @param scale_coef should scaled betas be returned, or unscaled?
#' @param ... additional parameters (not yet used)
#'
#' @importFrom tibble rownames_to_column
#' @importFrom broom tidy
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#'
#' @rdname selector
#'
#' @export
tidy.selector <- function(x, scale_coef = TRUE, ...) {

  selected_coefs <- tibble::rownames_to_column(
    data.frame(estimate = coef(x), selected = 1), "term")

  all_terms <- attr(x, "all_terms")
  rec_obj <- attr(x, "recipe_obj")
  scale_step_idx <- which(tidy(rec_obj)$type == "scale")

  if(length(scale_step_idx)) {
    sds <- tidy(rec_obj, number = scale_step_idx) %>%
      select(term = terms, sd = value)
  } else {
    stop("a scaling step is required")
  }


  results <- tibble(term = all_terms) %>%
    left_join(selected_coefs, by = "term") %>%
    mutate(selected = ifelse(is.na(selected), 0, selected)) %>%
    left_join(sds, by = "term") %>%
    mutate(coef_scaled = estimate,
           coef_unscaled = ifelse(term != "(Intercept)", coef_scaled/sd, estimate) ) %>%
    select(term, selected, estimate, coef_scaled, coef_unscaled)

  if(scale_coef) {
    results <- results %>%
      select(term, selected, estimate = coef_scaled)
  }  else {
    results <- results %>%
      select(term, selected, estimate = coef_unscaled)
  }

  results
}

#'
#' @rdname selector
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

#' coef method for `selector` object
#'
#' Returns a vector with all selected coefficients (scaled only, includes intercept)
#' @rdname selector
#' @export
coef.selector <- function(x) {
  attr(x, "selected_coefs")
}


#' A re-selection function
#'
#' Re-do the selection, possibly on new data. Does not re-do the pre-processing;
#' instead uses the original preprocess recipe from `selector_obj`.
#'
#' @rdname selector
#'
#' @export
#'
reselect <- function(selector_obj, newdata) {
  stopifnot(inherits(selector_obj, "selector"))

  original_fn <- paste0("select_", attr(selector_obj, "name"))
  do.call(original_fn, args = list(data = newdata, fitted_selector = selector_obj))
}
