#' Constructor for `selector`s
#'
#' Create a `selector` wrapper around selection algorithms so they
#' can be used with the `selectInferToolkit` package. This is not a user-facing
#' function.
#'
#' @param x an object from a selection algorithm
#' @param name name of the selector
#' @param label label of the selector (for pretty printing)
#' @param all_terms a slot containing names of all terms
#' @param recipe_obj preprocessor trained from recipes package
#' @param selected_coefs a vector of only selected coefficients
#' @param default_infer the root string of the default infer method
#' @param meta a list containing important meta-information
#'
#' @return An S3 object (list) of class `selector` containing:'
#'   x
#'   all_terms
#'   meta
#'
#' @rdname selector
#'
#' @export

as_selector <- function(x, name, label = name, all_terms,
                        recipe_obj, orig_formula,#formula_full,
                        selected_terms,selected_coefs,
                        default_infer, meta = list()) {

  stopifnot(is.list(meta))
  stopifnot(is.character(all_terms))

  structure(x,
            class = c("selector", class(x)),
            name = name,
            label = label,
            all_terms = all_terms,
            recipe_obj = recipe_obj,
            #formula_full =formula_full,
            orig_formula=  orig_formula,
            selected_terms=selected_terms,
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

predict.selector <- function(object, newdata, scale = TRUE, ...) {

  if(missing(newdata))
    stop("Please provide newdata")

  rec_obj <- attr(object, "recipe_obj")

  baked <- bake(rec_obj, new_data = newdata) %>%
    as.data.frame()

  if (attr(object, "name") =="stepwise_ic") {
    formula_used <- formula(object)
    X <- model.matrix(formula_used, baked)
    beta <- coef(object)
    XX <- X[, names(beta), drop = FALSE]
  } else {
    formula_used <- attr(object, "orig_formula")
    X <- model.matrix(formula_used, baked)
    beta <- attr(object, "selected_coefs")
    XX <- X[, names(beta), drop = FALSE]
  }

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
#' @importFrom dplyr left_join filter select mutate if_else
#' @importFrom tibble tibble
#'
#' @rdname selector
#'
#' @export
tidy.selector <- function(x, scale_coef = TRUE, ...) {

  all_terms <- attr(x, "all_terms")
  if (c("(Intercept)")%in% all_terms ){all_terms=all_terms}else
     {all_terms=c("(Intercept)",all_terms)}

  rec_obj <- attr(x, "recipe_obj")

  # Coefficients from selected model
  coef_df <- tibble(term = names(coef(x)),
    estimate = as.numeric(coef(x)),
    selected =1)

  # Drop intercept for term-level table
  selected_terms <- coef_df $term

  base <- tibble(
    term = all_terms,
    selected1 = as.integer(all_terms %in% selected_terms)) %>%
    #left_join(coef_df_terms, by = "term")
    left_join(coef_df, by = "term")


  scale_step_idx <- which(tidy(rec_obj)$type == "scale")
  if(length(scale_step_idx)) {
    sds <- tidy(rec_obj, number = scale_step_idx) %>%
      select(term = terms, sd = value)
  } else {
    stop("a scaling step is required")
  }

  results <- base %>%
    left_join(sds, by = "term") %>%
    mutate(
      coef_scaled = estimate,
      coef_unscaled = dplyr::if_else(
        !is.na(sd),
        coef_scaled / sd,
        coef_scaled   ))

  if (scale_coef) {
    results <- results %>%
      mutate(coef = ifelse(is.na(coef_scaled), 0, coef_scaled),
            selected =ifelse(is.na(selected),0,selected))
  } else {
    results <- results %>%
     mutate(coef = ifelse(is.na(coef_unscaled), 0, coef_unscaled),
              selected =ifelse(is.na(selected),0,selected))
  }

  results %>%
    select(term, selected, coef)

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
#' Returns a vector with all selected coefficients (scaled only, includes
#' intercept)
#'
#' @param object a selector object
#' @param use_native if true, passes call to original class `coef`
#' @param ... objects passed to native function, otherwise not used.
#'
#' @rdname selector
#'
#' @export
coef.selector <- function(object, use_native = FALSE, ...) {
  if(use_native)
    return(NextMethod())
  attr(object, "selected_coefs")
}


#' A re-selection function
#'
#' Re-do the selection, possibly on new data. Does not re-do the pre-processing;
#' instead uses the original preprocess recipe from `selector_obj`.
#'
#' @param selector_obj a selector object
#' @param newdata a new data set (or same one)
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
