#' Constructor for `inferrer`s
#'
#' Create an `inferrer` wrapper around post-selection inference algorithms so they
#' can be used with the `selectInferToolkit` package and can harmonize results
#' across various selection algorithms (`inferrer`s). This is not a user-facing
#' function.
#'
#' @param x a slot for the main inferential object
#' @param name brief name for method
#' @param label label for method
#' @param nonselection how non-selections were/are handled
#' @param inferences a slot for post-selection inferences that should
#'   have confidence intervals (at least)
#' @param conf.level the confidence level used for intervals (1-`sig.level`)
#' @param meta a list containing important meta-information from the method
#' @param selector a carried-forward `selector` object
#'
#' @return An S3 object (list) of class `inferrer`
#'
#' @rdname infer
#'
#' @export

as_inferrer <- function(
    x,
    name,
    label = name,
    nonselection,
    inferences,
    conf.level,
    selector,
    meta = list()) {

  stopifnot(is.list(meta))

  structure(x,
            class = c("inferrer", class(x)),
            name = name,
            label = label,
            nonselection = nonselection,
            inferences = inferences,
            conf.level = conf.level,
            selector = selector,
            meta = meta)
}

#' tidy method for `inferrer` object
#'
#' Returns a tibble with all candidate variables, estimates (scaled & unscaled)

#' @param x an `inferrer`
#' @param scale_coef should scaled betas be returned, or unscaled?
#' @param ... additional parameters (not yet used)
#'
#' @importFrom tibble rownames_to_column
#' @importFrom broom tidy
#' @importFrom dplyr left_join transmute
#' @importFrom tibble tibble
#' @rdname inferrer
#'
#' @export
tidy.inferrer <- function(x, scale_coef = TRUE, ...) {

  inferences <- attr(x, "inferences")
  selector_obj <- attr(x, "selector")
  results <- tidy(selector_obj, scale_coef = scale_coef)

  rec_obj <- attr(selector_obj, "recipe_obj")

  scale_step_idx <- which(tidy(rec_obj)$type == "scale")

  if(length(scale_step_idx)) {
    sds <- tidy(rec_obj, number = scale_step_idx) %>%
      select(term = terms, sd = value)
  } else {
    stop("a scaling step is required")
  }

  tidy_inferences <- inferences %>%
    left_join(sds, by = "term") %>%
    transmute(
      term = term,
      sd= sd,
      estimate_scaled = estimate,
      ci_low_scaled = ci_low,
      ci_high_scaled = ci_high
    ) %>%
    mutate(
      estimate_unscaled = ifelse(term != "(Intercept)",
                                               estimate_scaled/sd,
                                 estimate_scaled),
      ci_low_unscaled = ifelse(term != "(Intercept)", ci_low_scaled/sd,
                               ci_low_scaled),
      ci_high_unscaled = ifelse(term != "(Intercept)",
                                ci_high_scaled/sd,
                                ci_high_scaled),
    )

  results <- results %>%
    left_join(tidy_inferences, by = c("term"))

  if(scale_coef) {
    results <- results %>%
      select(term, selected, coef, estimate = estimate_scaled,
             ci_low = ci_low_scaled, ci_high = ci_high_scaled)
  }  else {
    results <- results %>%
      select(term, selected, coef, estimate = estimate_unscaled,
             ci_low = ci_low_unscaled, ci_high = ci_high_unscaled)
  }

  # Check for other goodies, add if available
  to_add <- which(names(inferences) %in% c("p_value", "prop_selected"))
  if(length(to_add)) {
    results <- results %>%
      left_join(inferences[,c(1, to_add)], by = "term")
  }


  results
}

#'
#' @rdname inferrer
#' @export
print.inferrer <- function(x, ...) {

  selector_obj <- attr(x, "selector")

  cat(attr(x, "label"), "inference applied post",
      attr(selector_obj, "label"), "Selection \n")

  meta <- attr(x, "meta")

  beta <- coef(selector_obj)[-1]

  n_selections <- length(beta)
  p <- length(attr(selector_obj, "all_terms")) - 1

  cat(n_selections, "of", p, "variables selected.", "\n")

  cat("Nonselections were considered", attr(x, "nonselection"), "\n")

  cat("Meta information:", format_meta(meta), "\n")

  results <- tidy(x, scale_coef = TRUE)
  precision <- results %>%
    mutate(ci_ln = ci_high - ci_low) %>%
    summarize(med = median(ci_ln, na.rm = TRUE),
              mean = mean(ci_ln, na.rm = TRUE),
              iqr_low = quantile(ci_ln, .25, na.rm = TRUE),
              iqr_high = quantile(ci_ln, .75, na.rm = TRUE),
              max = max(ci_ln, na.rm = TRUE))

  pretty_median <- round(precision$med, 1)
  pretty_mean <- round(precision$mean, 1)
  pretty_max <- round(precision$max, 1)


  cat("Median CI length = ", pretty_median,"; mean = ", pretty_mean, "; max = ",
      pretty_max, "\n", sep = "")

  invisible(precision)
}

