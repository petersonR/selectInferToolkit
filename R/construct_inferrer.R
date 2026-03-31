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
#' @importFrom dplyr left_join transmute case_when
#' @importFrom tibble tibble
#' @importFrom rlang .data
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
      select(term = .data$terms, sd = .data$value)
  } else {
    stop("a scaling step is required")
  }

  tidy_inferences <- inferences %>%
    left_join(sds, by = "term") %>%
    transmute(.data$term, .data$sd,
              estimate_scaled = .data$estimate,
              ci_low_scaled   = .data$ci_low,
              ci_high_scaled  =  .data$ci_high
    ) %>%
    mutate(
      estimate_unscaled = case_when(
        .data$term == "(Intercept)"  ~ estimate_scaled,
        is.na(.data$estimate_scaled) ~ NA_real_,
        !is.na(.data$sd)             ~ estimate_scaled / sd,
        # Below is case when selecting factors together in stepAIC fn, so their sd is NA
        is.na(.data$sd)              ~ estimate_scaled
      ),
      ci_low_unscaled = case_when(
        .data$term == "(Intercept)" ~ .data$ci_low_scaled,
        is.na(ci_low_scaled)  ~ NA_real_,
        !is.na(sd)           ~ .data$ci_low_scaled / .data$sd,
        # Below is case when selecting factors together in stepAIC fn, so their sd is NA
        is.na(sd)            ~ .data$ci_low_scaled
      ),
      ci_high_unscaled = case_when(
        term == "(Intercept)"  ~ .data$ci_high_scaled,
        is.na(ci_high_scaled)  ~ NA_real_,
        !is.na(sd)             ~ .data$ci_high_scaled / .data$sd,
        # Below is case when selecting factors together in stepAIC fn, so their sd is NA
        is.na(sd)              ~ .data$ci_high_scaled
      )
    )


  results <- results %>%
    left_join(tidy_inferences, by = c("term"))

  if(scale_coef) {
    results <- results %>%
      select(.data$term, .data$selected, .data$coef, estimate = .data$estimate_scaled,
             ci_low = .data$ci_low_scaled, ci_high = .data$ci_high_scaled)
  }  else {
    results <- results %>%
      select(.data$term, .data$selected, .data$coef, estimate = .data$estimate_unscaled,
             ci_low = .data$ci_low_unscaled, ci_high = .data$ci_high_unscaled)
  }

  # Check for other goodies, add if available
  to_add <- which(names(inferences) %in% c("p_value", "prop_selected"))
  if(length(to_add)) {
    results <- results %>%
      left_join(inferences[,c(1, to_add)], by = "term")
  }


  results
}

#' @param x an `inferrer`
#' @importFrom rlang .data
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
    mutate(ci_ln =  .data$ci_high -  .data$ci_low) %>%
    summarize(med = median( .data$ci_ln, na.rm = TRUE),
              mean = mean( .data$ci_ln, na.rm = TRUE),
              iqr_low = quantile( .data$ci_ln, .25, na.rm = TRUE),
              iqr_high = quantile( .data$ci_ln, .75, na.rm = TRUE),
              max = max( .data$ci_ln, na.rm = TRUE))

  pretty_median <- round(precision$med, 1)
  pretty_mean <- round(precision$mean, 1)
  pretty_max <- round(precision$max, 1)


  cat("Median CI length = ", pretty_median,"; mean = ", pretty_mean, "; max = ",
      pretty_max, "\n", sep = "")

  invisible(precision)
}

