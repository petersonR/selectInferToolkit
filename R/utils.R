#' @importFrom broom tidy
#' @importFrom magrittr %>%
#' @importFrom rlang %||%
NULL

format_meta <- function(meta, digits = 3) {
  if (is.null(meta) || !length(meta)) return("none")

  fmt <- vapply(names(meta), function(nm) {
    val <- meta[[nm]]

    if (is.null(val)) return(paste0(nm, "=NULL"))

    # atomic numerics -> rounded
    if (is.numeric(val) && length(val) == 1)
      return(paste0(nm, "=", signif(val, digits)))

    # short character vectors
    if (is.character(val) && length(val) == 1)
      return(paste0(nm, "=", val))
    if (is.character(val))
      return(paste0(nm, "=[", paste(val, collapse = ","), "]"))

    # short logicals
    if (is.logical(val) && length(val) == 1)
      return(paste0(nm, "=", if (val) "TRUE" else "FALSE"))

    # short numeric vectors
    if (is.numeric(val) && length(val) <= 3)
      return(paste0(nm, "=[", paste(signif(val, digits), collapse = ","), "]"))

    # otherwise, abbreviate as list(...)
    paste0(nm, "=list(...)")
  }, character(1))

  paste(fmt, collapse = ", ")
}


# selectiveInference's fsInf()/fixedLassoInf() warn when they fall back to
# sd(y) for sigma, and point at estimateSigma() -- a function this package calls
# on the user's behalf rather than expecting them to call directly. Restate it
# in terms of the arguments infer_selective() actually exposes.
#
# Use as: withCallingHandlers(<si call>, warning = si_sigma_warning)
# The rewritten message deliberately avoids the phrase matched below, so
# re-signalling it here does not re-enter this handler.
si_sigma_warning <- function(w) {
  msg <- conditionMessage(w)
  if (!grepl("you may want to use the estimateSigma function", msg, fixed = TRUE))
    return(invisible(NULL))   # anything else passes through untouched

  sd_val <- sub(".*sd\\(y\\) = ([0-9.eE+-]+).*", "\\1", msg)

  warning(paste0(
    "p > n/2, so selectiveInference fell back to sd(y) = ", sd_val,
    " for sigma. That is biased upward whenever a predictor is genuinely ",
    "associated with the outcome, which widens intervals and makes p-values ",
    "conservative. Supply `sigma` if you have a better estimate, or set ",
    "`use_cv_sigma = TRUE` to estimate it by cross-validation."),
    call. = FALSE)

  invokeRestart("muffleWarning")
}

# Coefficients glm() could not estimate (aliased columns) come back NA; they
# are not selected
drop_aliased <- function(coefs) coefs[!is.na(coefs)]

# Fit y on columns `cols` of the design matrix X. The columns enter under
# syntactic stand-in names (v1, v2, ...), so any column name works without
# backticks or make.names(); coef_on_columns() and tidy_on_columns() translate
# back. For inference (lm_for_gaussian = TRUE), gaussian models go through
# lm() so that broom's intervals use t quantiles, matching the p-values
# (tidy() on a gaussian glm() gives normal-quantile intervals but t-based
# p-values).
#' @importFrom stats lm glm reformulate setNames
fit_on_columns <- function(X, y, cols, family, lm_for_gaussian = TRUE) {
  ids <- sprintf("v%d", seq_along(cols))  # character(0) when no columns
  df <- data.frame(.y = y)
  for (k in seq_along(cols)) df[[ids[k]]] <- X[, cols[k]]
  f <- if (length(ids)) reformulate(ids, ".y") else .y ~ 1
  fit <- if (family == "gaussian" && lm_for_gaussian) lm(f, data = df) else
    glm(f, data = df, family = family)
  attr(fit, "column_names") <- setNames(c("(Intercept)", cols),
                                        c("(Intercept)", ids))
  fit
}

coef_on_columns <- function(fit) {
  cf <- stats::coef(fit)
  names(cf) <- attr(fit, "column_names")[names(cf)]
  cf
}

# broom::tidy() of a fit_on_columns() fit, with column names and the
# package's column names for the interval
tidy_on_columns <- function(fit, conf.level) {
  td <- tidy(fit, conf.int = TRUE, conf.level = conf.level)
  tibble::tibble(term = unname(attr(fit, "column_names")[td$term]),
                 estimate = td$estimate, std_error = td$std.error,
                 ci_low = td$conf.low, ci_high = td$conf.high,
                 p_value = td$p.value)
}

#' @importFrom rlang .data
fill_in_nonselections <- function(inferences, selector_obj,
                                  nonselection, X, y, conf.level) {

  family <- attr(selector_obj, "meta")$family
  val <- tidy(selector_obj) %>%
    left_join(inferences, by = c("term", "selected"))

  if(nonselection == "confident_nulls") {
    val$estimate = ifelse(val$selected, val$estimate, 0)
    val$ci_low = ifelse(val$selected, val$ci_low, 0)
    val$ci_high = ifelse(val$selected, val$ci_high, 0)
    val$p_value = ifelse(val$selected, val$p_value, 1)
  }

  if(nonselection == "uncertain_nulls") {
    # each nonselected column gets the interval from adding it, alone, to the
    # selected model; zero-variance columns are not in X and stay NA
    selected_cols <- intersect(val$term[val$selected == 1], colnames(X))
    nonselected_cols <- intersect(val$term[val$selected == 0], colnames(X))

    for(col in nonselected_cols) {
      fit <- fit_on_columns(X, y, c(selected_cols, col), family)
      coef_row <- tidy_on_columns(fit, conf.level)
      coef_row <- coef_row[coef_row$term == col, ]
      if(nrow(coef_row) == 0) next  # aliased

      val$estimate[val$term == col] <- coef_row$estimate
      val$ci_low[val$term == col] <- coef_row$ci_low
      val$ci_high[val$term == col] <- coef_row$ci_high
      val$p_value[val$term == col] <- coef_row$p_value
    }
  }

  val
}
