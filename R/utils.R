#' @importFrom broom tidy
#' @importFrom magrittr %>%
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

fill_in_nonselections <- function(inferences, selector_obj, nonselection, X, y, conf.level) {

  family <- attr(selector_obj, "meta")$family
  val <- tidy(selector_obj) %>%
    left_join(inferences, by = c("term", "selected"))

  if(nonselection == "confident_nulls") {
    val <- val %>%
      mutate(estimate = ifelse(selected, estimate, 0),
             ci_low = ifelse(selected, ci_low, 0),
             ci_high = ifelse(selected, ci_high, 0)
             # , p_value = ifelse(selected, p_value, 1)
      )
  }

  if(nonselection == "uncertain_nulls") {

    selected_vars <- val$term[val$selected == 1][-1]
    nonselected_vars <- val$term[!val$selected]

    f_selected <- paste0(c("y ~ 1 ", selected_vars), collapse = " + ")

    if(length(nonselected_vars)) {
      f_selected_fo <- as.formula(f_selected)
      fit_selected <- glm(f_selected_fo, data = X)

      for(j in 1:length(nonselected_vars)) {
        f_j <- paste0(f_selected, " + ", nonselected_vars[j])
        fit_j <- glm(as.formula(f_j), data = X)
        val_j <- tail(tidy(fit_j, conf.int = TRUE), 1)
        val$estimate[val$term == nonselected_vars[j]] <- val_j$estimate
        val$ci_low[val$term == nonselected_vars[j]] <- val_j$conf.low
        val$ci_high[val$term == nonselected_vars[j]] <- val_j$conf.high
        # val$p_value[val$term == nonselected_vars[j]] <- val_j$p.value

      }
    }
  }
  val
}
