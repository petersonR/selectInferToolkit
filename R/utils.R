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

fill_in_nonselections <- function(inferences, selector_obj,
                                  nonselection, X, y, conf.level,
                                  term_to_col = NULL) {

  family <- attr(selector_obj, "meta")$family
  val <- tidy(selector_obj) %>%
    left_join(inferences, by = c("term", "selected"))


  if(nonselection == "confident_nulls") {
    val$estimate <- ifelse(val$selected, val$estimate, 0)

    val$estimate = ifelse(val$selected, val$estimate, 0)
    val$ci_low = ifelse(val$selected, val$ci_low, 0)
    val$ci_high = ifelse(val$selected, val$ci_high, 0)
    val$p_value = ifelse(val$selected, val$p_value, 1)

  }

  if(nonselection == "uncertain_nulls") {
    selected_terms <- term_to_col$col[term_to_col$term %in% val$term[val$selected==1]]
    nonselected_terms <- val$term[!val$selected]

    if(length(nonselected_terms)) {

      df <- X
      df$y <- y

      for(term in nonselected_terms) {

        cols <- term_to_col$col[term_to_col$term == term]
        if(length(cols) == 0) next  # skip if no match

        all_terms <- c(selected_terms, cols)

        # construct formula with selected columns + current nonselected column(s)
        formula_str <- paste0("y ~ ", paste0("`", all_terms, "`", collapse = " + "))
        fit <- glm(as.formula(formula_str), data = df, family = attr(selector_obj, "meta")$family)

        # extract coefficient info for this term
        fit_tidy <- tidy(fit, conf.int = TRUE, conf.level = conf.level)
        coef_row <- fit_tidy[fit_tidy$term %in% cols, ]
        if(nrow(coef_row) == 0) next

        val$estimate[val$term == term] <- coef_row$estimate
        val$ci_low[val$term == term] <- coef_row$conf.low
        val$ci_high[val$term == term] <- coef_row$conf.high
        val$p_value[val$term == term] <- coef_row$p.value

      }
    }
  }

  if("term_clean" %in% colnames(val)){val %>% select(-term_clean)}

  val
}


NULL
clean_name <- function(x) {
  x<- gsub("\\s+", ".", x)        # spaces -> dot
  x <- gsub("-", ".", x)           # hyphens -> dot
  x <- gsub("\\+", ".", x)         # plus signs -> dot
  x
}

