# Internal helper: clean colnames to avoid special characters
#' Title
#'
#' @param nms a vector with column names to be cleaned
#'
#' @return a vector with clean columns
#'
#'

clean_colnames_fn <- function(nms) {
  ifelse(grepl("[^a-zA-Z0-9._]", nms), paste0("`", nms, "`"), nms)
}

# Transform factor variables to dummies if needed
#' Title
#'
#' @param data  data to be processed
#' @param make_levels whether to make dummy variables for each factor level
#'
#' @return model matrix with clean dataset

dummy_col_fn <- function(data, make_levels) {
  if (!make_levels) return(data)

  # Identify factor and numeric variables
  fact_vars <- names(data)[sapply(data, is.factor)]
  if (length(fact_vars) == 0) return(data)

  dummy_data <- model.matrix(~ . - 1, data = data[, fact_vars, drop = FALSE])[,-1]
  numeric_data <- data[, !names(data) %in% fact_vars, drop = FALSE]

  cbind(numeric_data, dummy_data)
}

#' An (internal) function to obtain uncertain null effects
#'
#' @param mod model results in data frame with NA for estimates for variables that are
#' not seelcted
#' @param res residual from final selected model
#' @param x Design data frame
#'
#' @return tidy data frame with coefficients for all variables
#'
get_uncertain_nulls <- function(mod, res, x) { # val will be residuals
  selections <- mod$term[!is.na(mod$estimate)]
  nulls <- mod$term[is.na(mod$estimate)]
  nulls <- setdiff(nulls , "(Intercept)")

  selected <- ifelse(is.na(mod$estimate),0,1)
  data = cbind(x,res)
  for(j in nulls) {
    f <- as.formula(paste0("res ~ 1 + `", j, "`"))
    model <- lm(f,data = data)
    coefs <- summary(model)$coefficients
    conf_int <- confint( model)

    # Create full_mod data frame
    model_sum <- data.frame(
      term = gsub("`", "", rownames(coefs)),
      estimate = coefs[, "Estimate"],
      std.error = coefs[,"Std. Error"],
      p.value = coefs[, "Pr(>|t|)"],
      conf.low = conf_int[, 1],
      conf.high = conf_int[, 2],
      stringsAsFactors = FALSE
    )
    rownames( model_sum) <- NULL

    mod[mod$term == j,] <- model_sum[2,]
  }
  mod$selected <-selected
  mod$ci_ln <- mod$conf.high-mod$conf.low
  mod$na_coeff = ifelse(is.na(mod$estimate),1,0)

  # Check for rows where `na_coeff` is 1 and return a warning message
  for (i in 1:nrow(mod)) {
    if (mod$na_coeff[i] == 1) {
      warning(paste("Coefficient for variable", mod$term[i], "is NA"))
    }
  }

  return(mod)
}


