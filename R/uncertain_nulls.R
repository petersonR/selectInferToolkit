#' Title
#'
#' @param mod model results in data frame with NA for estimates for variables that are
#' not seelcted
#' @param res residual from final selected model
#' @param x Design data frame
#'
#' @return tidy data frame with coefficients for all variables
#'
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

