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
  #selections <- setdiff(selections , "(Intercept)")
  nulls <- mod$term[is.na(mod$estimate)]

  nulls <- setdiff(nulls , "(Intercept)")

  selected <- ifelse(is.na(mod$estimate),0,1)
  #f <- as.formula(paste0("y ~ 1 + ", paste0(selections, collapse = "+ ")))
  #fit <- lm(f, data = data)
  # instead of refitting get the residuals

  data = cbind(x,res)
  for(j in nulls) {
    #f <- as.formula(paste0("res ~ 1 + ",  j))

    f <- as.formula(paste0("res ~ 1 + `", j, "`"))

    #print(f)

    # model the residuals 1+ variable j
    #print(mod$term == j)
    #print(tidy(lm(f,data = data), conf.int=T) )
    mod[mod$term == j,] <- tidy(lm(f,data = data), conf.int=T) %>%tail(1)
    mod$term <-gsub("`", "", mod$term )
  }
  mod$selected <-selected
  mod %>% mutate(   ci_ln = conf.high- conf.low)
}

