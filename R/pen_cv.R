#' Title
#'
#' @param x Dataframe with predictors
#' @param y outcome vector
#' @param std if TRUE (defult), standardize design matrix
#' @param penalty lasso or MCP
#' @param lambda  extrac coefficeints assoicated wtih lambda min or lamba SE
#' @param ... Additional arguments that can be passed
#'
#' @return  A data frame with betahat
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @importFrom stats lm
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats na.pass
#' @importFrom MASS stepAIC
#' @importFrom ncvreg cv.ncvreg
#' @export
#'

pen_cv <- function(x,y,std=TRUE,penalty= "lasso",lambda="min",...){

  if(std==TRUE){
    x_std = x %>%
      mutate_if(is.numeric, scale)
    colnames(x_std) = colnames(x)
    x_dup<- model.matrix(y ~., model.frame(~ ., cbind(x_std,y), na.action=na.pass))[,-1]

  } else {
    x_dup<- model.matrix(y ~., model.frame(~ ., cbind(x,y), na.action=na.pass))[,-1]
  }

  raw_data = as.data.frame(cbind(x_dup,y))
  rownames(raw_data) <- NULL

  full_model <- broom::tidy(lm(y~x_dup), conf.int = TRUE)[-1,] %>% select(term, estimate)
  full_model$term<- colnames(x_dup)

  fit <- cv.ncvreg(X = x_dup, y = y, penalty = penalty, family="gaussian")
  foldid <-fit[["fold"]]

  # Calculate correct lambda
  if(lambda=="min"){
    lambda_mod =  fit$lambda[which.min(fit$cve)]
  } else{
    lambda_mod <- fit$lambda[which(fit$cve < min(fit$cve + fit$cvse))[1]]

  }

  tolerance <- 1e-6 # Define a small tolerance
  lambda_index <- which.min(abs(fit$lambda - lambda_mod)) # Find the closest lambda
  beta <- data.frame(term=names(coef(fit, which = lambda_index)) ,
                     estimate=unname(coef(fit, which = lambda_index)))


  val <- list( beta=beta, std=std,penalty=penalty, lambda=lambda, lambda.select= lambda_mod,
              fold=foldid, x=fit[["fit"]][["X"]], y=fit[["fit"]][["y"]] )
  class(val) <- "selector"
  val

}
