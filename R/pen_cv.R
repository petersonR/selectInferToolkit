#' Fit an MCP- or SCAD-penalized regression
#' @description
#' Performs k-fold cross validation for MCP- or SCAD-penalized regression
#' models over a grid of values for the regularization parameter lambda and returns the
#' coefficeints associated with either value of lambda that gives minimum cvm or largest value of lambda such that error is within 1 standard error of the minimum.
#'
#' @param x Dataframe/model matrix with predictors (without intercept)
#' @param y outcome vector
#' @param std if TRUE (default), standardize design matrix
#' @param penalty lasso or MCP
#' @param lambda  extra coefficients associated with "lambda.min" or "lambda.1se"
#' @param alpha Tuning parameter  small, but not exactly 0.
#' @param ... Additional arguments that can be passed with cv.ncvreg function in cv.ncvreg package
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix
#' @importFrom stats na.pass
#' @importFrom MASS stepAIC
#' @importFrom ncvreg cv.ncvreg
#' @return A list of class `selector_pen` containing:
#' \item{beta}{a tibble containing term names and coefficients}
#' \item{std}{Was desing matrix standadrized}
#' \item{penalty}{penalty used (lasso or MCP)}
#' \item{lambda}{are the coefficeint associated with "lambda.min" or "lambda.1se"}
#' \item{lambda.select}{numeric value of selected lambda}
#' \item{fold}{Which fold each observation belongs to. By default the observations are randomly assigned.}
#' \item{x}{ the model dataframe used}
#' \item{y}{repsonse used}
#' \item{alpha}{selected alpha for model fitting}
#' @export

pen_cv <- function(x,y,std=TRUE,penalty= "lasso",lambda="lambda.min",alpha=1,...){

  if (is.matrix(x) || is.data.frame(x)) {
    if (std) {
      # Standardize numeric columns
      x_std <- as.data.frame(x, check.names = FALSE)
      num_cols <- sapply(x_std, is.numeric)
      x_std[num_cols] <- lapply(x_std[num_cols], scale)

      # Preserve original column names
      colnames(x_std) <- colnames(x)

      # Create model matrix
      x_dup <- model.matrix(
        y ~ .,
        model.frame(~., data = data.frame(cbind(x_std, y), check.names = FALSE), na.action = na.pass)
      )[, -1]
    } else {
      # No standardization, directly build model matrix
      x_dup <- model.matrix(
        y ~ .,
        model.frame(~., data = data.frame(cbind(x, y), check.names = FALSE), na.action = na.pass)
      )[, -1]
    }
  }
  # Note: ncvreg standardizes the data and includes an intercept by default.

  raw_data = as.data.frame(cbind(x_dup,y))
  rownames(raw_data) <- NULL

  full_model <- lm(y ~ ., data= raw_data)
  coef_est <- coef(full_model)
  full_model_df <- data.frame(term = names(coef_est))
  full_model$term <- gsub("`", "", full_model$term)


  if (alpha==1 & penalty=="lasso"){
    #for glmnet alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
    fit <- cv.glmnet(x = x_dup, y = y, alpha = alpha, standardize = F, family = "gaussian", ...)
    foldid <-fit[["foldid"]]
    lmax <- max(fit$lambda)
    lambda_seq= fit[["lambda"]]

  }else{
    #alpha=1 is equivalent to MCP/SCAD penalty,
    fit <- cv.ncvreg(X = x_dup, y = y,  penalty = penalty, family="gaussian",alpha=alpha,...)
    foldid <-fit[["fold"]]
    lmax <- max(fit$lambda)
    lambda_seq=  fit[["lambda"]]

  }


  # Calculate correct lambda
  if(lambda=="lambda.min"){
    lambda_mod =  fit[["lambda.min"]]
  } else if (lambda=="lambda.1se"){
    if(alpha==1 & penalty=="lasso"){
      lambda_mod =  fit[["lambda.1se"]]
    }
    else{
      lambda_mod <- fit$lambda[which(fit$cve < min(fit$cve + fit$cvse))[1]]

    }
  }

  if (alpha==1 & penalty=="lasso"){
      b <- coef(fit, s=  lambda_mod, exact = TRUE)
      beta <- data.frame(term = rownames(b), estimate = as.vector(b))

  }else{
    tolerance <- 1e-6 # Define a small tolerance
    lambda_index <- which.min(abs(fit$lambda - lambda_mod)) # Find the closest lambda
    beta <- data.frame(term=names(coef(fit, which = lambda_index)) ,
                       estimate=unname(coef(fit, which = lambda_index)))

  }

  val <- list( beta=beta, std=std,penalty=penalty, lambda_seq=  lambda_seq, lambda=lambda,
               lambda.select= lambda_mod,lmax=lmax,
              fold=foldid, x=x_dup, y= y,
              alpha=alpha,    x_original=x, model=   fit)
  class(val) <- "selector_pen"
  val

}


#' Title
#'
#' @param x  model of class `selector_pen`
#' @importFrom tibble as_tibble
#' @method print `selector_pen`
#' @return returns x invisibly
#' @export
print.selector_pen <- function(x, ...) {
  cat("Penalized regression  Model Summary:\n")

  # Standard errors used in the model
  if(x$std) {
    cat("Design Matrix standardized: TRUE\n")
  } else {
    cat("Design Matrix standardized: FALSE\n")
  }

  # Penalty used for selection
  cat("Penalty used: ", x$penalty, " and alpha:",x$alpha, "\n")

  # lambda
  cat("Coefficient associated with : ", x$lambda, "\n")

  beta = x[["beta"]][x[["beta"]][["estimate"]]!=0,][,2]
  names(beta) =x[["beta"]][x[["beta"]][["estimate"]]!=0,][,1]

  # Model coefficients (first few)
  cat("\nFinal Model Non-Zero Coefficients:\n")
  print(beta)

}


#' Title
#' @param x   model of class `selector_pen`
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.selector_pen <- function(x, ...) {

  ret<- as_tibble(x[["beta"]])
  return(ret)
}
