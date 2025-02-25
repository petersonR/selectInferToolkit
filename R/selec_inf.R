# Selective inference for stepwise
#' Title
#'
#' @param x  design matrix
#' @param y  outcome variable
#' @param mult
#'
#' @return  returns a data frame with results from forward stepwise selective inference
#' @importFrom selectiveInference fs
#' @importFrom selectiveInference fsInf
#' @export
#'
#'
#'

sel_inf_fs <- function(x,y, mult=2, intercept= TRUE, std= T, ...) {
  variable_names <- colnames(x)

  # Run forward stepwise selection and compute p-values and confidence intervals
  fs_result <- fs(x, y, intercept =intercept, normalize= std,... )  # Compute the forward selection object
  #print(fs_result)
  #fs_result <- fs(x_mat, y, intercept =T, normalize= std)  # Compute the forward selection object



  # Get AIC-based selection with confidence intervals
  out_aic <- fsInf(fs_result, type = "aic", mult= mult, verbose = FALSE,  alpha = 0.05,...)
  #print(out_aic)

  # Extract selected variable names and calculate coefficients
  selected_vars <- variable_names[out_aic$vars]

  # Calculate the coefficients
  coefficients <- as.vector(out_aic$vmat %*% y)
  names(coefficients) <- selected_vars

  # Combine the results into a data frame
  results <- data.frame(
    term = selected_vars,
    estimate = out_aic$sign*as.vector(coefficients),  # Convert matrix to vector
    conf.low =  out_aic$ci[,1],
    conf.high = out_aic$ci[,2],
    #ci_ln= round(out_aic$ci[,2]-out_aic$ci[,1],4),
    p.value =  out_aic$pv
  )%>% dplyr::arrange(term )

  results
}


# Selective inference for lasso
#' Title
#'
#' @param x  design matrix
#' @param y outcome variable
#' @param lam lambda.1se or lambda.min
#' @param std whether to standaddize design matrix
#' @param ... addtional arguments that can be passed to  cv.glmnet
#' @importFrom glmnet cv.glmnet
#' @importFrom selectiveInference fixedLassoInf
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' @return returns a data frame with results from lasso selective inference
#'
#'
#'

sel_inf <- function(x,y, lam = "lambda.min", std=FALSE,intercept= TRUE, ...) {
  n<- nrow(x)

  # Defualt is fault assuming when fitting initial data matrix, it was standardized
  #cv.glmnet standardizes by default,The coefficients are always returned on the original scale.

  if (std== TRUE){
    x.std=x
    fit_lso <- cv.glmnet(x = x.std, y = y, standardize=FALSE,intercept=intercept , ...)

  } else {
      #x.std=std(x)
    x.std=x
      fit_lso <- cv.glmnet(x = x.std, y = y, standardize=TRUE,intercept=intercept , ...)

  }

  lam <- fit_lso[[lam]]
  sig <- min(sqrt(fit_lso$cvm))

  b <- coef(fit_lso, s=lam, exact = TRUE,  x = x.std, y = y)[-1]

  # re-compute with smaller lambda if none selected (other)
  while(all(b == 0))
    b <- coef(fit_lso, s=lam*.99, exact = TRUE,  x = x.std, y = y)[-1]

  # fixed lasso function reuires no intercept in beta vector
  res <- fixedLassoInf(x=x.std, y= y, b, lam*n, alpha = .05, sigma = sig, intercept =TRUE)
  bb <- res$vmat %*% y
  B <- cbind(bb, res$ci, res$pv)
  dimnames(B) <- list(names(res$vars), c('estimate', 'conf.low', 'conf.high', 'p.value'))
  data.frame(B) %>%
    tibble::rownames_to_column("term")
}
