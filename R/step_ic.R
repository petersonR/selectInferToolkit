#' Stepwise forwad/backward/bidirectional selection with AIC/BIC
#'
#'#' @description
#'
#' This function implements forwad/backward/bidirectional stepwise regression,
#' for use in the practicalPSI package
#'
#' @details You can learn more about package authoring with RStudio at:
#'
#' @param x Dataframe with predictors
#' @param y outcome vector
#' @param std if TRUE (defult), standardize design matrix
#' @param penalty AIC or BIC
#' @param direction the mode of stepwise search, can be one of "both", "backward", or "forward", with a default of "both"
#' @param ... Additional arguments that can be passed
#'
#' @return A data frame with betahat
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @importFrom stats lm
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats na.pass
#' @importFrom MASS stepAIC
#' @export

step_ic <- function(x,y,std=TRUE,penalty= "AIC", direction="both",...){

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

  full_model <- broom::tidy(lm(y~x_dup), conf.int = TRUE) %>% select(term, estimate)
  full_model$term<- c("intercept",colnames(x_dup))

  if(penalty=="AIC"){
    ## Step wise AIC
    model=  MASS::stepAIC(lm(y~., data = raw_data), trace =0, direction = direction)
    aic_mod <-broom::tidy( model, conf.int = T)
    data = model[["model"]]

    aic_full <-full_model %>%
      dplyr::select(term) %>%
      dplyr::left_join(aic_mod , by = "term") %>% select(term, estimate) %>% as.data.frame()

    val <- list(beta=aic_full, std=std,penalty=penalty, direction=direction,  x=data[,-1],y= data[,1],
                model=model)
    class(val) <- "selector"
    val
  }

  else if(penalty=="BIC"){
    # stepwise BIC
    model = MASS::stepAIC(lm(y~., data = raw_data), trace =0,k=log(nrow(raw_data)), direction = direction)
    bic_mod <- broom::tidy(model, conf.int = T)
    data = model[["model"]]

    bic_full <-full_model %>%
      dplyr::select(term) %>%
      dplyr::left_join(bic_mod, by = "term") %>% select(term, estimate)

    val <- list(beta=bic_full, std=std,penalty=penalty, direction=direction, x=data[,-1],y= data[,1],
                model=model)
    class(val) <- "selector"
    val

  }

}
