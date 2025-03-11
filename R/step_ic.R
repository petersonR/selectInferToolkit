#' Stepwise forwad/backward/bidirectional selection with AIC/BIC
#'
#'
#' @description This function implements forwad/backward/bidirectional stepwise regression,
#' for use in the practicalPSI package
#'
#'
#' @param x Dataframe/model matrix with predictors (without intercept)
#' @param y outcome vector
#' @param std if TRUE (default), standardize design matrix
#' @param penalty AIC or BIC
#' @param direction the mode of step wise search, can be one of "both", "backward", or "forward", with a default of "forward"
#' @param make_levels  whether to model selection after dummy coding for categorical variables (defult FALSE)
#' @param ... Additional arguments that can be passed with stepAIC function in MASS package
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#' @importFrom stats lm  model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @return A list of class `selector_ic` containing:#'
#' \item{beta}{a tibble containing term names and coefficients}
#' \item{std}{Was desing matrix standadrized}
#' \item{penalty}{penalty used (AIC or BIC)}
#' \item{direction}{the mode of step wise search}
#' \item{x}{the model dataframe used}
#' \item{y}{repsonse used in vector}
#' \item{model_sum}{the stepwise-selected model  details is returned}
#' @export



step_ic <- function(x,y,std=FALSE,penalty= "AIC", direction="forward",make_levels= FALSE,...){

  if(is.matrix(x)){
    if(std==TRUE){
      x_std= data.frame(x, check.names = FALSE)%>%
        mutate_if(is.numeric, scale)
      colnames(x_std) = colnames(x)
    }
    else{
      x_std= data.frame(x, check.names = FALSE)
    }
  }
  else if (is.data.frame(x)){
    if(std==TRUE){
      x_std = x %>%
        mutate_if(is.numeric, scale)
      colnames(x_std) = colnames(x)
      #x_dup<- model.matrix(y ~., model.frame(~ ., cbind(x_std,y), na.action=na.pass))[,-1]

    }
    else{
      x_std=x
      #x_dup<- model.matrix(y ~., model.frame(~ ., cbind(x,y), na.action=na.pass))[,-1]

    }
  }


  raw_data = as.data.frame(cbind(x_std,y))
  rownames(raw_data) <- NULL

  # fit full model to get term names
  full_model <- broom::tidy(lm(y~., data= raw_data ), conf.int = TRUE) %>% select(term, estimate)

  #full_model$term<- c("(Intercept)",colnames(x_dup))

  if(penalty=="AIC"){
    if (direction == "forward") {
      if(make_levels== TRUE){
        fact_vars <- names(x_std)[sapply(x_std, is.factor)]
        if (length(fact_vars) > 0) {
          dummy_data <- model.matrix(~ . - 1, data = x_std[, fact_vars, drop = FALSE])  # -1 removes intercept
          x_std_transformed <- cbind(x_std[, !names(x_std) %in% fact_vars, drop = FALSE], dummy_data)
        } else {
          x_std_transformed <- x_std  # If no factors, use the original dataset
        }

        # Clean column names (optional, if needed for safety)
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x_std_transformed)),
                                 paste0("`", colnames(x_std_transformed), "`"),
                                 colnames(x_std_transformed))
        colnames(x_std_transformed)
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        raw_data_fs <- as.data.frame(cbind(x_std_transformed, y))
        model <- MASS::stepAIC(
          lm(y ~ 1, data = raw_data_fs),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward",
          k = 2,
          trace = 0, ...
        )
      }
      else{
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x_std)),
                                 paste0("`", colnames(x_std), "`"),
                                 colnames(x_std))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        model = MASS::stepAIC(
          lm(y ~ 1, data = raw_data),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward", k=2, trace = 0,...)
      }


    }
    else{
      if(make_levels== TRUE){
        fact_vars <- names(x_std)[sapply(x_std, is.factor)]
        if (length(fact_vars) > 0) {
          dummy_data <- model.matrix(~ . - 1, data = x_std[, fact_vars, drop = FALSE])  # -1 removes intercept
          x_std_transformed <- cbind(x_std[, !names(x_std) %in% fact_vars, drop = FALSE], dummy_data)
        } else {
          x_std_transformed <- x_std  # If no factors, use the original dataset
        }

        # Clean column names (optional, if needed for safety)
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x_std_transformed)),
                                 paste0("`", colnames(x_std_transformed), "`"),
                                 colnames(x_std_transformed))
        colnames(x_std_transformed)
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        raw_data_fs <- as.data.frame(cbind(x_std_transformed, y))
        model <- MASS::stepAIC(
          lm(y ~ 1, data = raw_data_fs),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = direction,
          k = 2,
          trace = 0, ...)
      }

      else{
        model=  MASS::stepAIC(lm(y~., data = raw_data), trace =0, direction = direction,...)

      }

    }


    ### Final data frames to return
    if(make_levels== TRUE){
      aic_mod <-broom::tidy( model, conf.int = T)%>%mutate(term = gsub("`", "", term))


      aic_full <- data.frame(term= colnames(x_std_transformed)) %>%dplyr::full_join(aic_mod, by = "term") %>%
        select(term, estimate) %>%
        as.data.frame()


      } else {
        aic_mod <-broom::tidy( model, conf.int = T)


        aic_full <-full_model %>%
          dplyr::select(term) %>%
          dplyr::left_join(aic_mod , by = "term") %>% select(term, estimate) %>% as.data.frame()

      }

    data = model[["model"]]
    val <- list(beta=aic_full, std=std,penalty=penalty, direction=direction,
                x_original=x,
                y= data[,1],
                x_model  =raw_data %>% select(-y),
                model_sum= summary(model))
    class(val) <- "selector_ic"
    val

  }


  else if(penalty=="BIC"){
    # stepwise BIC
    if(direction =="forward"){
      if(make_levels== TRUE){
        fact_vars <- names(x_std)[sapply(x_std, is.factor)]
        if (length(fact_vars) > 0) {
          dummy_data <- model.matrix(~ . - 1, data = x_std[, fact_vars, drop = FALSE])  # -1 removes intercept
          x_std_transformed <- cbind(x_std[, !names(x_std) %in% fact_vars, drop = FALSE], dummy_data)
        } else {
          x_std_transformed <- x_std  # If no factors, use the original dataset
        }

        # Clean column names (optional, if needed for safety)
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x_std_transformed)),
                                 paste0("`", colnames(x_std_transformed), "`"),
                                 colnames(x_std_transformed))
        colnames(x_std_transformed)
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        raw_data_fs <- as.data.frame(cbind(x_std_transformed, y))
        model <- MASS::stepAIC(
          lm(y ~ 1, data = raw_data_fs),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward",
          trace = 0,
          k=log(nrow(raw_data)), ...)
      }
      else{
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x_std)),
                                 paste0("`", colnames(x_std), "`"),
                                 colnames(x_std))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        model = MASS::stepAIC(
          lm(y ~ 1, data = raw_data),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward", k=log(nrow(raw_data)), trace = 0,...
        )
      }

      #model=  MASS::stepAIC(lm(y~., data = raw_data), trace =0, k=log(nrow(raw_data)),direction = direction)
    } else{
      #clean_colnames <- sapply(colnames(x_dup), function(name) {
      if(make_levels== TRUE){
        fact_vars <- names(x_std)[sapply(x_std, is.factor)]
        if (length(fact_vars) > 0) {
          dummy_data <- model.matrix(~ . - 1, data = x_std[, fact_vars, drop = FALSE])  # -1 removes intercept
          x_std_transformed <- cbind(x_std[, !names(x_std) %in% fact_vars, drop = FALSE], dummy_data)
        } else {
          x_std_transformed <- x_std  # If no factors, use the original dataset
        }
        # Clean column names (optional, if needed for safety)
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x_std_transformed)),
                                 paste0("`", colnames(x_std_transformed), "`"),
                                 colnames(x_std_transformed))
        colnames(x_std_transformed)
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        raw_data_fs <- as.data.frame(cbind(x_std_transformed, y))
        model <- MASS::stepAIC(
          lm(y ~ 1, data = raw_data_fs),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = direction,
          k = log(nrow(raw_data)),
          trace = 0, ...)
      }

      else{
        model=  MASS::stepAIC(lm(y~., data = raw_data), trace =0, k=log(nrow(raw_data)), direction = direction,...)

      }

    }

    ### Final dataframes to return
    if(make_levels== TRUE){
      bic_mod <-broom::tidy( model, conf.int = T)%>%mutate(term = gsub("`", "", term))


      bic_full <- data.frame(term= colnames(x_std_transformed)) %>%dplyr::full_join(bic_mod, by = "term") %>%
        select(term, estimate) %>%
        as.data.frame()


    } else {
      bic_mod <-broom::tidy( model, conf.int = T)


      bic_full <-full_model %>%
        dplyr::select(term) %>%
        dplyr::left_join(bic_mod , by = "term") %>% select(term, estimate) %>% as.data.frame()

    }

    data = model[["model"]]
    val <- list(beta=bic_full, std=std,penalty=penalty, direction=direction,
                x_original=x,
                y= data[,1],
                x_model = raw_data %>% select(-y),
                model_sum= summary(model))
    class(val) <- "selector_ic"
    val


  }

}

#' Title
#'
#' @param x model of class `selector_ic`
#' @param ... additioanl arugments (currerntly not suported)
#' @method print `selector_ic`
#' @return returns x invisibily
#' @export
print.selector_ic <- function(x, ...) {
  cat("Stepwise Model Selection Summary:\n")

  # Model direction
  cat("Direction of Selection: ", x$direction, "\n")

  # Penalty used for selection
  cat("Penalty used: ", x$penalty, "\n")

  # Standard errors used in the model
  if(x$std) {
    cat("Design Matrix standardized: TRUE\n")
  } else {
    cat("Design Matrix standardized: FALSE\n")
  }

  # Model coefficients (first few)
  cat("\nFinal Model Coefficients:\n")
  print( x[["model_sum"]][["coefficients"]][,1])


}


#' Title
#'
#' @param x model of class `selector_ic`
#' @param ... Additional arguments passed to the generic `tidy` function.
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @return  A tibble containing the tidied coefficients of the model.
#' @export
tidy.selector_ic <- function(x, ...) {

  ret<- as_tibble(x[["model_sum"]]$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  ret <- ret %>% select(term, estimate)
  return(ret)
}
