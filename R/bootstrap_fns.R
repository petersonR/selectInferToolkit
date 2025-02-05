
#' Title
#'
#' @param model  lm object with design matrix and output variable
#' @param B The number of bootstrap replicates.
#' @param family currently only "gaussian" supported
#' @param parallel whether to run bootstrap in parallel for faster computation
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize bind_rows
#' @importFrom broom tidy
#' @importFrom stats lm
#' @importFrom parallel detectCores makeCluster clusterExport
#' @importFrom pbapply pbapply
#' @return A tidy dataframe with bootstrap result and CIs
#' \item{term}{variable name}
#' \item{mean_estimate}{mean of regression coefficients across bootstrap samples}
#' \item{conf.low}{lower 2.5 percentile bootstrap interval}
#' \item{conf.high}{upper 97.5 percentile bootstrap interval}
#' \item{median_p.value}{median p value of regression coefficients  across bootstrap samples}
#' \item{ci_ln}{confidence interval length}
#' \item{prop.rej}{proportion of time coefficient was found significant at 0.05 alpha level}
#' \item{ci_avg_ratio}{Average of CI length across all variables after bootstrap}
#' \item{ci_median_ratio}{median of CI length across all variables after bootstrap}
#'
#' @export
#'

full_boot <- function(model, B = 250,family="gaussian",parallel = FALSE) {
  if (is.null(model[["y"]]) |is.null(model[["x"]])){
    print("Please rerun lm function with options x= TRUE and y=TRUE to save data to run bootstrap")
  }else{

  }
  x=model[["x"]]
  x<- x[, colnames(x) != "(Intercept)"]
  y <- model[["y"]]

  # Fit the full model first to get the coefficient names
  fit_full <- broom::tidy(lm(y ~ x), conf.int = TRUE) %>%
    dplyr::mutate(term = c("(Intercept)",colnames(x)))


  if (!parallel){

    # Pre-allocate list for results
    boot_fits <- vector("list", B)

    #boot_fits <- list(numeric(B))

    # boostrap the data and store regression results
    for(b in 1:B) {
      boot_id <- sample(seq_len(nrow(x)), replace = TRUE)
      y_boot <- y [boot_id]
      x_boot <- x[boot_id, , drop = FALSE]

      # Fit bootstrap model and retrieve coefficients
      fits <- broom::tidy(lm(y_boot ~ x_boot), conf.int = TRUE) %>%
        dplyr::mutate(term = c("(Intercept)",colnames(x)))


      boot_fits[[b]] <- fit_full %>%
        dplyr::select(term) %>%
        dplyr::left_join(fits, by = "term") %>%
        dplyr::mutate(  ci_length_boot = conf.high-conf.low,boot = b)
    }

    # get the results metrics by summarizing across bootstrap samples
    dplyr::bind_rows(boot_fits) %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      dplyr:: summarize(
        mean_estimate = mean(estimate, na.rm = TRUE),
        conf.low = quantile(estimate, .025, na.rm = TRUE),
        conf.high = quantile(estimate, .975, na.rm = TRUE),
        median_p.value = median(p.value, na.rm = TRUE),
        ci_ln = conf.high - conf.low,
        #prop.na = round(mean(is.na(estimate)), 4),
        prop.rej = mean(p.value < 0.05, na.rm = TRUE)
      )   %>%
      dplyr::mutate(      ci_avg_ratio = mean( ci_ln, na.rm = T),
                   ci_median_ratio = median(ci_ln, na.rm=T))
  }
  # do with parallel computing
  else{
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x", "y", "fit_full", "B"), envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
    })


    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_id <- sample(seq_len(nrow(x)),replace = TRUE)
      y_boot <- y[boot_id]
      x_boot <- x[boot_id, , drop = FALSE ]
      fits <- broom::tidy(lm(y_boot ~ x_boot), conf.int = TRUE) %>%
        dplyr::mutate(term = c("(Intercept)",colnames(x)))

      # Return results aligned with the full model
      return(
        fit_full %>%
          dplyr::select(term) %>%
          dplyr::left_join(fits, by = "term") %>%
          dplyr::mutate(ci_length_boot = conf.high - conf.low, boot = b)
      )
    }, cl = cl)

    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    dplyr::bind_rows(boot_fits) %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = mean(estimate, na.rm = TRUE),
        conf.low = quantile(estimate, .025, na.rm = TRUE),
        conf.high =quantile(estimate, .975, na.rm = TRUE),
        median_p.value =median(p.value, na.rm = TRUE),
        ci_ln = conf.high - conf.low,
        prop.rej = mean(p.value < 0.05, na.rm = TRUE),
      ) %>%
      dplyr::mutate(
        ci_avg_ratio = mean(ci_ln , na.rm = TRUE) ,
        ci_median_ratio = median(ci_ln , na.rm = TRUE)
      )


  }

}







#' Title
#'
#' @param x  dataframe with predictors
#' @param y outcome vector
#' @param B The number of bootstrap replicates.
#' @param family Currently only "gaussian" supported
#' @param nonselector A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param parallel whether to run bootstrap in parallel for faster computation
#' @param direction the mode of stepwise search, can be one of "both", "backward", or "forward",
#' with a default of "both". If the scope argument is missing the default for direction is "backward".
#' @param model Selected model from whole data set and stepwise AIC method
#' @param ... 	any additional arguments to that can be passed to stepAIC
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize bind_rows
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @importFrom forcats fct_inorder
#' @return Tidy dataframe with bootstrap result and CIs
#' \item{term}{variable name}
#' \item{mean_estimate}{mean of regression coefficients across bootstrap samples}
#' \item{conf.low}{lower 2.5 percentile bootstrap interval}
#' \item{conf.high}{upper 97.5 percentile bootstrap interval}
#' \item{median_p.value}{median p value of regression coefficients  across bootstrap samples}
#' \item{ci_ln}{confidence interval length}
#' \item{prop.select}{propotion of times a given variable is selected by model selection method}
#' \item{prop.rej}{proportion of time coefficient was found significant at 0.05 alpha level}
#
#'

boot_stepwise_aic <- function(x,y, B = 250,family="gaussian",nonselector="ignored", parallel= FALSE,
                              direction="both", model=model, ...) {

  data= data.frame(cbind(y,x))
  #fit_full <- broom::tidy(lm(y ~ ., data = data))
  #fit_aic<- tidy(model[["model_sum"]])

  non_zero_terms <- model[["beta"]][["term"]][! is.na(model[["beta"]][["estimate"]]) ]
  selected_vars <- data.frame(term =   non_zero_terms)
  all_vars <- data.frame(term = model[["beta"]][["term"]])


  if ( nonselector=="ignored" & parallel == FALSE){
    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
       fit = broom::tidy(MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward", trace = 1,...
        ), conf.int = T)


      } else{
        fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0, direction = direction,...), conf.int = T)

      }
      #fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0, direction = "both"), conf.int = T)[-1,]

      boot_fits[[b]] <-    selected_vars    %>%
        dplyr::select(term) %>%
        dplyr::left_join(  fit, by = "term") %>%
        dplyr::mutate(is.select = ifelse(is.na(estimate),0,1),
               p.value = ifelse(is.na(p.value), 1, p.value),
               estimate = ifelse(is.na(estimate), 0,  estimate),
               boot = b)
    }


    boot_fits  %>%
      dplyr:: bind_rows() %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(estimate, na.rm=T),4),
        conf.low = round(quantile(estimate, .025, na.rm=T),4),
        conf.high = round(quantile(estimate, .975, na.rm = T),4),
        median_p.value  = round(median(p.value, na.rm=T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select==1),4),
        prop.rej= round(mean(p.value<0.05, na.rm=T),4),
      )
  }
  else if(nonselector=="ignored"& parallel == TRUE){
    # do with parallel computing
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x", "y", "selected_vars","direction" ),envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
    })

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        fit = broom::tidy(MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward", trace = 1,...
        ), conf.int = T)


      } else{
        fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0, direction = direction,...), conf.int = T)

      }

      # Return results aligned with the full model
      return(
        selected_vars  %>%
          dplyr:: select(term) %>%
          dplyr::left_join(  fit, by = "term") %>%
          dplyr::mutate(  is.select = ifelse(is.na(estimate),0,1),
                   p.value = ifelse(is.na(p.value), 1, p.value),
                   estimate = ifelse(is.na(estimate), 0,  estimate),
                   boot = b)
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    boot_fits  %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(estimate,na.rm=T),4),
        conf.low = round(quantile(estimate, .025,na.rm=T),4),
        conf.high = round(quantile(estimate, .975,na.rm=T),4),
        median_p.value  = round(median(p.value,na.rm=T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select==1),4),
        prop.rej= round(mean(p.value<0.05, na.rm=T),4),
      )
  }
  else if(nonselector=="confident_nulls" & parallel == FALSE){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]
      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        fit = broom::tidy(MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward", trace = 1,...
        ), conf.int = T)


      } else{
        fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0, direction = direction,...), conf.int = T)

      }

      boot_fits[[b]] <-all_vars    %>%
        select(term) %>%
        dplyr::left_join(  fit, by = "term") %>%
        dplyr::mutate(is.select = ifelse(is.na(estimate),0,1),
               p.value = ifelse(is.na(p.value), 1, p.value),
               estimate = ifelse(is.na(estimate), 0,  estimate),
               boot = b)
    }


    boot_fits  %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(estimate),4),
        conf.low = round(quantile(estimate, .025),4),
        conf.high = round(quantile(estimate, .975),4),
        median_p.value  =  round(median(p.value),4),
        ci_ln = round(conf.high-conf.low,4),
        prop.select = round(mean(is.select==1),4),
        prop.rej= round(mean(p.value<0.05),4),
        #ci_avg_ln_boot= mean(ci_length_boot, na.rm=T)
      )


  }
  else if(nonselector=="confident_nulls"& parallel == TRUE){
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x", "y", "all_vars","direction"),envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
    })

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        fit = broom::tidy(MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward", trace = 1,...
        ), conf.int = T)


      } else{
        fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0, direction = direction,...), conf.int = T)

      }

      # Return results aligned with the full model
      return(
        all_vars    %>%
          dplyr:: select(term) %>%
          dplyr::left_join(  fit, by = "term") %>%
          dplyr::mutate(  is.select = ifelse(is.na(estimate),0,1),
                   p.value = ifelse(is.na(p.value), 1, p.value),
                   estimate = ifelse(is.na(estimate), 0,  estimate),
                   boot = b)
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    boot_fits  %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(estimate,na.rm=T),4),
        conf.low = round(quantile(estimate, .025,na.rm=T),4),
        conf.high = round(quantile(estimate, .975,na.rm=T),4),
        median_p.value  = round(median(p.value,na.rm=T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select==1),4),
        prop.rej= round(mean(p.value<0.05, na.rm=T),4),
      )

  }
  else if(nonselector=="uncertain_nulls" & parallel == FALSE){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        model<- MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward", trace = 0
        )

        tidy_fit = broom::tidy(model, conf.int = T)


      } else{
        model<-MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0, direction = direction)
        tidy_fit<- broom::tidy(model, conf.int = T)

      }
      res =residuals(model)

      aic_ignore_full <-all_vars   %>%
        dplyr::select(term) %>%
        dplyr::left_join(tidy_fit, by = "term")

      data_boot <- droplevels(data_boot)
      x_dup<- as.data.frame(model.matrix(y ~., data_boot,check.names=FALSE))[,-1]

      print(colnames(x_dup))
      get_uncertain_nulls (mod=aic_ignore_full, res=res, x=x_dup) %>%
        select(term, estimate, selected, p.value)

      boot_fits[[b]] <-  get_uncertain_nulls (mod=aic_ignore_full, res=res, x=x_dup) %>%
        select(term, estimate, selected, p.value) %>%
        dplyr::mutate(   boot = b)
    }


    boot_fits  %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(estimate),4),
        conf.low = round(quantile(estimate, .025),4),
        conf.high = round(quantile(estimate, .975),4),
        ci_ln = round(conf.high-conf.low,4),
        prop.select = round(mean(selected==1),4),
        prop.rej= round(mean(p.value<0.05),4),
        #ci_avg_ln_boot= mean(ci_length_boot, na.rm=T)
      )


  }
  else if(nonselector=="uncertain_nulls" & parallel == TRUE){
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    parallel::clusterExport(cl, varlist = c("x", "y", "all_vars","direction" ),envir = environment())

    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(MASS)
    })


    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        model<- stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          direction = "forward", trace = 0
        )

        tidy_fit = broom::tidy(model, conf.int = T)


      } else{
        model<-MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),
                             k=log(nrow(na.omit(data_boot))),trace =0, direction = direction)
        tidy_fit<- broom::tidy(model, conf.int = T)

      }
      res =residuals(model)

      aic_ignore_full <-  all_vars  %>%
        dplyr::select(term) %>%
        dplyr::left_join(tidy_fit, by = "term")

      data_boot <- droplevels(data_boot)
      x_dup<- as.data.frame(model.matrix(y ~., data_boot,check.names=FALSE))[,-1]

      return(get_uncertain_nulls (mod=aic_ignore_full, res=res, x=x_dup) %>%
               select(term, estimate, selected, p.value) %>%
               dplyr::mutate(   boot = b)
      )},cl=cl)

    parallel::stopCluster(cl)

    boot_fits  %>%
      dplyr:: bind_rows() %>%
      dplyr:: group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(estimate),4),
        conf.low = round(quantile(estimate, .025),4),
        conf.high = round(quantile(estimate, .975),4),
        ci_ln = round(conf.high-conf.low,4),
        prop.select = round(mean(selected==1),4),
        prop.rej= round(mean(p.value<0.05),4),
        #ci_avg_ln_boot= mean(ci_length_boot, na.rm=T)
      )


  }

}


#' Title
#'
#' @param x  dataframe with predictors
#' @param y outcome vector
#' @param B The number of bootstrap replicates.
#' @param family Currently only "gaussian" supported
#' @param nonselector A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param parallel whether to run bootstrap in parallel for faster computation
#' @param direction the mode of stepwise search, can be one of "both", "backward", or "forward",
#' with a default of "both". If the scope argument is missing the default for direction is "backward".
#' @param model Selected model from whole data set and stepwise BIC method
#' @param ... 	any additional arguments to that can be passed to stepAIC
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @importFrom forcats fct_inorder
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @return Tidy dataframe with bootstrap result and CIs
#' \item{term}{variable name}
#' \item{mean_estimate}{mean of regression coefficients across bootstrap samples}
#' \item{conf.low}{lower 2.5 percentile bootstrap interval}
#' \item{conf.high}{upper 97.5 percentile bootstrap interval}
#' \item{median_p.value}{median p value of regression coefficients  across bootstrap samples}
#' \item{ci_ln}{confidence interval length}
#' \item{prop.select}{propotion of times a given variable is selected by model selection method}
#' \item{prop.rej}{proportion of time coefficient was found significant at 0.05 alpha level}
#
#' @export
#'

boot_stepwise_bic <- function(x,y, B = 250,family="gaussian",nonselector="ignored", parallel= FALSE,
                              direction="both", model=model, ...) {

  data= data.frame(cbind(y,x))
  #fit_full <- broom::tidy(lm(y ~ ., data = data))
  #fit_bic<- tidy(model[["model_sum"]])

  non_zero_terms <- model[["beta"]][["term"]][! is.na(model[["beta"]][["estimate"]]) ] # Exclude the intercept
  selected_vars <- data.frame(term =   non_zero_terms)
  all_vars <- data.frame(term = model[["beta"]][["term"]])


  if ( nonselector=="ignored" & parallel == FALSE){
    boot_fits <- list(numeric(B))

    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        fit = broom::tidy(MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          k=log(nrow(na.omit(data_boot))),
          direction = "forward", trace = 0,...
        ), conf.int = T)


      } else{
        fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0, k=log(nrow(na.omit(data_boot))),
                                        direction = direction,...), conf.int = T)

      }
      #fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0, direction = "both"), conf.int = T)[-1,]

      boot_fits[[b]] <-     selected_vars   %>%
        select(term) %>%
        dplyr::left_join(  fit, by = "term") %>%
        dplyr::mutate(is.select = ifelse(is.na(estimate),0,1),
               p.value = ifelse(is.na(p.value), 1, p.value),
               estimate = ifelse(is.na(estimate), 0,  estimate),
               boot = b)
    }


    boot_fits  %>%
      dplyr:: bind_rows() %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      summarize(
        mean_estimate = round(mean(estimate, na.rm=T),4),
        conf.low = round(quantile(estimate, .025, na.rm=T),4),
        conf.high = round(quantile(estimate, .975, na.rm = T),4),
        median_p.value  = round(median(p.value, na.rm=T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select==1),4),
        prop.rej= round(mean(p.value<0.05, na.rm=T),4),
      )
  }
  else if(nonselector=="ignored"& parallel == TRUE){
    # do with parallel computing
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x", "y", "selected_vars","direction" ),envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
    })

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        fit = broom::tidy(MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          k=log(nrow(na.omit(data_boot))),
          direction = "forward", trace = 1,...
        ), conf.int = T)


      } else{
        fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0,
                                        k=log(nrow(na.omit(data_boot))),direction = direction,...), conf.int = T)

      }

      # Return results aligned with the full model
      return(
        selected_vars  %>%
          dplyr:: select(term) %>%
          dplyr::left_join(  fit, by = "term") %>%
          dplyr::mutate(  is.select = ifelse(is.na(estimate),0,1),
                   p.value = ifelse(is.na(p.value), 1, p.value),
                   estimate = ifelse(is.na(estimate), 0,  estimate),
                   boot = b)
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    boot_fits  %>%
      dplyr:: bind_rows() %>%
      dplyr::group_by(term = forcats::fct_inorder(term)) %>%
      summarize(
        mean_estimate = round(mean(estimate,na.rm=T),4),
        conf.low = round(quantile(estimate, .025,na.rm=T),4),
        conf.high = round(quantile(estimate, .975,na.rm=T),4),
        median_p.value  = round(median(p.value,na.rm=T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select==1),4),
        prop.rej= round(mean(p.value<0.05, na.rm=T),4),
      )
  }
  else if(nonselector=="confident_nulls" & parallel == FALSE){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]
      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        fit = broom::tidy(MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          k=log(nrow(na.omit(data_boot))),
          direction = "forward", trace = 0,...
        ), conf.int = T)


      } else{
        fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0,
                                        k=log(nrow(na.omit(data_boot))), direction = direction,...), conf.int = T)

      }

      boot_fits[[b]] <-   all_vars   %>%
        select(term) %>%
        dplyr::left_join(  fit, by = "term") %>%
        dplyr::mutate(is.select = ifelse(is.na(estimate),0,1),
               p.value = ifelse(is.na(p.value), 1, p.value),
               estimate = ifelse(is.na(estimate), 0,  estimate),
               boot = b)
    }


    boot_fits  %>%
      dplyr:: bind_rows() %>%
      group_by(term = forcats::fct_inorder(term)) %>%
      summarize(
        mean_estimate = round(mean(estimate),4),
        conf.low = round(quantile(estimate, .025),4),
        conf.high = round(quantile(estimate, .975),4),
        median_p.value  =  round(median(p.value),4),
        ci_ln = round(conf.high-conf.low,4),
        prop.select = round(mean(is.select==1),4),
        prop.rej= round(mean(p.value<0.05),4),
        #ci_avg_ln_boot= mean(ci_length_boot, na.rm=T)
      )


  }
  else if(nonselector=="confident_nulls"& parallel == TRUE){
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x", "y", "all_vars","direction"),envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
    })

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        fit = broom::tidy(MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          k=log(nrow(na.omit(data_boot))),
          direction = "forward", trace = 1,...
        ), conf.int = T)


      } else{
        fit<- broom::tidy(MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),  trace =0,
                                        k=log(nrow(na.omit(data_boot))),direction = direction,...), conf.int = T)

      }

      # Return results aligned with the full model
      return(
        all_vars %>%
          dplyr:: select(term) %>%
          dplyr::left_join(  fit, by = "term") %>%
          dplyr::mutate(  is.select = ifelse(is.na(estimate),0,1),
                   p.value = ifelse(is.na(p.value), 1, p.value),
                   estimate = ifelse(is.na(estimate), 0,  estimate),
                   boot = b)
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    boot_fits  %>%
      dplyr:: bind_rows() %>%
      group_by(term = forcats::fct_inorder(term)) %>%
      summarize(
        mean_estimate = round(mean(estimate,na.rm=T),4),
        conf.low = round(quantile(estimate, .025,na.rm=T),4),
        conf.high = round(quantile(estimate, .975,na.rm=T),4),
        median_p.value  = round(median(p.value,na.rm=T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select==1),4),
        prop.rej= round(mean(p.value<0.05, na.rm=T),4),
      )

  }
  else if(nonselector=="uncertain_nulls" & parallel == FALSE){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        model<- MASS::stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          k=log(nrow(na.omit(data_boot))),
          direction = "forward", trace = 0
        )

        tidy_fit = broom::tidy(model, conf.int = T)


      } else{
        model<-MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),
                             k=log(nrow(na.omit(data_boot))),trace =0, direction = direction)
        tidy_fit<- broom::tidy(model, conf.int = T)

      }
      res =residuals(model)

      bic_ignore_full <-  all_vars  %>%
        dplyr::select(term) %>%
        dplyr::left_join(tidy_fit, by = "term")

      x_dup<- as.data.frame(model.matrix(y ~., data_boot,check.names=FALSE))[,-1]

      boot_fits[[b]] <-  get_uncertain_nulls (mod=bic_ignore_full, res=res, x=x_dup) %>%
        select(term, estimate, selected, p.value) %>%
        dplyr::mutate(   boot = b)

    }


    boot_fits  %>%
      dplyr:: bind_rows() %>%
      dplyr:: group_by(term = forcats::fct_inorder(term)) %>%
      dplyr:: summarize(
        mean_estimate = round(mean(estimate),4),
        conf.low = round(quantile(estimate, .025),4),
        conf.high = round(quantile(estimate, .975),4),
        ci_ln = round(conf.high-conf.low,4),
        prop.select = round(mean(selected==1),4),
        prop.rej= round(mean(p.value<0.05),4),
        #ci_avg_ln_boot= mean(ci_length_boot, na.rm=T)
      )


  }
  else if(nonselector=="uncertain_nulls" & parallel == TRUE){

    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    parallel::clusterExport(cl, varlist = c("x", "y", "all_vars","direction" ),envir = environment())

    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(MASS)
    })


    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      if (direction == "forward") {
        # Clean variable names for forward selection
        clean_colnames <- ifelse(grepl("[^a-zA-Z0-9._]", colnames(x)),
                                 paste0("`", colnames(x), "`"),
                                 colnames(x))
        scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
        model<- stepAIC(
          lm(y ~ 1, data=na.omit(data_boot)),  # Start with an empty model
          scope = list(lower = ~1, upper = scope_formula),
          k=log(nrow(na.omit(data_boot))),
          direction = "forward", trace = 0
        )

        tidy_fit = broom::tidy(model, conf.int = T)


      } else{
        model<-MASS::stepAIC(lm(y ~ ., data=na.omit(data_boot)),
                             k=log(nrow(na.omit(data_boot))),trace =0, direction = direction)
        tidy_fit<- broom::tidy(model, conf.int = T)

      }
      res =residuals(model)

      bic_ignore_full <-  all_vars  %>%
        dplyr::select(term) %>%
        dplyr::left_join(tidy_fit, by = "term")

      x_dup<- as.data.frame(model.matrix(y ~., data_boot,check.names=FALSE))[,-1]

     return(get_uncertain_nulls (mod=bic_ignore_full, res=res, x=x_dup) %>%
        select(term, estimate, selected, p.value) %>%
          dplyr::mutate(   boot = b)
     )},cl=cl)

    parallel::stopCluster(cl)

    boot_fits  %>%
      dplyr:: bind_rows() %>%
      dplyr:: group_by(term = forcats::fct_inorder(term)) %>%
      dplyr:: summarize(
        mean_estimate = round(mean(estimate),4),
        conf.low = round(quantile(estimate, .025),4),
        conf.high = round(quantile(estimate, .975),4),
        ci_ln = round(conf.high-conf.low,4),
        prop.select = round(mean(selected==1),4),
        prop.rej= round(mean(p.value<0.05),4),
        #ci_avg_ln_boot= mean(ci_length_boot, na.rm=T)
      )


  }

}






#' Title
#'
#' @param model model of class selector_pen
#' @param B The number of bootstrap replicates.
#' @param family Currently only "gaussian" supported
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param parallel A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param ... any additional arguments to that can be passed to ncvreg
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix
#' @importFrom stats na.pass
#' @importFrom MASS stepAIC
#' @importFrom forcats fct_inorder
#' @importFrom ncvreg ncvreg
#' @return A tidy dataframe with bootstrap dataset
#' \item{term}{variable name}
#' \item{mean_estimate}{mean of regression coefficients across bootstrap samples}
#' \item{conf.low}{lower 2.5 percentile bootstrap interval}
#' \item{conf.high}{upper 97.5 percentile bootstrap interval}
#' \item{median_p.value}{median p value of regression coefficients  across bootstrap samples}
#' \item{ci_ln}{confidence interval length}
#' \item{prop.select}{proportion of times a given variable is selected by model selection method}
#' \item{prop.rej}{proportion of time coefficient was found significant at 0.05 alpha level}
#
#' @export
#'
#'

boot_pen <- function(model, B = 250,family="gaussian",nonselection="ignored",
                               parallel= FALSE, ...) {

  x <-model[["x"]]
  y <- model[["y"]]
  std=model[["std"]]
  lambda_full= model[["lambda.select"]]
  penalty = model[["penalty"]]
  alpha=model[["alpha"]]
  non_zero_terms <- model[["beta"]][["term"]][model[["beta"]][["estimate"]] !=0]
  #non_zero_terms <-  non_zero_terms [  non_zero_terms != "(Intercept)"]
  selected_vars <- data.frame(term =   non_zero_terms)
  all_vars <- data.frame(term = model[["beta"]][["term"]])

  if (nonselection=="ignored" & parallel == FALSE){
    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]
      fit_b  <- ncvreg::ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,family="gaussian",...)

      # Find the lambda value in fit_b$lambda closest to lambda_full
      closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      beta <- coef(fit_b ,lambda =  closest_lambda ) # select coeff  from lambda full
      beta <-beta[beta != 0] # 0 are for variable that are not selected
      # Convert beta to data frame
      beta_df <- data.frame(term = names(beta), beta = beta,row.names = NULL)
      beta_df$term <- gsub("`", "", beta_df$term)

      boot_fits[[b]] <- selected_vars %>% select(term) %>%
        dplyr::left_join(beta_df, by = "term")%>%
        mutate(is.select = ifelse(is.na(beta),0,1),
               beta = ifelse(is.na(beta), 0, beta),
               boot = b)
    }

    results= boot_fits  %>%
      dplyr:: bind_rows() %>%
      dplyr:: group_by(term = forcats::fct_inorder(term)) %>%
      dplyr:: summarize(
        mean_estimate = round(mean(beta, na.rm=T),4),
        conf.low = round(quantile(beta, .025, na.rm=T),4),
        conf.high = round(quantile(beta, .975, na.rm = T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select ),4),
      )
    return(results)
  }
  else if(nonselection=="ignored" & parallel == TRUE){

    # do with parallel computing
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x","y","alpha", "penalty", "lambda_full", "selected_vars"),
                            envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(ncvreg)
    })


    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]
      fit_b  <- ncvreg::ncvreg(X = x_boot, y = y_boot, penalty = penalty,alpha= alpha,family="gaussian",...)

      # Find the lambda value in fit_b$lambda closest to lambda_full
      closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      beta <- coef(fit_b ,lambda =  closest_lambda ) # select coeff  from lambda full
      beta <-beta[beta != 0]
      # Convert beta to data frame
      beta_df <- data.frame(term = names(beta), beta = beta,row.names = NULL)
      beta_df$term <- gsub("`", "", beta_df$term)

      # Return results aligned with the full model
      return(
        selected_vars %>% select(term) %>%
          dplyr::left_join(beta_df, by = "term")%>%
          mutate(is.select = ifelse(is.na(beta),0,1),
                 beta = ifelse(is.na(beta), 0, beta),
                 boot = b)
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)



   results=  boot_fits  %>%
     dplyr:: bind_rows() %>%
     dplyr:: group_by(term = forcats::fct_inorder(term)) %>%
       dplyr::summarize(
        mean_estimate = round(mean(beta, na.rm=T),4),
        conf.low = round(quantile(beta, .025, na.rm=T),4),
        conf.high = round(quantile(beta, .975, na.rm = T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select ),4)
      ) %>%
      mutate(     ci_avg_ratio = mean(ci_ln, na.rm=T),
                  ci_median_ratio = median(ci_ln, na.rm=T))

    return( results)

  }
  else if(nonselection=="confident_nulls" & parallel == FALSE){

    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]
      fit_b  <- ncvreg::ncvreg (X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,family="gaussian",...)

      # Find the lambda value in fit_b$lambda closest to lambda_full
      closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      beta <- coef(fit_b ,lambda =  closest_lambda ) # select coeff  from lambda full
      # Convert beta to data frame
      beta_df <- data.frame(term = names(beta), beta = beta,row.names = NULL)
      beta_df$term <- gsub("`", "", beta_df$term)

      boot_fits[[b]] <- all_vars  %>% select(term) %>%
        dplyr::left_join(beta_df, by = "term")%>%
        mutate(is.select = ifelse(beta == 0,0,1),
               beta = ifelse(is.na(beta), 0, beta),
               boot = b)
    }

    results= boot_fits  %>%
      dplyr:: bind_rows() %>%
      dplyr:: group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(beta, na.rm=T),4),
        conf.low = round(quantile(beta, .025, na.rm=T),4),
        conf.high = round(quantile(beta, .975, na.rm = T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select ),4),
      )
    return(results)


  }
  else if(nonselection=="confident_nulls" & parallel == TRUE){
    # do with parallel computing
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x","y","alpha", "penalty", "lambda_full", "all_vars"),
                            envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(ncvreg)
    })


    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]
      fit_b  <- ncvreg::ncvreg(X = x_boot, y = y_boot, penalty = penalty,alpha= alpha,family="gaussian",...)

      # Find the lambda value in fit_b$lambda closest to lambda_full
      closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      beta <- coef(fit_b ,lambda =  closest_lambda ) # select coeff  from lambda full
      # Convert beta to data frame
      beta_df <- data.frame(term = names(beta), beta = beta,row.names = NULL)
      beta_df$term <- gsub("`", "", beta_df$term)

      # Return results aligned with the full model
      return(
        all_vars  %>% select(term) %>%
          dplyr::left_join(beta_df, by = "term")%>%
          mutate(is.select = ifelse(beta == 0,0,1),
                 beta = ifelse(is.na(beta), 0, beta),
                 boot = b)
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)



    results=  boot_fits  %>%
      dplyr:: bind_rows() %>%
      group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(beta, na.rm=T),4),
        conf.low = round(quantile(beta, .025, na.rm=T),4),
        conf.high = round(quantile(beta, .975, na.rm = T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select ),4)
      )
    return( results)


  }
  else if(nonselection=="uncertain_nulls" & parallel == FALSE){
    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]
      fit_b  <- ncvreg::ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,family="gaussian")

      # Find the lambda value in fit_b$lambda closest to lambda_full
      closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      beta <- coef(fit_b ,lambda =  closest_lambda ) # select coeff  from lambda full
      non_zero_terms  <- names(beta[beta != 0])
      non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
      non_zero_terms  <-  gsub("`", "",non_zero_terms )


      selected_data <-  data.frame(y = y_boot, x_boot, check.names = FALSE) %>%
        select(all_of(non_zero_terms))

      xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta
      res <- y - xbeta

      fit_lso <- broom::tidy(lm(y ~ ., data = selected_data), conf.int = TRUE) %>%mutate(
        term = gsub("`", "", term),  # Remove backticks
      )

      lso_ignore_mod <- data.frame(term= all_vars) %>%
        select(term) %>% dplyr::left_join(fit_lso, by = "term")

      boot_fits[[b]] <- get_uncertain_nulls (mod= lso_ignore_mod, res=res, x=x_boot) %>%
        select(term, estimate) %>%
        mutate(is.select = ifelse(beta == 0,0,1),
               boot = b)
    }

    results= boot_fits  %>%
      dplyr:: bind_rows() %>%
      group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(estimate, na.rm=T),4),
        conf.low = round(quantile(estimate, .025, na.rm=T),4),
        conf.high = round(quantile(estimate, .975, na.rm = T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select ),4),
      )
    return(results)
  }
  else if(nonselection=="uncertain_nulls" & parallel == TRUE){
    # do with parallel computing
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x","y","alpha", "penalty", "lambda_full"),
                            envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(ncvreg)
    })

    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]
      fit_b  <- ncvreg::ncvreg(X = x_boot, y = y_boot, penalty = penalty,alpha= alpha,family="gaussian",...)

      # Find the lambda value in fit_b$lambda closest to lambda_full
      closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      beta <- coef(fit_b ,lambda =  closest_lambda ) # select coeff  from lambda full
      non_zero_terms  <- names(beta[beta != 0])
      non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
      non_zero_terms  <-  gsub("`", "",non_zero_terms )

      selected_data <-  data.frame(y = y_boot, x_boot, check.names = FALSE) %>%
        select(all_of(non_zero_terms))

      xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta
      res <- y - xbeta

      fit_lso <- broom::tidy(lm(y ~ ., data = selected_data), conf.int = TRUE) %>%mutate(
        term = gsub("`", "", term),  # Remove backticks
      )

      lso_ignore_mod <- data.frame(term= all_vars) %>%
        select(term) %>% dplyr::left_join(fit_lso, by = "term")



      # Return results aligned with the full model
      return(
        get_uncertain_nulls (mod= lso_ignore_mod, res=res, x=x_boot) %>%
          select(term, estimate) %>%
          mutate(is.select = ifelse(beta == 0,0,1),
                 boot = b)
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    results= boot_fits  %>%
      dplyr:: bind_rows() %>%
      group_by(term = forcats::fct_inorder(term)) %>%
      dplyr::summarize(
        mean_estimate = round(mean(estimate, na.rm=T),4),
        conf.low = round(quantile(estimate, .025, na.rm=T),4),
        conf.high = round(quantile(estimate, .975, na.rm = T),4),
        ci_ln = round(conf.high - conf.low,4),
        prop.select = round(mean(is.select ),4),
      )
    return(results)

  }
}







