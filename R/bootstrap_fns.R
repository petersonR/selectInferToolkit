
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

full_boot <- function(model, B = 250,family="gaussian",parallel = FALSE, std =F,save_beta= F, ...) {
  if (is.null(model[["y"]]) |is.null(model[["x"]])){
    print("Please rerun lm function with options x= TRUE and y=TRUE to save data to run bootstrap")
  }else{

  }
  x=model[["x"]]
  x<- x[, colnames(x) != "(Intercept)"]
  y <- model[["y"]]

  # Fit the full model first to get the coefficient names
  all_terms <- data.frame(term = colnames(model[["x"]]), stringsAsFactors = FALSE)

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
      fit <- lm(y_boot ~ x_boot)
      fits <- summary(fit)$coefficients
      fits_df <- data.frame(
        term = c("(Intercept)", colnames(x)),
        estimate = fits[, "Estimate"],
        stringsAsFactors = FALSE
      )
      matched_rows <- match(all_terms$term, fits_df$term)

      boot_fits[[b]] <- data.frame(
        term = all_terms$term,
        estimate = fits_df$estimate[matched_rows],
        boot = b,
        stringsAsFactors = FALSE
      )


    }


    boot_results_df <- do.call(rbind, boot_fits)
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      row.names = NULL
    )
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms$term, boot_summary$term)


    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      stringsAsFactors = FALSE
    )


  }
  # do with parallel computing
  else{
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x", "y", "all_terms", "B"), envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
    })


    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_id <- sample(seq_len(nrow(x)),replace = TRUE)
      y_boot <- y[boot_id]
      x_boot <- x[boot_id, , drop = FALSE ]
      # Fit bootstrap model and retrieve coefficients
      fit <- lm(y_boot ~ x_boot)
      fits <- summary(fit)$coefficients
      fits_df <- data.frame(
        term = c("(Intercept)", colnames(x)),
        estimate = fits[, "Estimate"],
        stringsAsFactors = FALSE
      )
      matched_rows <- match(all_terms$term, fits_df$term)


      # Return results aligned with the full model
      return(
        data.frame(
          term = all_terms$term,
          estimate = fits_df$estimate[matched_rows],
          boot = b,
          stringsAsFactors = FALSE
        )
      )
    }, cl = cl)

    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    boot_results_df <- do.call(rbind, boot_fits)
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      row.names = NULL
    )
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms$term, boot_summary$term)


    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      stringsAsFactors = FALSE
    )


  }


  if (save_beta== T){
    return(list( beta= boot_results_df, results = final_results))
  } else{
    return(final_results)
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
#' @return Ddataframe with bootstrap result and CIs
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
                              direction="both", model=model,make_levels=F, save_beta =F,  ...) {

  data= data.frame(cbind(y,x),check.names = F)
  selected_terms  <- model[["beta"]][["term"]][! is.na(model[["beta"]][["estimate"]]) ]
  all_terms <- model[["beta"]][["term"]]



  if ( nonselector=="ignored" & parallel == FALSE){
    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "AIC",
                        make_levels = make_levels, ...)
      fit <- boot_ic[["beta"]]
      fit$term <-  gsub("`", "", fit$term)

      # Match estimates to selected terms
      match_idx <- match(selected_terms, fit$term )
      estimates <-   fit $estimate[match_idx]

      # Replace NAs with 0 for estimate and 0/1 for is.select
      is.select <- as.integer(!is.na(estimates))
      estimates[is.na(estimates)] <- 0

      boot_fits[[b]] <- data.frame(
        term = selected_terms,
        estimate = estimates,
        is.select = is.select,
        boot = b,
        stringsAsFactors = FALSE
      )
    }

    boot_results_df <- do.call(rbind, boot_fits)
    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )


    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )

    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselector=="ignored"& parallel == TRUE){
    # do with parallel computing, Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("data", "selected_terms","direction","make_levels" ),envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(practicalPSI)
    })

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "AIC",
                        make_levels = make_levels, ...)
      fit <- boot_ic[["beta"]]
      fit$term <-  gsub("`", "", fit$term)

      # Match estimates to selected terms
      match_idx <- match(selected_terms, fit$term )
      estimates <-   fit $estimate[match_idx]

      # Replace NAs with 0 for estimate and 0/1 for is.select
      is.select <- as.integer(!is.na(estimates))
      estimates[is.na(estimates)] <- 0



      # Return results aligned with the full model
      return(
        data.frame(
          term = selected_terms,
          estimate = estimates,
          is.select = is.select,
          boot = b,
          stringsAsFactors = FALSE
        )
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )

    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )

    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselector=="confident_nulls" & parallel == FALSE){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "AIC",
                        make_levels = make_levels, ...)
      fit <- boot_ic[["beta"]]
      fit$term <-  gsub("`", "", fit$term)
      fit$is.select <- ifelse(is.na(fit$estimate),0,1)
      fit$estimate <- ifelse(is.na(fit$estimate), 0,  fit$estimate)
      fit$boot <- b

      boot_fits[[b]] <- fit
    }

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

     # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )

    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )

    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselector=="confident_nulls"& parallel == TRUE){
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("data", "all_terms","direction", "make_levels" ),envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
    })

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "AIC",
                        make_levels = make_levels, ...)

      fit <- boot_ic[["beta"]]
      fit$term <-  gsub("`", "", fit$term)
      fit$is.select <- ifelse(is.na(fit$estimate),0,1)
      fit$estimate <- ifelse(is.na(fit$estimate), 0,  fit$estimate)
      fit$boot <- b



      # Return results aligned with the full model
      return(fit)

    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )
    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )
    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselector=="uncertain_nulls" & parallel == FALSE){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "AIC",
                        make_levels = make_levels, ...)

      coefs <- summary(boot_ic[["model_sum"]])$coefficients
      terms <- gsub("`", "", rownames(coefs))

      full_mod <- data.frame(
        term = terms,
        estimate = coefs[, "Estimate"],
        std.error = coefs[,"Std. Error"],
        p.value = coefs[, "Pr(>|t|)"],
        conf.low = NA,
        conf.high = NA,
        stringsAsFactors = FALSE
      )
      rownames(full_mod) <- NULL


      all_terms <- data.frame(term = boot_ic[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      res =residuals(boot_ic[["model_sum"]])

      x <-boot_ic[["x_model"]]
      x_dup<- as.data.frame(model.matrix(y ~., model.frame(~ ., cbind(x,y=boot_ic[["y"]]), na.action=na.pass))[,-1],
                            check.names=FALSE)

      final_mod= get_uncertain_nulls (mod=full_mod, res=res, x=x_dup)
      final_mod <- final_mod[, c("term", "estimate", "selected", "p.value")]
      final_mod$boot <- b

      boot_fits[[b]] <-   final_mod
    }

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$selected))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )

    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms$term, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms$term,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )
    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselector=="uncertain_nulls" & parallel == TRUE){
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    parallel::clusterExport(cl, varlist = c("data", "all_terms","direction","make_levels"  ),envir = environment())

    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(MASS)
    })


    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "AIC",
                        make_levels = make_levels, ...)

      coefs <- summary(boot_ic[["model_sum"]])$coefficients
      terms <- gsub("`", "", rownames(coefs))

      full_mod <- data.frame(
        term = terms,
        estimate = coefs[, "Estimate"],
        std.error = coefs[,"Std. Error"],
        p.value = coefs[, "Pr(>|t|)"],
        conf.low = NA,
        conf.high = NA,
        stringsAsFactors = FALSE
      )
      rownames(full_mod) <- NULL


      all_terms <- data.frame(term = boot_ic[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      res =residuals(boot_ic[["model_sum"]])

      x <-boot_ic[["x_model"]]
      x_dup<- as.data.frame(model.matrix(y ~., model.frame(~ ., cbind(x,y=boot_ic[["y"]]), na.action=na.pass))[,-1],
                            check.names=FALSE)

      final_mod= get_uncertain_nulls (mod=full_mod, res=res, x=x_dup)
      final_mod <- final_mod[, c("term", "estimate", "selected", "p.value")]
      final_mod$boot <- b



      return( final_mod )},cl=cl)

    parallel::stopCluster(cl)

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$selected))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )
    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )
    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }
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
#' @importFrom dplyr mutate_if select mutate summarize group_by bind_rows
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @importFrom forcats fct_inorder
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @return Dataframe with bootstrap result and CIs
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
                              direction="both", model=model,make_levels=F,save_beta=F,   ...) {

  data= data.frame(cbind(y,x),check.names = F)
  selected_terms  <- model[["beta"]][["term"]][! is.na(model[["beta"]][["estimate"]]) ]
  all_terms <- model[["beta"]][["term"]]



  if ( nonselector=="ignored" & parallel == FALSE){
    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "BIC",
                        make_levels = make_levels, ...)
      fit <- boot_ic[["beta"]]
      fit$term <-  gsub("`", "", fit$term)

      # Match estimates to selected terms
      match_idx <- match(selected_terms, fit$term )
      estimates <-   fit $estimate[match_idx]

      # Replace NAs with 0 for estimate and 0/1 for is.select
      is.select <- as.integer(!is.na(estimates))
      estimates[is.na(estimates)] <- 0

      boot_fits[[b]] <- data.frame(
        term = selected_terms,
        estimate = estimates,
        is.select = is.select,
        boot = b,
        stringsAsFactors = FALSE
      )
    }

    boot_results_df <- do.call(rbind, boot_fits)
    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )


    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )

    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }



  }
  else if(nonselector=="ignored"& parallel == TRUE){
    # do with parallel computing, Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("data", "selected_terms","direction","make_levels" ),envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(practicalPSI)
    })

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "BIC",
                        make_levels = make_levels, ...)
      fit <- boot_ic[["beta"]]
      fit$term <-  gsub("`", "", fit$term)

      # Match estimates to selected terms
      match_idx <- match(selected_terms, fit$term )
      estimates <-   fit $estimate[match_idx]

      # Replace NAs with 0 for estimate and 0/1 for is.select
      is.select <- as.integer(!is.na(estimates))
      estimates[is.na(estimates)] <- 0



      # Return results aligned with the full model
      return(
        data.frame(
          term = selected_terms,
          estimate = estimates,
          is.select = is.select,
          boot = b,
          stringsAsFactors = FALSE
        )
      )
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )

    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )

    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }
  }
  else if(nonselector=="confident_nulls" & parallel == FALSE){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "BIC",
                        make_levels = make_levels, ...)
      fit <- boot_ic[["beta"]]
      fit$term <-  gsub("`", "", fit$term)
      fit$is.select <- ifelse(is.na(fit$estimate),0,1)
      fit$estimate <- ifelse(is.na(fit$estimate), 0,  fit$estimate)
      fit$boot <- b

      boot_fits[[b]] <- fit
    }

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )

    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )


    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselector=="confident_nulls"& parallel == TRUE){
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("data", "all_terms","direction", "make_levels" ),envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
    })

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "BIC",
                        make_levels = make_levels, ...)

      fit <- boot_ic[["beta"]]
      fit$term <-  gsub("`", "", fit$term)
      fit$is.select <- ifelse(is.na(fit$estimate),0,1)
      fit$estimate <- ifelse(is.na(fit$estimate), 0,  fit$estimate)
      fit$boot <- b



      # Return results aligned with the full model
      return(fit)

    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )
    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )
    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselector=="uncertain_nulls" & parallel == FALSE){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "BIC",
                        make_levels = make_levels, ...)

      coefs <- summary(boot_ic[["model_sum"]])$coefficients
      terms <- gsub("`", "", rownames(coefs))

      full_mod <- data.frame(
        term = terms,
        estimate = coefs[, "Estimate"],
        std.error = coefs[,"Std. Error"],
        p.value = coefs[, "Pr(>|t|)"],
        conf.low = NA,
        conf.high = NA,
        stringsAsFactors = FALSE
      )
      rownames(full_mod) <- NULL


      all_terms <- data.frame(term = boot_ic[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      res =residuals(boot_ic[["model_sum"]])

      x <-boot_ic[["x_model"]]
      x_dup<- as.data.frame(model.matrix(y ~., model.frame(~ ., cbind(x,y=boot_ic[["y"]]), na.action=na.pass))[,-1],
                            check.names=FALSE)

      final_mod= get_uncertain_nulls (mod=full_mod, res=res, x=x_dup)
      final_mod <- final_mod[, c("term", "estimate", "selected", "p.value")]
      final_mod$boot <- b

      boot_fits[[b]] <-   final_mod
    }

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$selected))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )

    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms$term, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms$term,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )
    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselector=="uncertain_nulls" & parallel == TRUE){
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    parallel::clusterExport(cl, varlist = c("data", "all_terms","direction","make_levels"  ),envir = environment())

    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(MASS)
    })


    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=F,
                        direction=direction, penalty = "BIC",
                        make_levels = make_levels, ...)

      coefs <- summary(boot_ic[["model_sum"]])$coefficients
      terms <- gsub("`", "", rownames(coefs))

      full_mod <- data.frame(
        term = terms,
        estimate = coefs[, "Estimate"],
        std.error = coefs[,"Std. Error"],
        p.value = coefs[, "Pr(>|t|)"],
        conf.low = NA,
        conf.high = NA,
        stringsAsFactors = FALSE
      )
      rownames(full_mod) <- NULL


      all_terms <- data.frame(term = boot_ic[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      res =residuals(boot_ic[["model_sum"]])

      x <-boot_ic[["x_model"]]
      x_dup<- as.data.frame(model.matrix(y ~., model.frame(~ ., cbind(x,y=boot_ic[["y"]]), na.action=na.pass))[,-1],
                            check.names=FALSE)

      final_mod= get_uncertain_nulls (mod=full_mod, res=res, x=x_dup)
      final_mod <- final_mod[, c("term", "estimate", "selected", "p.value")]
      final_mod$boot <- b



      return( final_mod )},cl=cl)

    parallel::stopCluster(cl)

    # Combine all into one data.frame
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$selected))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )
    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )
    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }
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
#' @importFrom dplyr mutate_if select mutate summarize group_by
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix
#' @importFrom stats na.pass
#' @importFrom MASS stepAIC
#' @importFrom forcats fct_inorder
#' @importFrom ncvreg ncvreg
#' @importFrom glmnet glmnet
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
                               parallel= FALSE, save_beta=F,   ...) {

  x <-model[["x"]]
  y <- model[["y"]]
  std=model[["std"]]
  lambda_full= model[["lambda.select"]]
  lmax= model[["lmax"]]
  penalty = model[["penalty"]]
  alpha=model[["alpha"]]
  selected_terms <- model[["beta"]][["term"]][model[["beta"]][["estimate"]] !=0]
  #non_zero_terms <-  non_zero_terms [  non_zero_terms != "(Intercept)"]
  #selected_vars <- data.frame(term =   non_zero_terms)
  all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
  lam_seq <- exp(seq(log(lmax), log(lambda_full), len=10))

  if (nonselection=="ignored" & parallel == FALSE){
    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]


      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F, family = "gaussian",
                                lambda=lam_seq, ...)
      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,family="gaussian",
                        lambda=lam_seq,  ...)

      }

      # Find the lambda value in fit_b$lambda closest to lambda_full
     # closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s=  lambda_full)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        beta_df <-beta[beta$estimate != 0, ]
        beta_df$term <- gsub("`", "", beta_df$term)

        # Match estimates to selected terms
        match_idx <- match(selected_terms, beta_df$term)
        estimates <-   beta_df$estimate[match_idx]

        # Replace NAs with 0 for estimate and 0/1 for is.select
        is.select <- as.integer(!is.na(estimates))
        estimates[is.na(estimates)] <- 0


      }else{
        beta <- coef(fit_b ,lambda =  lambda_full ) # select coeff  from lambda full
        beta <-beta[beta != 0] # 0 are for variable that are not selected
        # Convert beta to data frame
        #print(beta)
        beta_df <- data.frame(term = names(beta), estimate = beta,row.names = NULL)
        beta_df$term <- gsub("`", "", beta_df$term)
        # Match estimates to selected terms
        match_idx <- match(selected_terms, beta_df$term)
        estimates <-   beta_df$estimate[match_idx]

        # Replace NAs with 0 for estimate and 0/1 for is.select
        is.select <- as.integer(!is.na(estimates))
        estimates[is.na(estimates)] <- 0

      }

      boot_fits[[b]] <-  data.frame(
        term = selected_terms,
        estimate = estimates,
        is.select = is.select,
        boot = b,
        stringsAsFactors = FALSE
      )

    }
    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)

    # Compute summary statistics
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )


    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    full_mod <-merge(all_terms, boot_summary, by = "term", all.x = TRUE, sort = FALSE)
    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])

    if (save_beta== T){
      return(list( beta= boot_results_df, results = full_mod))
    } else{
      return(full_mod)
    }

  }
  else if(nonselection=="ignored" & parallel == TRUE){

    # do with parallel computing
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x","y","alpha", "penalty", "lambda_full",
                                           "lam_seq", "selected_terms"),
                            envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(ncvreg)
      library(glmnet)
    })


    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lam_seq, family = "gaussian", ...)
      }else{
        fit_b <- ncvreg::ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                                lambda=lam_seq,family="gaussian",...)

      }

      # Find the lambda value in fit_b$lambda closest to lambda_full
      #closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s=  lambda_full)

        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        beta_df <-beta[beta$estimate != 0, ]
        beta_df$term <- gsub("`", "", beta_df$term)
        match_idx <- match(selected_terms, beta_df$term)
        estimates <-   beta_df$estimate[match_idx]
        is.select <- as.integer(!is.na(estimates))
        estimates[is.na(estimates)] <- 0
      }else{
        beta <- coef(fit_b ,lambda =  lambda_full ) # select coeff  from lambda full
        beta <-beta[beta != 0] # 0 are for variable that are not selected
        # Convert beta to data frame
        beta_df <- data.frame(term = names(beta), estimate = beta,row.names = NULL)
        beta_df$term <- gsub("`", "", beta_df$term)
        match_idx <- match(selected_terms, beta_df$term)
        estimates <-   beta_df$estimate[match_idx]
        is.select <- as.integer(!is.na(estimates))
        estimates[is.na(estimates)] <- 0
      }

      # Return results aligned with the full model
      return(
        data.frame(
          term = selected_terms,
          estimate = estimates,
          is.select = is.select,
          boot = b,
          stringsAsFactors = FALSE
        ))

    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    boot_results_df <- do.call(rbind, boot_fits)
    term_split <- split(boot_results_df, boot_results_df$term)

    # Compute summary statistics
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    full_mod <-merge(all_terms, boot_summary, by = "term", all.x = TRUE, sort = FALSE)
    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])


    if (save_beta== T){
      return(list( beta= boot_results_df, results = full_mod))
    } else{
      return(full_mod)
    }


  }
  else if(nonselection=="confident_nulls" & parallel == FALSE){

    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lam_seq, family = "gaussian", ...)
      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lam_seq, family="gaussian",...)

      }

      # Find the lambda value in fit_b$lambda closest to lambda_full
      #closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s= lambda_full)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        beta_df <-beta[beta$estimate != 0, ]
        beta_df$term <- gsub("`", "", beta_df$term)
      }else{
        beta <- coef(fit_b ,lambda =  lambda_full) # select coeff  from lambda full
        beta <-beta[beta != 0] # 0 are for variable that are not selected
        # Convert beta to data frame
        beta_df <- data.frame(term = names(beta), estimate = beta,row.names = NULL)
        beta_df$term <- gsub("`", "", beta_df$term)

      }

      # Merge all_vars and beta_df by "term"
      full_mod <-merge(all_terms,  beta_df, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod$is.select = ifelse(is.na(full_mod$estimate), 0, 1)
      full_mod$estimate = ifelse(is.na(full_mod$estimate), 0, full_mod$estimate)
      full_mod$boot= b

      boot_fits[[b]] <- full_mod
    }

    boot_results_df <- do.call(rbind, boot_fits)
    term_split <- split(boot_results_df, boot_results_df$term)

    # Compute summary statistics
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )


    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms$term, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )


    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }


  }
  else if(nonselection=="confident_nulls" & parallel == TRUE){
    # do with parallel computing
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x","y","alpha", "penalty", "lambda_full",
                                            "lam_seq","all_terms"),
                            envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(broom)
      library(dplyr)
      library(ncvreg)
      library(glmnet)
    })


    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lam_seq, family = "gaussian", ...)
      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lam_seq, family="gaussian",...)

      }

      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s=  lambda_full)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        beta_df <-beta[beta$estimate != 0, ]
        beta_df$term <- gsub("`", "", beta_df$term)
      }else{
        beta <- coef(fit_b ,lambda =  lambda_full) # select coeff  from lambda full
        beta <-beta[beta != 0] # 0 are for variable that are not selected
        # Convert beta to data frame
        beta_df <- data.frame(term = names(beta), estimate = beta,row.names = NULL)
        beta_df$term <- gsub("`", "", beta_df$term)

      }
      # Merge all_vars and beta_df by "term"
      full_mod <-merge(all_terms,  beta_df, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod$is.select = ifelse(is.na(full_mod$estimate), 0, 1)
      full_mod$estimate = ifelse(is.na(full_mod$estimate), 0, full_mod$estimate)


      # Return results aligned with the full model
      return(full_mod)
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)



    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)

    # Compute summary statistics
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$is.select))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )


    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms$term, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )

    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }

  }
  else if(nonselection=="uncertain_nulls" & parallel == FALSE){
    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lam_seq, family = "gaussian", ...)

      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lam_seq,  family="gaussian",...)
      }

      # Find the lambda value in fit_b$lambda closest to lambda_full
     # closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]

      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s=  lambda_full)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }else{
        beta <- coef(fit_b ,lambda =  lambda_full ) # select coeff  from lambda full
        non_zero_terms  <- names(beta[beta != 0])
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }


      if (alpha==1 & penalty=="lasso"){
        xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta$estimate
        res <- y_boot - xbeta

      }else{
        xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta
        res <- y_boot - xbeta
      }

      selected_data <-  data.frame(y_boot = y_boot,
                                   data.frame(x_boot,check.names = FALSE),check.names = FALSE)
      selected_data <- selected_data[, c("y_boot", non_zero_terms)]

      fit <- lm(y_boot ~ ., data = selected_data)
      conf <- confint(fit)
      coefs <- coef(summary(fit))
      terms <- gsub("`", "", rownames(coefs))

      full_mod <- data.frame(
        term = terms,
        estimate = coefs[, "Estimate"],
        std.error = coefs[,"Std. Error"],
        p.value = coefs[, "Pr(>|t|)"],
        conf.low = conf[, 1],
        conf.high = conf[, 2],
        stringsAsFactors = FALSE
      )
      rownames(full_mod) <- NULL
      all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])


      un_results = get_uncertain_nulls (mod= full_mod, res=res,
                                        x= data.frame( x_boot, check.names = FALSE) )
      un_results$boot= b

      boot_fits[[b]] <-    un_results
    }

    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)

    # Compute summary statistics
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$selected))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )


    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms$term, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )

    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }
  }
  else if(nonselection=="uncertain_nulls" & parallel == TRUE){
    # do with parallel computing
    # Number of cores to use
    n_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(n_cores)

    # Export required variables to each worker
    parallel::clusterExport(cl, varlist = c("x","y","alpha", "penalty", "lambda_full",
                                         "lam_seq",  "all_terms"),
                            envir = environment())

    # Ensure required packages are loaded in each worker
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(ncvreg)
      library(glmnet)
    })

    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lam_seq,family = "gaussian", ...)
      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lam_seq,family="gaussian",...)
      }

      # Find the lambda value in fit_b$lambda closest to lambda_full
     # closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_full))]


      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s=  lambda_full)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }else{
        beta <- coef(fit_b ,lambda =   lambda_full) # select coeff  from lambda full
        non_zero_terms  <- names(beta[beta != 0])
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }

      if (alpha==1 & penalty=="lasso"){
        xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta$estimate
        res <- y_boot - xbeta

      }else{
        xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta
        res <- y_boot - xbeta
      }

      selected_data <-  data.frame(y_boot = y_boot,
                                   data.frame(x_boot,check.names = FALSE),check.names = FALSE)
      selected_data <- selected_data[, c("y_boot", non_zero_terms)]

      fit <- lm(y_boot ~ ., data = selected_data)
      conf <- confint(fit)
      coefs <- coef(summary(fit))
      terms <- gsub("`", "", rownames(coefs))
      full_mod <- data.frame(
        term = terms,
        estimate = coefs[, "Estimate"],
        std.error = coefs[,"Std. Error"],
        p.value = coefs[, "Pr(>|t|)"],
        conf.low = conf[, 1],
        conf.high = conf[, 2],
        stringsAsFactors = FALSE
      )

      rownames(full_mod) <- NULL
      all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod$is.select = ifelse(is.na(full_mod$estimate), 0, 1)
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])


      un_results = get_uncertain_nulls (mod= full_mod, res=res,
                                        x= data.frame( x_boot, check.names = FALSE) )
      un_results$boot= b

      # Return results aligned with the full model
      return( un_results)
    }, cl = cl)
    # Stop the cluster after computation is done
    parallel::stopCluster(cl)

    boot_results_df <- do.call(rbind, boot_fits)

    # Split by term
    term_split <- split(boot_results_df, boot_results_df$term)

    # Compute summary statistics
    mean_estimate <- sapply(term_split, function(x) mean(x$estimate))
    conf.low <- sapply(term_split, function(x) quantile(x$estimate, 0.025))
    conf.high <- sapply(term_split, function(x) quantile(x$estimate, 0.975))
    prop.select <- sapply(term_split, function(x) mean(x$selected))

    # Combine into a data frame
    boot_summary <- data.frame(
      term = names(mean_estimate),
      mean_estimate = mean_estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      prop.select = prop.select,
      row.names = NULL
    )


    # Add CI width
    boot_summary$ci_ln <- boot_summary$conf.high - boot_summary$conf.low
    rownames(boot_summary)<- NULL

    # Match terms and align boot_results to all_vars
    match_idx <- match(all_terms$term, boot_summary$term)

    # Create output with NA where terms weren't selected
    final_results <- data.frame(
      term = all_terms,
      mean_estimate =  boot_summary$mean_estimate[match_idx],
      conf.low =  boot_summary$conf.low[match_idx],
      conf.high =  boot_summary$conf.high[match_idx],
      ci_ln =  boot_summary$ci_ln[match_idx],
      prop.select =  boot_summary$prop.select[match_idx],
      stringsAsFactors = FALSE
    )

    if (save_beta== T){
      return(list( beta= boot_results_df, results = final_results))
    } else{
      return(final_results)
    }
  }
}







