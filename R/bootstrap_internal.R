
#' Internal function for bootstrapping
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize bind_rows
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @importFrom forcats fct_inorder
boot_stepwise <- function(x,y,family, penalty, std, B, nonselector, direction, make_levels, save_beta,  ...) {

  if ( nonselector=="ignored"){
    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std=std,
                        direction=direction, penalty = penalty,
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
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else{
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_ic"
    return(results)

  }

  if(nonselector=="confident_nulls"){


    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std = std,
                        direction=direction, penalty = penalty,
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
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else{
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_ic"
    return(results)

  }
  if(nonselector=="uncertain_nulls"){

    boot_fits <- list(numeric(B))
    for(b in 1:B) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std = std,
                        direction=direction, penalty = penalty,
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
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else{
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_ic"
    return(results)

  }

}

#' Internal function for bootstrapping
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize bind_rows
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @importFrom forcats fct_inorder
boot_stepwise_parallel <- function(x, y, penalty, B, family, n_cores, nonselector, direction, make_levels, save_beta, ...) {

  # do with parallel computing, Number of cores to use
  cl <- parallel::makeCluster(n_cores)

  # Export required variables to each worker
  parallel::clusterExport(cl, varlist = c("data", "selected_terms","direction","make_levels" ),envir = environment())

  # Ensure required packages are loaded in each worker
  parallel::clusterEvalQ(cl, {
    library(broom)
    library(dplyr)
    library(selectInferToolkit)
  })

  if(nonselector=="ignored"){

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std = std,
                        direction=direction, penalty = penalty,
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
      results <- list(B=B, beta= boot_results_df, results = final_results)
    } else{
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_ic"
    return(results)

  }

  if(nonselector=="confident_nulls"){

    # Parallel bootstrap operation
    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std = std,
                        direction=direction, penalty = penalty,
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
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else{
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_ic"
    return(results)
  }

  if(nonselector=="uncertain_nulls"){

    boot_fits <- pbapply::pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(data), replace = TRUE)
      data_boot <- data[boot_idx,]

      boot_ic = step_ic(x= data_boot[, colnames(data_boot) != "y"],
                        y=data_boot[, "y", drop = FALSE],std = std,
                        direction=direction, penalty = penalty,
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
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else{
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_ic"
    return(results)
  }
}

#' Internal function for bootstrapping penalized model
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize bind_rows
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @importFrom forcats fct_inorder
boot_pen <- function(x, y, B, family, std, nonselection, parallel,
                     lambda_selected,  lmax, all_terms, penalty, alpha,
                     selected_terms, lambda_seq, nlambda, boot_desparse,
                     save_beta) {

  boot_fits <- list(numeric(B))

  if (nonselection=="ignored"){
    if(!boot_desparse) { # If you want to keep sparse coefficients within bootstrap
      # If lasso, use glmnet
      if (alpha==1 & penalty=="lasso"){
        for(b in 1:B) {
          boot_idx <- sample(1:nrow(x), replace = TRUE)
          x_boot <- x[boot_idx,]
          y_boot<- y[boot_idx]

          fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha,
                                  standardize = F, family = family,
                                  lambda=lambda_seq, nlambda= nlambda)

          bb <- coef(fit_b, s= round(lambda_selected,3))
          beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
          beta_df <-beta[beta$estimate != 0, ]
          beta_df$term <- gsub("`", "", beta_df$term)

          # Match estimates to selected terms
          match_idx <- match(selected_terms, beta_df$term)
          estimates <-   beta_df$estimate[match_idx]

          # Replace NAs with 0 for estimate and 0/1 for is.select
          is.select <- as.integer(!is.na(estimates))
          estimates[is.na(estimates)] <- 0

          # Find the lambda value in fit_b$lambda closest to lambda_selected
          # closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_selected))]

          boot_fits[[b]] <-  data.frame(
            term = selected_terms,
            estimate = estimates,
            is.select = is.select,
            boot = b,
            stringsAsFactors = FALSE
          )

        }
      } else { # Otherwise use ncvreg
        for(b in 1:B) {
          boot_idx <- sample(1:nrow(x), replace = TRUE)
          x_boot <- x[boot_idx,]
          y_boot<- y[boot_idx]

          fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,family = family,
                          lambda=lambda_seq, nlambda= nlambda)

          beta <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
          beta <-beta[beta != 0] # 0 are for variable that are not selected
          beta_df <- data.frame(term = names(beta), estimate = beta,row.names = NULL)
          beta_df$term <- gsub("`", "", beta_df$term)

          # Match estimates to selected terms
          match_idx <- match(selected_terms, beta_df$term)
          estimates <-   beta_df$estimate[match_idx]

          # Replace NAs with 0 for estimate and 0/1 for is.select
          is.select <- as.integer(!is.na(estimates))
          estimates[is.na(estimates)] <- 0

          # Match estimates to selected terms
          match_idx <- match(selected_terms, beta_df$term)
          estimates <-   beta_df$estimate[match_idx]

          # Replace NAs with 0 for estimate and 0/1 for is.select
          is.select <- as.integer(!is.na(estimates))
          estimates[is.na(estimates)] <- 0

          # Find the lambda value in fit_b$lambda closest to lambda_selected
          # closest_lambda <- fit_b$lambda[which.min(abs(fit_b$lambda - lambda_selected))]

          boot_fits[[b]] <-  data.frame(
            term = selected_terms,
            estimate = estimates,
            is.select = is.select,
            boot = b,
            stringsAsFactors = FALSE
          )

        }
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

      if (save_beta){
        results <- list(B=B, beta= boot_results_df, results = full_mod)
      } else {
        results <- list(B=B, results = full_mod)
      }

      class(results) <- "boot_selector_pen"
    } else { # If you want to desparsify coefficients within bootstrap

      # Get boot_fits using bootstrapping
      if (alpha==1 & penalty=="lasso"){
        for(b in 1:B) {
          boot_idx <- sample(1:nrow(x), replace = TRUE)
          x_boot <- x[boot_idx,]
          y_boot<- y[boot_idx]

          fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F, family = family,
                                  lambda=lambda_seq, nlambda= nlambda)

          bb <- coef(fit_b, s= round(lambda_selected,3))
          beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
          non_zero_terms <-beta$term[beta$estimate != 0]
          non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
          non_zero_terms  <-  gsub("`", "",non_zero_terms )

          selected_data <-  data.frame(y_boot = y_boot,
                                       data.frame(x_boot,check.names = FALSE),check.names = FALSE)
          if(length(non_zero_terms) !=0 ){
            selected_data <- selected_data[, c("y_boot", non_zero_terms)]
          } else{
            selected_data <- data.frame("y_boot"= y_boot)
          }

          fit <- lm(y_boot ~ ., data = selected_data)
          coefs <- data.frame(coef(summary(fit)), check.names = F)
          coefs$term <-gsub("`", "", rownames(coefs))
          rownames(coefs) <- NULL
          coefs$is.select <-1

          # Match estimates to selected terms
          match_idx <- match(selected_terms, coefs $term )
          estimates <-   coefs $Estimate[match_idx]
          is.select <- coefs$is.select[match_idx]

          # Replace NAs with 0 for estimate and 0/1 for is.select
          is.select <- ifelse(is.na(is.select),0, is.select)
          estimates[is.na(estimates)] <- 0


          boot_fits[[b]] <-  data.frame(
            term = selected_terms,
            estimate = estimates,
            is.select = is.select,
            boot = b,
            stringsAsFactors = FALSE
          )
        }
      } else {
        for(b in 1:B) {
          boot_idx <- sample(1:nrow(x), replace = TRUE)
          x_boot <- x[boot_idx,]
          y_boot<- y[boot_idx]

          fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,family = family,
                          lambda=lambda_seq, nlambda= nlambda)
          beta <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
          non_zero_terms  <- names(beta[beta != 0])
          non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
          non_zero_terms  <-  gsub("`", "",non_zero_terms )

          selected_data <-  data.frame(y_boot = y_boot,
                                       data.frame(x_boot,check.names = FALSE),check.names = FALSE)
          if(length(non_zero_terms) !=0 ){
            selected_data <- selected_data[, c("y_boot", non_zero_terms)]
          } else{
            selected_data <- data.frame("y_boot"= y_boot)
          }

          fit <- lm(y_boot ~ ., data = selected_data)
          coefs <- data.frame(coef(summary(fit)), check.names = F)
          coefs$term <-gsub("`", "", rownames(coefs))
          rownames(coefs) <- NULL
          coefs$is.select <-1

          # Match estimates to selected terms
          match_idx <- match(selected_terms, coefs $term )
          estimates <-   coefs $Estimate[match_idx]
          is.select <- coefs$is.select[match_idx]

          # Replace NAs with 0 for estimate and 0/1 for is.select
          is.select <- ifelse(is.na(is.select),0, is.select)
          estimates[is.na(estimates)] <- 0

          boot_fits[[b]] <-  data.frame(
            term = selected_terms,
            estimate = estimates,
            is.select = is.select,
            boot = b,
            stringsAsFactors = FALSE
          )
        }
      }

      # Process boot_fits
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

      if (save_beta){
        results <- list(B=B, beta= boot_results_df, results = full_mod)
      }else {
        results <- list(B=B, results = full_mod)
      }

      class(results) <- "boot_selector_pen"
    }
    return(results)
  }

  if(nonselection=="confident_nulls" & !boot_desparse){
    if (alpha==1 & penalty=="lasso") {
      for(b in 1:B) {
        boot_idx <- sample(1:nrow(x), replace = TRUE)
        x_boot <- x[boot_idx,]
        y_boot<- y[boot_idx]

        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq, nlambda= nlambda,family = family)
        bb <- coef(fit_b, s= lambda_selected)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        beta_df <-beta[beta$estimate != 0, ]
        beta_df$term <- gsub("`", "", beta_df$term)

        # Merge all_vars and beta_df by "term"
        full_mod <-merge(all_terms,  beta_df, by = "term", all.x = TRUE, sort = FALSE)
        matched_rows <- match(all_terms$term, full_mod$term)
        full_mod <- full_mod[matched_rows, ]
        full_mod$is.select = ifelse(is.na(full_mod$estimate), 0, 1)
        full_mod$estimate = ifelse(is.na(full_mod$estimate), 0, full_mod$estimate)
        full_mod$boot= b

        boot_fits[[b]] <- full_mod
      }
    } else {
      for(b in 1:B) {
        boot_idx <- sample(1:nrow(x), replace = TRUE)
        x_boot <- x[boot_idx,]
        y_boot<- y[boot_idx]

        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lambda_seq, nlambda= nlambda,family = family)
        beta <- coef(fit_b ,lambda =  lambda_selected) # select coeff  from lambda full
        beta <-beta[beta != 0] # 0 are for variable that are not selected
        # Convert beta to data frame
        beta_df <- data.frame(term = names(beta), estimate = beta,row.names = NULL)
        beta_df$term <- gsub("`", "", beta_df$term)

        # Merge all_vars and beta_df by "term"
        full_mod <-merge(all_terms,  beta_df, by = "term", all.x = TRUE, sort = FALSE)
        matched_rows <- match(all_terms$term, full_mod$term)
        full_mod <- full_mod[matched_rows, ]
        full_mod$is.select = ifelse(is.na(full_mod$estimate), 0, 1)
        full_mod$estimate = ifelse(is.na(full_mod$estimate), 0, full_mod$estimate)
        full_mod$boot= b

        boot_fits[[b]] <- full_mod
      }
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


    if (save_beta){
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else {
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_pen"
    return(results)
  }
  if(nonselection=="confident_nulls" & boot_desparse){
    if (alpha==1 & penalty=="lasso"){
      for(b in 1:B) {
        boot_idx <- sample(1:nrow(x), replace = TRUE)
        x_boot <- x[boot_idx,]
        y_boot<- y[boot_idx]

        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F, family = family,
                                lambda=lambda_seq, nlambda= nlambda)
        bb <- coef(fit_b, s= round(lambda_selected,3))
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )

        selected_data <-  data.frame(y_boot = y_boot,
                                     data.frame(x_boot,check.names = FALSE),check.names = FALSE)

        if(length(non_zero_terms) !=0 ){
          selected_data <- selected_data[, c("y_boot", non_zero_terms)]
        } else{
          selected_data <- data.frame("y_boot"= y_boot)
        }


        fit <- lm(y_boot ~ ., data = selected_data)
        coefs <- data.frame(coef(summary(fit)), check.names = F)
        coefs$term <-gsub("`", "", rownames(coefs))
        rownames(coefs) <- NULL
        coefs$is.select <-1

        # Match estimates to selected terms
        match_idx <- match(all_terms$term, coefs $term )
        estimates <-   coefs $Estimate[match_idx]
        is.select <- coefs$is.select[match_idx]
        # Replace NAs with 0 for estimate and 0/1 for is.select
        is.select <- ifelse(is.na(is.select),0, is.select)
        estimates[is.na(estimates)] <- 0


        boot_fits[[b]] <-  data.frame(
          term = all_terms$term,
          estimate = estimates,
          is.select = is.select,
          boot = b,
          stringsAsFactors = FALSE
        )

      }
    } else{
      for(b in 1:B) {
        boot_idx <- sample(1:nrow(x), replace = TRUE)
        x_boot <- x[boot_idx,]
        y_boot<- y[boot_idx]

        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,family = family,
                        lambda=lambda_seq, nlambda= nlambda)
        beta <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
        non_zero_terms  <- names(beta[beta != 0])
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )

        selected_data <-  data.frame(y_boot = y_boot,
                                     data.frame(x_boot,check.names = FALSE),check.names = FALSE)

        if(length(non_zero_terms) !=0 ){
          selected_data <- selected_data[, c("y_boot", non_zero_terms)]
        } else{
          selected_data <- data.frame("y_boot"= y_boot)
        }


        fit <- lm(y_boot ~ ., data = selected_data)
        coefs <- data.frame(coef(summary(fit)), check.names = F)
        coefs$term <-gsub("`", "", rownames(coefs))
        rownames(coefs) <- NULL
        coefs$is.select <-1

        # Match estimates to selected terms
        match_idx <- match(all_terms$term, coefs $term )
        estimates <-   coefs $Estimate[match_idx]
        is.select <- coefs$is.select[match_idx]
        # Replace NAs with 0 for estimate and 0/1 for is.select
        is.select <- ifelse(is.na(is.select),0, is.select)
        estimates[is.na(estimates)] <- 0


        boot_fits[[b]] <-  data.frame(
          term = all_terms$term,
          estimate = estimates,
          is.select = is.select,
          boot = b,
          stringsAsFactors = FALSE
        )

      }
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
    match_idx <- match(all_terms$term, boot_summary$term)   # Match terms and align boot_results to all_vars
    boot_summary <-  boot_summary[match_idx,]


    if (save_beta){
      results <- list(B=B, beta= boot_results_df, results = boot_summary)
    }else {
      results <- list(B=B, results = boot_summary)
    }

    class(results) <- "boot_selector_pen"
    return(results)


  }
  if(nonselection=="uncertain_nulls" & !boot_desparse){
    if (alpha==1 & penalty=="lasso"){
      for(b in 1:B) {
        boot_idx <- sample(1:nrow(x), replace = TRUE)
        x_boot <- x[boot_idx,]
        y_boot<- y[boot_idx]

        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq, nlambda= nlambda,family = family)
        bb <- coef(fit_b, s=  lambda_selected)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )


        xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta$estimate
        res <- y_boot - xbeta
        selected_data <-  data.frame(y_boot = y_boot,
                                     data.frame(x_boot,check.names = FALSE),check.names = FALSE)

        if(length(non_zero_terms) !=0 ){
          selected_data <- selected_data[, c("y_boot", non_zero_terms)]
        } else{
          selected_data <- data.frame("y_boot"= y_boot)
        }

        full_mod <- data.frame(
          term = beta$term[beta$estimate != 0],
          estimate = beta$estimate[beta$estimate != 0],
          std.error = NA,
          p.value = NA,
          conf.low = NA,
          conf.high = NA,
          stringsAsFactors = FALSE
        )

        rownames(full_mod) <- NULL
        full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
        matched_rows <- match(all_terms$term, full_mod$term)
        full_mod <- full_mod[matched_rows, ]
        full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
        full_mod<- full_mod [, c("term", "estimate", "std.error", "p.value","conf.low", "conf.high")]

        un_results = get_uncertain_nulls(mod= full_mod, res=res,
                                         x= data.frame( x_boot, check.names = FALSE) )
        un_results$boot= b

        boot_fits[[b]] <-    un_results
      }
    } else{
      for(b in 1:B) {
        boot_idx <- sample(1:nrow(x), replace = TRUE)
        x_boot <- x[boot_idx,]
        y_boot<- y[boot_idx]

        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lambda_seq, nlambda= nlambda, family = family)
        bb <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
        beta <- data.frame(term = names(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )

        xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta$estimate
        res <- y_boot - xbeta
        selected_data <-  data.frame(y_boot = y_boot,
                                     data.frame(x_boot,check.names = FALSE),check.names = FALSE)

        if(length(non_zero_terms) !=0 ){
          selected_data <- selected_data[, c("y_boot", non_zero_terms)]
        } else{
          selected_data <- data.frame("y_boot"= y_boot)
        }

        full_mod <- data.frame(
          term = beta$term[beta$estimate != 0],
          estimate = beta$estimate[beta$estimate != 0],
          std.error = NA,
          p.value = NA,
          conf.low = NA,
          conf.high = NA,
          stringsAsFactors = FALSE
        )

        rownames(full_mod) <- NULL
        full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
        matched_rows <- match(all_terms$term, full_mod$term)
        full_mod <- full_mod[matched_rows, ]
        full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
        full_mod<- full_mod [, c("term", "estimate", "std.error", "p.value","conf.low", "conf.high")]

        un_results = get_uncertain_nulls(mod= full_mod, res=res,
                                         x= data.frame( x_boot, check.names = FALSE) )
        un_results$boot= b

        boot_fits[[b]] <-    un_results
      }
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

    if (save_beta){
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else {
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_pen"
    return(results)
  }
  if(nonselection=="uncertain_nulls" & boot_desparse){
    if (alpha==1 & penalty=="lasso"){
      for(b in 1:B) {
        boot_idx <- sample(1:nrow(x), replace = TRUE)
        x_boot <- x[boot_idx,]
        y_boot<- y[boot_idx]

        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq, nlambda= nlambda,family = family)
        bb <- coef(fit_b, s=  lambda_selected)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )


        selected_data <-  data.frame(y_boot = y_boot,
                                     data.frame(x_boot,check.names = FALSE),check.names = FALSE)

        if(length(non_zero_terms) !=0 ){
          selected_data <- selected_data[, c("y_boot", non_zero_terms)]
        } else{
          selected_data <- data.frame("y_boot"= y_boot)
        }

        fit <- lm(y_boot ~ ., data = selected_data)
        coefs <- coef(summary(fit))
        terms <- gsub("`", "", rownames(coefs))
        full_mod <- data.frame(
          term = terms,
          estimate = coefs[,1],
          std.error = NA,
          p.value = NA,
          conf.low = NA,
          conf.high = NA,
          stringsAsFactors = FALSE
        )
        rownames(full_mod) <- NULL

        all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
        full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
        matched_rows <- match(all_terms$term, full_mod$term)
        full_mod <- full_mod[matched_rows, ]
        full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
        full_mod<- full_mod [, c("term", "estimate", "std.error", "p.value","conf.low", "conf.high")]
        estimates <- full_mod$estimate
        estimates[is.na(estimates)] <- 0
        xbeta<- as.matrix(cbind("(Intercept)"=1, x_boot)) %*% estimates
        res <- y_boot - xbeta

        un_results = get_uncertain_nulls (mod= full_mod, res=res,
                                          x= data.frame( x_boot, check.names = FALSE) )
        un_results$boot= b

        boot_fits[[b]] <-    un_results
      }

    } else{
      for(b in 1:B) {
        boot_idx <- sample(1:nrow(x), replace = TRUE)
        x_boot <- x[boot_idx,]
        y_boot<- y[boot_idx]

        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lambda_seq, nlambda= nlambda, family = family)
        bb <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
        beta <- data.frame(term = names(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )

        selected_data <-  data.frame(y_boot = y_boot,
                                     data.frame(x_boot,check.names = FALSE),check.names = FALSE)

        if(length(non_zero_terms) !=0 ){
          selected_data <- selected_data[, c("y_boot", non_zero_terms)]
        } else{
          selected_data <- data.frame("y_boot"= y_boot)
        }

        fit <- lm(y_boot ~ ., data = selected_data)
        coefs <- coef(summary(fit))
        terms <- gsub("`", "", rownames(coefs))
        full_mod <- data.frame(
          term = terms,
          estimate = coefs[,1],
          std.error = NA,
          p.value = NA,
          conf.low = NA,
          conf.high = NA,
          stringsAsFactors = FALSE
        )
        rownames(full_mod) <- NULL

        all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
        full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
        matched_rows <- match(all_terms$term, full_mod$term)
        full_mod <- full_mod[matched_rows, ]
        full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
        full_mod<- full_mod [, c("term", "estimate", "std.error", "p.value","conf.low", "conf.high")]
        estimates <- full_mod$estimate
        estimates[is.na(estimates)] <- 0
        xbeta<- as.matrix(cbind("(Intercept)"=1, x_boot)) %*% estimates
        res <- y_boot - xbeta

        un_results = get_uncertain_nulls (mod= full_mod, res=res,
                                          x= data.frame( x_boot, check.names = FALSE) )
        un_results$boot= b

        boot_fits[[b]] <-    un_results
      }
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

    if (save_beta){
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else {
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_pen"
    return(results)

  }
}

#' Internal function for bootstrapping penalized model in parallel
#' Currently this could be made more efficient.
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize bind_rows
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @importFrom forcats fct_inorder
boot_pen_parallel <- function(x, y, B, family, std, nonselection, n_cores, parallel,
                              lambda_selected, nlambda,
                              all_terms, lmax, penalty, alpha,
                              selected_terms, lambda_seq, boot_desparse,
                              save_beta) {
  cl <- parallel::makeCluster(n_cores)

  # Export required variables to each worker
  parallel::clusterExport(cl, varlist = c("x","y","alpha", "penalty", "lambda_selected",
                                          "lambda_seq", "nlambda", "selected_terms"),
                          envir = environment())

  # Ensure required packages are loaded in each worker
  parallel::clusterEvalQ(cl, {
    library(broom)
    library(dplyr)
    library(ncvreg)
    library(glmnet)
  })

  # Code below could be made more effient, as in the non-parallel case
  if(nonselection=="ignored"  & !boot_desparse){

    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq,nlambda= nlambda, family = family)
      }else{
        fit_b <- ncvreg::ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                                lambda=lambda_seq,nlambda= nlambda,family=family)

      }


      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s= round(lambda_selected,3))
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
        beta <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
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

    full_mod <-merge(all_terms, boot_summary, by = "term", all.x = TRUE, sort = FALSE)
    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])


    if (save_beta== T){
      results <- list(B=B, beta= boot_results_df, results = full_mod)
    }else {
      results <- list(B=B, results = full_mod)
    }

    class(results) <- "boot_selector_pen"
    return(results)


  }
  if(nonselection=="ignored"  & boot_desparse){


    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq,nlambda= nlambda, family = family)
      }else{
        fit_b <- ncvreg::ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                                lambda=lambda_seq,nlambda= nlambda, family = family)

      }

      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s= round(lambda_selected,3))
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )

      }else{
        beta <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
        non_zero_terms  <- names(beta[beta != 0])
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }

      selected_data <-  data.frame(y_boot = y_boot,
                                   data.frame(x_boot,check.names = FALSE),check.names = FALSE)

      if(length(non_zero_terms) !=0 ){
        selected_data <- selected_data[, c("y_boot", non_zero_terms)]
      }
      else{
        selected_data <- data.frame("y_boot"= y_boot)
      }


      fit <- lm(y_boot ~ ., data = selected_data)
      coefs <- data.frame(coef(summary(fit)), check.names = F)
      coefs$term <-gsub("`", "", rownames(coefs))
      rownames(coefs) <- NULL
      coefs$is.select <-1

      # Match estimates to selected terms
      match_idx <- match(selected_terms, coefs $term )
      estimates <-   coefs $Estimate[match_idx]
      is.select <- coefs$is.select[match_idx]
      is.select <- ifelse(is.na(is.select),0, is.select)
      estimates[is.na(estimates)] <- 0

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


    full_mod <-merge(all_terms, boot_summary, by = "term", all.x = TRUE, sort = FALSE)
    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])


    if (save_beta== T){
      results <- list(B=B, beta= boot_results_df, results = full_mod)
    }else {
      results <- list(B=B, results = full_mod)
    }

    class(results) <- "boot_selector_pen"
    return(results)

  }
  if(nonselection=="confident_nulls" & !boot_desparse){
    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq, nlambda= nlambda,family = family)
      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lambda_seq, nlambda= nlambda,family = family)

      }


      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s= lambda_selected)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        beta_df <-beta[beta$estimate != 0, ]
        beta_df$term <- gsub("`", "", beta_df$term)
      }else{
        beta <- coef(fit_b ,lambda =  lambda_selected) # select coeff  from lambda full
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

    if (save_beta){
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else {
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_pen"
    return(results)

  }
  if(nonselection=="confident_nulls" & boot_desparse){
    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq, nlambda= nlambda,family = family)
      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lambda_seq, nlambda= nlambda,family = family)

      }


      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s= round(lambda_selected,3))
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )

      }else{
        beta <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
        non_zero_terms  <- names(beta[beta != 0])
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )

      }

      selected_data <-  data.frame(y_boot = y_boot,
                                   data.frame(x_boot,check.names = FALSE),check.names = FALSE)

      if(length(non_zero_terms) !=0 ){
        selected_data <- selected_data[, c("y_boot", non_zero_terms)]
      }
      else{
        selected_data <- data.frame("y_boot"= y_boot)
      }

      fit <- lm(y_boot ~ ., data = selected_data)
      coefs <- data.frame(coef(summary(fit)), check.names = F)
      coefs$term <-gsub("`", "", rownames(coefs))
      rownames(coefs) <- NULL
      coefs$is.select <-1

      # Match estimates to selected terms
      match_idx <- match(all_terms$term, coefs $term )
      estimates <-   coefs $Estimate[match_idx]
      is.select <- coefs$is.select[match_idx]
      # Replace NAs with 0 for estimate and 0/1 for is.select
      is.select <- ifelse(is.na(is.select),0, is.select)
      estimates[is.na(estimates)] <- 0

      full_mod =data.frame(
        term = all_terms$term,
        estimate = estimates,
        is.select = is.select,
        boot = b,
        stringsAsFactors = FALSE
      )


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
    match_idx <- match(all_terms$term, boot_summary$term)
    boot_summary <-  boot_summary[match_idx,]


    if (save_beta){
      results <- list(B=B, beta= boot_results_df, results = boot_summary)
    }else {
      results <- list(B=B, results = boot_summary)
    }

    class(results) <- "boot_selector_pen"
    return(results)
  }
  if(nonselection=="uncertain_nulls" & !boot_desparse){
    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq,nlambda= nlambda,family = family)
      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lambda_seq,nlambda= nlambda,family = family)
      }


      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s=  lambda_selected)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }else{
        bb <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
        beta <- data.frame(term = names(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }


      xbeta<- as.matrix(cbind("(Intercept)"=1,x_boot)) %*% beta$estimate
      res <- y_boot - xbeta
      selected_data <-  data.frame(y_boot = y_boot,
                                   data.frame(x_boot,check.names = FALSE),check.names = FALSE)

      if(length(non_zero_terms) !=0 ){
        selected_data <- selected_data[, c("y_boot", non_zero_terms)]
      }
      else{
        selected_data <- data.frame("y_boot"= y_boot)
      }


      full_mod <- data.frame(
        term = beta$term[beta$estimate != 0],
        estimate = beta$estimate[beta$estimate != 0],
        std.error = NA,
        p.value = NA,
        conf.low = NA,
        conf.high = NA,
        stringsAsFactors = FALSE
      )
      rownames(full_mod) <- NULL

      full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      full_mod<- full_mod [, c("term", "estimate", "std.error", "p.value","conf.low", "conf.high")]

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

    if (save_beta){
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else {
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_pen"
    return(results)
  }
  if(nonselection=="uncertain_nulls" & boot_desparse){
    boot_fits <- pblapply(1:B, function(b) {
      boot_idx <- sample(1:nrow(x), replace = TRUE)
      x_boot <- x[boot_idx,]
      y_boot<- y[boot_idx]

      if (alpha==1 & penalty=="lasso"){
        fit_b <- glmnet::glmnet(x = x_boot, y = y_boot, alpha = alpha, standardize = F,
                                lambda=lambda_seq,nlambda= nlambda,family = family)
      }else{
        fit_b <- ncvreg(X = x_boot, y = y_boot,  alpha= alpha, penalty = penalty,
                        lambda=lambda_seq,nlambda= nlambda,family = family)
      }


      if (alpha==1 & penalty=="lasso"){
        bb <- coef(fit_b, s=  lambda_selected)
        beta <- data.frame(term = rownames(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }else{
        bb <- coef(fit_b ,lambda =  lambda_selected ) # select coeff  from lambda full
        beta <- data.frame(term = names(bb), estimate = as.vector(bb))
        non_zero_terms <-beta$term[beta$estimate != 0]
        non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
        non_zero_terms  <-  gsub("`", "",non_zero_terms )
      }

      selected_data <-  data.frame(y_boot = y_boot,
                                   data.frame(x_boot,check.names = FALSE),check.names = FALSE)

      if(length(non_zero_terms) !=0 ){
        selected_data <- selected_data[, c("y_boot", non_zero_terms)]
      }
      else{
        selected_data <- data.frame("y_boot"= y_boot)
      }


      fit <- lm(y_boot ~ ., data = selected_data)
      coefs <- coef(summary(fit))
      terms <- gsub("`", "", rownames(coefs))
      full_mod <- data.frame(
        term = terms,
        estimate = coefs[,1],
        std.error = NA,
        p.value = NA,
        conf.low = NA,
        conf.high = NA,
        stringsAsFactors = FALSE
      )
      rownames(full_mod) <- NULL

      full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      full_mod<- full_mod [, c("term", "estimate", "std.error", "p.value","conf.low", "conf.high")]
      estimates <- full_mod$estimate
      estimates[is.na(estimates)] <- 0
      xbeta<- as.matrix(cbind("(Intercept)"=1, x_boot)) %*% estimates
      res <- y_boot - xbeta

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

    if (save_beta){
      results <- list(B=B, beta= boot_results_df, results = final_results)
    }else {
      results <- list(B=B, results = final_results)
    }

    class(results) <- "boot_selector_pen"
    return(results)
  }

}
