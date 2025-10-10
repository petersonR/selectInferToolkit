#' Bootstrapping model selection methods (case-wise)
#'
#' @param model Selected model from whole data set and stepwise AIC method
#' @param B The number of bootstrap replicates.
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param n_cores whether to run bootstrap in parallel for faster computation
#' @param boot_desparse for penalized methods, should within-bootstrap coefficients be refit?
#' @param make_levels ?
#' @param save_beta ?
#' @param ... 	any additional arguments to that can be passed to stepAIC
#
#' @export
boot <- function(model, B = 250,
                 nonselection=c("ignored", "uncertain_nulls", "confident_nulls"),
                 n_cores = 1,
                 boot_desparse=FALSE,
                 make_levels=FALSE,
                 save_beta =FALSE,  ...) {

  family <- model$family
  nonselection <- match.arg(nonselection)
  penalty <- model$penalty
  direction <- model$direction
  n_cores <- min(n_cores, parallel::detectCores() - 1)

  UseMethod("boot")
}

#' Title
#'
#' @param model  lm object with design matrix and output variable
#' @param B The number of bootstrap replicates.
#' @param family currently only "gaussian" supported
#' @param n_cores whether to run bootstrap in parallel for faster computation
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

boot.full_model <- function(model, B = 250,family = family,n_cores = 1, std =F,save_beta= F, ...) {
  if (is.null(model[["y"]]) |is.null(model[["x"]])){
    print("Please rerun lm function with options x= TRUE and y=TRUE to save data to run bootstrap")
  }else{

  }
  x=model[["x"]]
  x<- x[, colnames(x) != "(Intercept)"]
  y <- model[["y"]]

  # Fit the full model first to get the coefficient names
  all_terms <- data.frame(term = colnames(model[["x"]]), stringsAsFactors = FALSE)

  if (n_cores > 0){

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

#' Bootstrapping for selection process with stepwise AIC/BIC
#'
#' @param model Selected model from whole data set and stepwise AIC method
#' @param B The number of bootstrap replicates.
#' @param nonselector A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param n_cores whether to run bootstrap in parallel for faster computation
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
#' @rdname boot
#' @export

boot.selector_ic <- function(model, B = 250,
                             nonselection="ignored",
                             n_cores = 1,
                             make_levels=FALSE, save_beta = FALSE,  ...) {

  penalty <- model$penalty
  family <- model$family
  x <-model[["x"]]
  y <- model[["y"]]
  std=model[["std"]]
  data= data.frame(cbind(y,x),check.names = F)
  selected_terms  <- model[["beta"]][["term"]][! is.na(model[["beta"]][["estimate"]]) ]
  all_terms <- model[["beta"]][["term"]]
  direction <- model$direction

  if(n_cores > 0) {
    results <- boot_stepwise_parallel(x = x, y = y, family = family,
                                      std = std, n_cores = n_cores,
                                      B = B, direction = direction,
                                      nonselection=nonselection,
                                      penalty = penalty,
                                      save_beta = save_beta,
                                      make_levels = make_levels)
  } else {
    results <-  boot_stepwise(x = x, y = y, family = family, std = std,
                              B = B, direction = direction,
                              nonselection=nonselection, penalty = penalty,
                              save_beta = save_beta,
                              make_levels = make_levels)
  }

  results
}


#' Bootstrapping for penalized CV-based model
#'
#'
#' @param model model of class selector_pen
#' @param B The number of bootstrap replicates.
#' @param family Currently only "gaussian" supported
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param boot_desparse TBD
#' @param parallel should a cluster with detectCores()-1 be made and used
#' @param ... any additional arguments to that can be passed to ncvreg
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize group_by
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix
#' @importFrom stats na.pass
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

boot.selector_pen <- function(model, B = 250, nonselection="ignored",
                              parallel= FALSE, boot_desparse=FALSE,
                              save_beta = FALSE, ...) {

  x <-model[["x"]]
  y <- model[["y"]]
  family <- model$family

  std=model[["std"]]
  lambda_selected= model[["lambda.select"]]
  lmax= model[["lmax"]]
  penalty = model[["penalty"]]
  alpha=model[["alpha"]]
  selected_terms <- model[["beta"]][["term"]][model[["beta"]][["estimate"]] !=0]
  lambda_seq= model[["lambda_seq"]]

  #non_zero_terms <-  non_zero_terms [  non_zero_terms != "(Intercept)"]
  #selected_vars <- data.frame(term =   non_zero_terms)

  all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
  if (lmax==lambda_selected) {
    lambda_seq <- exp(seq(log(lmax), min( lambda_seq), len=10))
    nlambda <-10
  } else {
    lambda_seq <- exp(seq(log(lmax), log(lambda_selected), len=10))
    nlambda<-10
  }

  if(parallel) {
    results <- boot_pen_parallel(x=x, y=y, family = family, std=std, B = B,
                                 lambda_selected = lambda_selected,  lmax = lmax,
                                 penalty = penalty, alpha = alpha,
                                 selected_terms = selected_terms,
                                 n_cores = n_cores,
                                 lambda_seq = lambda_seq, nlambda = nlambda,
                                 all_terms = all_terms,
                                 boot_desparse = boot_desparse,
                                 save_beta = save_beta,
                                 nonselection = nonselection)
  } else {
    results <- boot_pen(x=x, y=y, family = family, std=std, B = B,
                        lambda_selected = lambda_selected,  lmax = lmax,
                        penalty = penalty, alpha = alpha,
                        selected_terms = selected_terms,
                        lambda_seq = lambda_seq, nlambda = nlambda,
                        all_terms = all_terms,
                        boot_desparse = boot_desparse,
                        save_beta = save_beta,
                        nonselection = nonselection)
  }

  results
}

