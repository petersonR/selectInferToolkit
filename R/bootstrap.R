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

  if (n_cores == 1){

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
