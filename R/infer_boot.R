#' Inference for `selector` via bootstrapping
#'
#' @param model model returned from selector_stepwise_ic  class
#' @param method A character string specifying method of post-selection inference.Currently "hybrid"or  "selectiveinf"
#' @param nonselection  A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param conf.level .95 by default
#' @return infer s3 class with things like...
#' \item{selector}{the selector used to fit the model}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{medain CI length across all variables in model}
#' \item{infmethod}{Inference method}
#' \item{nonselection}{method chosen  to deal with non selection}
#' @importFrom dplyr right_join group_by summarize bind_rows
#' @rdname boot
#'
#' @export

infer_boot <- function(
  object,
  data,
  type = c("paired", "residual"),
  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
  conf.level = .95,
  B = 250,
  n_cores = 4,
  ...) {

  # A bit of argument checking
  type <- match.arg(type)
  stopifnot(type == "paired")
  nonselection <- match.arg(nonselection)
  n_cores <- min(n_cores, parallel::detectCores() - 2)

  result <- boot(object, data=data, B=B, nonselection=nonselection,
                 n_cores= n_cores, conf.level = conf.level, ...)

  class(result) <- "infer"
  result
}


#' Bootstrapping for selection process with stepwise AIC/BIC
#'
#' @param object a `selector` object
#' @param B The number of bootstrap replicates.
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param uncertain_insample within a bootstrap, should in-sample or
#'  out-of-sample residuals be used for estimation (defaults to FALSE)
#' @param conf.level .95 by default
#' @param n_cores number of cores for parallel computation
#' @param ... 	any additional arguments to that can be passed to fitting engine
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize bind_rows
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @importFrom forcats fct_inorder
#'
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

boot <- function(object, data, B, nonselection, uncertain_insample = FALSE,
                 conf.level, n_cores, ...) {
  stopifnot(inherits(object, "selector"))

  rec_obj <- attr(object, "recipe_obj")

  # Uncomment after debug
  # # do with parallel computing, Number of cores to use
  # cl <- parallel::makeCluster(n_cores)
  #
  # # Export required variables to each worker
  # parallel::clusterExport(cl, varlist = c("object", "data", "nonselection"), envir = environment())
  #
  # # Ensure required packages are loaded in each worker
  # parallel::clusterEvalQ(cl, {
  #   library(broom)
  #   library(dplyr)
  #   library(selectInferToolkit)
  # })

  # boot_fits <- pbapply::pblapply(1:B, function(b) { uncomment after debug
  boot_fits <- lapply(1:B, function(b) {
    boot_idx <- sample(1:nrow(data), replace = TRUE)
    data_boot <- data[boot_idx,]

    sel_boot = reselect(object, newdata = data_boot)

    coefs_boot <- tidy(sel_boot)

    # compute uncertain nulls, if needed
    nulls <- which(is.na(coefs_boot$estimate))
    if(length(nulls) > 0) {
      if(uncertain_insample) {
        new_data <- data_boot
      } else {
        new_data <- data[-boot_idx,]
      }

      p_hat <- predict(sel_boot, new_data)
      X_boot <- bake(attr(object, "recipe_obj"), new_data = new_data, all_predictors())
      y_boot <- bake(attr(object, "recipe_obj"), new_data = new_data, all_outcomes())

      r <- ((y_boot[[1]] - p_hat) ^ 2)
      coefs_boot$estimate_uncertain <- coefs_boot$estimate

      for(j in 1:length(nulls)) {
        x_j_boot <- X_boot[,coefs_boot$term[nulls[j]]][[1]]
        model_in <- lm(r ~ x_j_boot)
        coefs_boot$estimate_uncertain[nulls[j]] <- model_in$coef[2]
      }
    }

    coefs_boot
    }
    # ,cl=cl uncomment after debug
    )

  # parallel::stopCluster(cl) # uncomment after debug
  boot_results_df <- bind_rows(boot_fits, .id = "bootstrap")

  if(nonselection=="ignored") {
    # Replace NA's in selected_coefs with 0's
    # calculate estimates for selected_coefs
    results <- boot_results_df %>%
      select(term, estimate) %>%
      filter(term %in% names(coef(object))) %>%
      mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
      group_by(term) %>%
      summarize(
        mean = mean(estimate),
        ci.low = quantile(estimate, (1 - conf.level) / 2),
        ci.high = quantile(estimate, 1 - (1 - conf.level) / 2),
        prop_selected = mean(estimate != 0)
      ) %>%
      dplyr::right_join(tidy(sel_boot)[,1], by = "term")
  }

  if(nonselection=="confident") {
    # replace all NAs with 0's
    # calculate estimates for all coefs
    results <- boot_results_df %>%
      select(term, estimate) %>%
      mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
      group_by(term) %>%
      summarize(
        mean = mean(estimate),
        ci.low = quantile(estimate, (1 - conf.level) / 2),
        ci.high = quantile(estimate, 1 - (1 - conf.level) / 2),
        prop_selected = mean(estimate != 0)
      )
  }

  if(nonselection=="uncertain") {
    # replace all NAs with uncertain betas (within bootstrap)
    # calculate estimates for all coefs
    results <- boot_results_df %>%
      select(term, estimate_uncertain, estimate) %>%
      group_by(term) %>%
      summarize(
        mean = mean(estimate_uncertain),
        ci.low = quantile(estimate_uncertain, (1 - conf.level) / 2),
        ci.high = quantile(estimate_uncertain, 1 - (1 - conf.level) / 2),
        prop_selected = mean(!is.na(estimate))
      )
  }


  val <-   list(
    selector = object,
    nonselection = nonselection,
    B=B,
    uncertain_insample = uncertain_insample,
    results = results,
    bootstraps = boot_results_df,
    conf.level=conf.level
  )

  class(val) <- "infer_boot"
  return(val)
}
