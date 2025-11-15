#' Inference for `selector` via bootstrapping
#'
#' @param object a `selector` object
#' @param data data must be passed to infer
#' @param inference_target is inference requested on all or selected only
#' @param debias should estimates be debiased
#' @param estimation_data within a bootstrap, should in-sample or
#'  out-of-sample residuals be used for estimation (defaults to FALSE)
#' @param conf.level .95 by default
#' @param type what type of bootstrap (currently only `paired` supported)
#' @param B number of bootstrap resamples
#' @param n_cores number of cores to use
#' @return `inferrer` s3 class with things like...
#' @importFrom dplyr right_join group_by summarize bind_rows
#'
#' @rdname boot
#'
#' @export

infer_boot <- function(
  object,
  data,
  inference_target = c("selections", "all"),
  debias = TRUE,
  estimation_data = c("in-sample", "out-of-sample"),
  conf.level = .95,
  type = c("paired", "residual"),
  B = 250,
  n_cores = 4,
  ...) {

  # A bit of argument checking
  inference_target <- match.arg(inference_target)
  estimation_data <- match.arg(estimation_data)
  type <- match.arg(type)

  stopifnot(type == "paired")
  n_cores <- min(n_cores, parallel::detectCores() - 2)

  result <- boot(object, data=data, B=B, inference_target=inference_target,
                 debias = debias, estimation_data = estimation_data,
                 n_cores= n_cores, conf.level = conf.level, ...)
  result
}


#' Bootstrapping selection process
#'
#' @param object a `selector` object
#' @param B The number of bootstrap replicates.
#' @param inference_target is inference requested on all or selected only
#' @param debias should estimates be debiased (no, non-selections, or all)
#' @param estimation_data within a bootstrap, should in-sample or
#'  out-of-sample residuals be used for estimation (defaults to FALSE)
#' @param conf.level .95 by default
#' @param n_cores number of cores for parallel computation
#' @param ... 	any additional arguments to that can be passed to fitting engine
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select mutate summarize bind_rows rename arrange right_join
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @importFrom rlang sym
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @importFrom forcats fct_inorder
#' @importFrom stats as.formula glm coef formula median quantile sd terms
#' @importFrom utils tail
#'
#' @return an `inferrer` object
#'
#' @rdname boot
#' @export

boot <- function(object, data, B,
                 inference_target = c("selections", "all"),
                 debias = TRUE,
                 estimation_data = c("in-sample", "out-of-sample"),
                 conf.level, n_cores, ...) {
  stopifnot(inherits(object, "selector"))


  rec_obj <- attr(object, "recipe_obj")
  family <- attr(object, "meta")$family
  val <- tidy(object)
  outcome <- bake(rec_obj, new_data = data, all_outcomes())
  outcome_name <- names(outcome)

  if(estimation_data != "in-sample") {
    warning("using out-of-sample data is experimental; requires large sample size.")
  }

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
    val_boot <- tidy(sel_boot)

    selected_vars <- val_boot$term[val_boot$selected == 1][-1]
    nonselected_vars <- val_boot$term[!val_boot$selected]

    f_selected <- paste0(c(paste0(outcome_name, "~ 1"), selected_vars), collapse = " + ")
    f_selected_formula <- as.formula(f_selected)

    if(estimation_data == "in-sample") {
      new_data <- bake(rec_obj, new_data = data_boot)
    } else {
      new_data <-  bake(rec_obj, new_data = data[-boot_idx,])
    }

    # debiasing for selected terms
    if(debias) {

      fit_selected_debias <- glm(f_selected_formula, data = new_data, family = family)
      val_boot <- left_join(val_boot, tidy(fit_selected_debias)[,1:2], by = "term")

      if(length(nonselected_vars) > 0) {
        # If debiased non-selections, set to "uncertain nulls"
        for(j in 1:length(nonselected_vars)) {
          f_j <- paste0(f_selected, " + ", nonselected_vars[j])
          fit_j <- glm(as.formula(f_j), data = new_data, family = family)
          val_j <- tail(tidy(fit_j), 1)
          val_boot$estimate[val_boot$term == nonselected_vars[j]] <- val_j$estimate
        }
      }
    } else {
      val_boot$estimate <- val_boot$coef
      val_boot$estimate <- ifelse(is.na(val_boot$estimate),0,val_boot$estimate)

    }

    val_boot
    }
    # ,cl=cl uncomment after debug
    )

  # parallel::stopCluster(cl) # uncomment after debug
  boot_results_df <- bind_rows(boot_fits, .id = "bootstrap")

  if(inference_target=="selections") {
    # Replace NA's in selected_coefs with 0's
    # calculate estimates for selected_coefs
    results <- boot_results_df %>%
      select(term, coef, estimate) %>%
      filter(term %in% names(coef(object))) %>%
      mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
      group_by(term) %>%
      summarize(
        estimate_m = mean(estimate),
        ci_low = quantile(estimate, (1 - conf.level) / 2),
        ci_high = quantile(estimate, 1 - (1 - conf.level) / 2),
        prop_selected = mean(coef != 0)
      ) %>%
      rename(estimate = estimate_m) %>%
      right_join(tidy(object)[,1], by = "term")%>%
      arrange(match(term, unique(boot_results_df$term)))
  }

  if(inference_target == "all") {
    # replace all NAs with uncertain betas (within bootstrap)
    results <- boot_results_df %>%
      select(term, coef,estimate)%>%
      group_by(term) %>%
      summarize(
        estimate_m = mean(estimate),
        ci_low = quantile(estimate, (1 - conf.level) / 2),
        ci_high = quantile(estimate, 1 - (1 - conf.level) / 2),
        prop_selected = mean(coef != 0)
      )%>%
      rename(estimate = estimate_m)%>%
      arrange(match(term, unique(boot_results_df$term)))
  }


  meta_information <- list(
    B = B,
    inference_target = inference_target,
    debias = debias,
    estimation_data = estimation_data
  )

  as_inferrer(
    boot_results_df,
    name = "boot",
    label = "Bootstrap",
    nonselection = "N/A",
    inferences = results,
    conf.level = conf.level,
    selector = object,
    meta = meta_information
  )
}
