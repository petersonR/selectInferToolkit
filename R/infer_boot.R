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
#' @importFrom dplyr mutate_if filter select mutate summarise bind_rows rename arrange right_join
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom tibble tibble
#' @importFrom purrr map
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

  if(estimation_data != "in-sample") {
    warning("using out-of-sample data is experimental; requires large sample size.")
  }

  rec_obj <- attr(object, "recipe_obj")
  family  <- attr(object, "meta")$family
  family <- if (is.character(family)) get(family,mode = "function")() else family

  outcome_name <- rec_obj$var_info |> filter(role == "outcome") |> pull(variable)

  # if cat var selected togther, expand them
  if (attr(object, "name") == "stepwise_ic" &&
      attr(object, "meta")$select_factors_together) {

    rec_obj <- rec_obj |> step_dummy(all_factor_predictors(),
        naming = function(...) dummy_names(..., sep = "")
      ) |>prep()
  }

  X_full <- bake(rec_obj, new_data = data, recipes::all_predictors())
  y_full <- bake(rec_obj, new_data = data, recipes::all_outcomes())[[1]]

  # term to  column mapping (CRITICAL for when factors are selected togther) ----
  tidy0 <- tidy(object)

  # align column names
  term_to_col <-tibble( term = tidy0$term,term_clean = clean_name(tidy0$term)) |>
    mutate( col = map( term_clean,
        ~ colnames(X_full)[clean_name(colnames(X_full)) == .x]))

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
    val_boot <- val_boot %>%
      mutate(term_clean = clean_name(term))

    # bake estimation data
    if (estimation_data == "in-sample") {
      X <- bake(rec_obj, new_data = data_boot, recipes::all_predictors())
      y <- bake(rec_obj, new_data = data_boot, recipes::all_outcomes())[[1]]
    } else {
      X <- bake(rec_obj, new_data = data[-boot_idx, ], recipes::all_predictors())
      y <- bake(rec_obj, new_data = data[-boot_idx, ], recipes::all_outcomes())[[1]]
    }

    selected_terms <- val_boot$term[val_boot$selected == 1]
    selected_cols <- term_to_col |>filter(term %in% selected_terms) |>pull(col) |>
      unlist()

    # debiasing for selected terms
    if(debias) {

      df <- X
      df$y <- y

      if(length( selected_cols) ==0){
        formula_str <- paste0("y ~ .")
      }else{
        formula_str <- paste0("y ~ ", paste0( selected_cols , collapse = " + "))
      }
      fit_sel_debias <- glm(as.formula(formula_str), data = df, family = family)

      sel_coefs <- tidy(  fit_sel_debias )[, c("term", "estimate")] %>%
        mutate(term_clean = term)

      val_boot <- val_boot %>% left_join(sel_coefs %>% select(term_clean, estimate),
          by = "term_clean",suffix = c("", "_new"))  %>%select( -term_clean)
      nonselected_terms <- val_boot$term[val_boot$selected == 0]

      if(length(nonselected_terms) > 0) {
        # If debiased non-selections, set to "uncertain nulls"
        for (t in nonselected_terms) {
          cols_t <- term_to_col$col[term_to_col$term == t][[1]]
          if (length(cols_t) == 0) next
          X_aug <- X[, c(selected_cols, cols_t), drop = FALSE]
          df_fit <- cbind(y, X_aug)
          fit_j <- glm(y ~.,data=df_fit,family = family)
          tj <- tidy(fit_j)
          tj <- tj[tj$term %in% cols_t, , drop = FALSE]
          if (nrow(tj) > 0) {
            val_boot$estimate[val_boot$term == t] <- tj$estimate[1]}
        }
        # for weird cases where it's not possible to do ucnertain null
        val_boot$estimate <- ifelse(is.na(val_boot$coef), 0, val_boot$coef)
      }

    } else {
      val_boot$estimate <- ifelse(is.na(val_boot$coef), 0, val_boot$coef)
    }

    val_boot
    }
    # ,cl=cl uncomment after debug
    )

  # parallel::stopCluster(cl) # uncomment after debug
  boot_df<- bind_rows(boot_fits, .id = "bootstrap")

  if(inference_target=="selections") {
    results <- boot_df  %>%
      filter(term %in% names(coef(object))) %>%
      select(term, coef, estimate) %>%
      group_by(term)  %>%
      summarise(
        estimate_m = mean(estimate),
        ci_low   = quantile(estimate, (1 - conf.level) / 2),
        ci_high  = quantile(estimate, 1 - (1 - conf.level) / 2),
        prop_selected = mean(coef != 0),
        .groups = "drop"
      )%>%
      rename(estimate = estimate_m) %>%
      right_join(tidy(object)[,1], by = "term")%>%
      arrange(match(term, unique(boot_df$term)))
  }

  if(inference_target=="all") {
    results <- boot_df  %>%
      select(term, coef, estimate) %>%
      group_by(term)  %>%
      summarise(
        estimate_m = mean(estimate),
        ci_low   = quantile(estimate, (1 - conf.level) / 2),
        ci_high  = quantile(estimate, 1 - (1 - conf.level) / 2),
        prop_selected = mean(coef != 0),
        .groups = "drop"
      )%>%
      rename(estimate = estimate_m) %>%
      arrange(match(term, unique(boot_df$term)))
  }


  meta_information <- list(
    B = B,
    inference_target = inference_target,
    debias = debias,
    estimation_data = estimation_data
  )

  as_inferrer(
    boot_df,
    name = "boot",
    label = "Bootstrap",
    nonselection = "N/A",
    inferences = results,
    conf.level = conf.level,
    selector = object,
    meta = meta_information
  )
}
