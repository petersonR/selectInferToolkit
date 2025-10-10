#' A two-step selection process using p-values
#'
#' @description This function implements a two-step selection process, either
#' screening marginal associations (null to model) or filtering (full to model).
#'
#' @param x Dataframe/model matrix with predictors (without intercept)
#' @param y outcome vector
#' @param family currently gaussian supported
#' @param std if TRUE (default), standardize design matrix
#' @param sig.level significance level for inclusion (default: 0.05)
#' @param starting_mod the mode of the search, can be "null" or "full" for forward vs backward
#' @param ... Additional arguments (not used currently)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#' @importFrom stats lm  model.frame model.matrix na.pass
#'
#' @return A list of class `selector_ic` containing:
#'
#' \item{beta}{a tibble containing term names and coefficients}
#' \item{std}{Was desing matrix standardized}
#' \item{sig.level}{penalty used (significance level for inclusion)}
#' \item{starting_mod}{the mode of the search ("null" or "full")}
#' \item{x}{the model dataframe used}
#' \item{y}{repsonse used in vector}
#' \item{model_sum}{the selected model details}
#'
#' @export

two_step_pval <- function(x, y, family = "gaussian", std = FALSE,
                    sig.level = 0.05, starting_mod = c("null", "full"),
                    ...) {

  starting_mod <- match.arg(starting_mod)

  if (is.matrix(x)) {
    if (std == TRUE) {
      x_df <- as.data.frame(x, check.names = FALSE)
      numeric_cols <- sapply(x_df, is.numeric)
      x_df[numeric_cols] <- lapply(x_df[numeric_cols], scale)
      colnames(x_df) <- colnames(x)
      x_std <- x_df
    } else {
      x_std <- as.data.frame(x, check.names = FALSE)
    }
  } else if (is.data.frame(x)) {
    if (std == TRUE) {
      numeric_cols <- sapply(x, is.numeric)
      x[numeric_cols] <- lapply(x[numeric_cols], scale)
      colnames(x) <- colnames(x) # optional, redundant unless colnames were altered
      x_std <- x
    } else {
      x_std <- x
    }
  }

  raw_data <- as.data.frame(cbind(x_std, y))
  rownames(raw_data) <- NULL

  # fit full model to get term names
  full_model <- lm(y ~ ., data = raw_data)
  coef_est <- coef(full_model)
  full_model_df <- data.frame(term = names(coef_est), estimate = as.numeric(coef_est))

  # Prepare x_input and raw_data_fs if needed
  x_input <- if (make_levels) dummy_col_fn(x_std, TRUE) else x_std

  pvals <- numeric(ncol(x_std))
  names(pvals) <- names(x_std)

  if(starting_mod == "full") {

    # Significance of all predictors relative to full model
    for(j in 1:ncol(x_std)) {
      x_j_name <- names(x_std)[j]
      f_j_removed <- as.formula(paste0("y ~ . - ", x_j_name))
      fit_minus_x_j <- update(full_model, f_j_removed)
      pvals[j] <- anova(fit_minus_x_j, full_model)$`Pr(>F)`[2]
    }

    mod_formula <- paste0("y ~ ", paste0(c(1, names(pvals)[pvals < sig.level]), collapse = " + "))

    model <- update(full_model, mod_formula)

  } else if (startin_mod == "null") {
    # screen for unadjusted p-values from null, throw all "significant" ones into model
    for(j in 1:ncol(x_std)) {
      x_j_name <- names(x_std)[j]
      f_j_added <- as.formula(paste0("y ~ 1+ ", x_j_name))
      fit_x_j <- update(full_model, f_j_added)
      pvals[j] <- anova(fit_x_j)$`Pr(>F)`[1]
    }

    mod_formula <- paste0("y ~ ", paste0(c(1, names(pvals)[pvals < sig.level]), collapse = " + "))
    model <- update(full_model, mod_formula)

  }

  # Extract model coefficients
  coefs <- coef(model)
  mod_df <- data.frame(term = names(coefs), estimate = as.numeric(coefs), stringsAsFactors = FALSE)
  mod_df$term <- gsub("`", "", mod_df$term)

  # Create final coefficient table
  all_terms <- full_model_df$term
  coef_full <- merge(
    data.frame(term = all_terms, stringsAsFactors = FALSE),
    mod_df,
    by = "term",
    all.x = TRUE,
    all.y = T,
    sort = F
  )
  coef_full <- coef_full[match(all_terms, coef_full$term), ]

  # Assemble final output
  data <- model[["model"]]
  val <- list(
    beta = coef_full,
    std = std,
    sig.level = sig.level,
    starting_mod = starting_mod,
    x_original = x,
    y = data[[1]],
    x_model = raw_data[, setdiff(names(raw_data), "y"), drop = FALSE],
    model_sum = model,
    family = family
  )
  class(val) <- "selector_two_step_pval"
  val
}

#' Title
#'
#' @param x model of class `selector_two_step_pval`
#' @param ... additional arguments (currently not supported)
#' @method print `selector_two_step_pval`
#' @return returns x invisibly
#' @export
print.selector_two_step_pval <- function(x, ...) {
  cat("Two-step p-value-based model selection summary:\n")

  # Model direction
  cat("Direction of Selection: ", x$starting_mod, "\n")

  # Penalty used for selection
  cat("Significance level used: ", x$sig.level, "\n")

  # Standard errors used in the model
  if (x$std) {
    cat("Design Matrix standardized: TRUE\n")
  } else {
    cat("Design Matrix standardized: FALSE\n")
  }

  # Model coefficients (first few)
  cat("\nFinal Model Coefficients:\n")
  print(summary(x[["model_sum"]])[["coefficients"]][, 1])
  return(invisible(x))
}


#' Tidy-er for selector object (`selector_two_step_pval`)
#'
#' @param x model of class `selector_two_step_pval`
#' @param ... Additional arguments passed to the generic `tidy` function.
#' @importFrom tibble as_tibble
#' @importFrom broom tidy
#' @return  A tibble containing the tidied coefficients of the model.
#' @export
tidy.selector_two_step_pval <- function(x, ...) {
  ret <- as_tibble(summary(x[["model_sum"]])$coefficients[,1], rownames = "term")
  names(ret)[2] <- "estimate"
  return(ret)
}

#' Inference for selector_ic class except bootstrap which is it's own function
#'
#'
#' @param model model returned from selector_ic  class
#' @param method A character string specifying method of post-selection inference.Currently "hybrid"or  "selectiveinf"
#' @param nonselection  A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @return infer_ic class list with
#' \item{model}{A dataframe with model results including columns for term, estimate, std.error, statistics, p.value, conf.low, conf.high, ci_ln}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{medain CI length across all variables in model}
#' \item{selection_method}{Stepwsie}
#' \item{direction}{the mode of step wise search}
#' \item{penalty}{penalty used (AIC or BIC)}
#' \item{infmethod}{Inference method chosen}
#' \item{nonselection}{method chosen  to deal with non selection}
#'
#' @rdname infer
#' @method infer selector_two_step_pval
#'
#' @export
infer.selector_two_step_pval <- function(
    model,
    method = c("hybrid", "selectiveinf", "boot"),
    nonselection = c("ignored", "confident_nulls", "uncertain_nulls")
) {

  method <- match.arg(method)
  nonselection <- match.arg(nonselection)

  if(method == "selectiveinf" & model$direction != "forward") {
    warning("Direction must be set to 'forward' for selectiveInference and make.levels=T")
  }

  if (method == "hybrid" && nonselection == "ignored") {

    coefs <- summary(model[["model_sum"]])$coefficients
    conf_int <- confint( model[["model_sum"]])
    terms <- gsub("`", "", rownames(coefs))
    ci_ln <- conf_int[, 2] - conf_int[, 1]

    # Create full_mod data frame
    full_mod <- data.frame(
      term = terms,
      estimate = coefs[, "Estimate"],
      std.error = coefs[,"Std. Error"],
      p.value = coefs[, "Pr(>|t|)"],
      conf.low = conf_int[, 1],
      conf.high = conf_int[, 2],
      ci_ln = ci_ln,
      stringsAsFactors = FALSE
    )
    rownames(full_mod) <- NULL

    # Compute summary stats excluding intercept
    ci_avg_ratio <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"], na.rm = TRUE)
    ci_median_ratio <- median(full_mod$ci_ln[full_mod$term != "(Intercept)"], na.rm = TRUE)

    all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
    full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)

    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])

    result <- list(
      model = full_mod, ci_avg_ratio = ci_avg_ratio, ci_median_ratio = ci_median_ratio,
      selection_method = "Stepwise", direction = model[["direction"]], penalty = model[["penalty"]],
      infmethod = method, nonselection = nonselection
    )
    class(result) <- "infer_ic"
    result
  }
  else if (method == "hybrid" && nonselection == "confident_nulls") {

    coefs <- summary(model[["model_sum"]])$coefficients
    conf_int <- confint( model[["model_sum"]])
    terms <- gsub("`", "", rownames(coefs))

    # Create full_mod data frame
    full_mod <- data.frame(
      term = terms,
      estimate = coefs[, "Estimate"],
      std.error = coefs[,"Std. Error"],
      p.value = coefs[, "Pr(>|t|)"],
      conf.low = conf_int[, 1],
      conf.high = conf_int[, 2],
      stringsAsFactors = FALSE
    )
    rownames(full_mod) <- NULL

    all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)

    full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])

    # Replace NAs
    full_mod$estimate   <- ifelse(is.na(full_mod$estimate),   0, full_mod$estimate)
    full_mod$p.value    <- ifelse(is.na(full_mod$p.value),    1, full_mod$p.value)
    full_mod$std.error  <- ifelse(is.na(full_mod$std.error),  0, full_mod$std.error)
    full_mod$conf.low   <- ifelse(is.na(full_mod$conf.low),   0, full_mod$conf.low)
    full_mod$conf.high  <- ifelse(is.na(full_mod$conf.high),  0, full_mod$conf.high)
    full_mod$ci_ln <- full_mod$conf.high - full_mod$conf.low

    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(
      model = full_mod, ci_avg_ratio = ci_avg_ratio, ci_median_ratio = ci_median_ratio,
      selection_method = "Stepwise", direction = model[["direction"]], penalty = model[["penalty"]],
      infmethod = method, nonselection = nonselection
    )
    class(result) <- "infer_ic"
    result
  }
  else if (method == "hybrid" && nonselection == "uncertain_nulls") {

    coefs <- summary(model[["model_sum"]])$coefficients
    conf_int <- confint( model[["model_sum"]])
    terms <- gsub("`", "", rownames(coefs))

    # Create full_mod data frame
    full_mod <- data.frame(
      term = terms,
      estimate = coefs[, "Estimate"],
      std.error = coefs[,"Std. Error"],
      p.value = coefs[, "Pr(>|t|)"],
      conf.low = conf_int[, 1],
      conf.high = conf_int[, 2],
      stringsAsFactors = FALSE
    )
    rownames(full_mod) <- NULL

    all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
    full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)
    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
    res =residuals(model[["model_sum"]])

    x <-model[["x_model"]]
    x_dup<- as.data.frame(model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))[,-1],
                          check.names=FALSE)

    final_mod= get_uncertain_nulls (mod=full_mod, res=res, x=x_dup)

    ci_avg_ratio  <- mean(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)


    result <- list(model=   final_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method="Stepwise",direction = model[["direction"]],penalty= model[["penalty"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_ic"
    result


  }
  else if (method == "selectiveinf" && nonselection == "ignored") {

    x <-data.frame(model[["x_model"]], check.names = FALSE)
    x <- droplevels(x)
    y <- model[["y"]]
    x_mat= model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))[,-1]
    std=model[["std"]]


    if (model[["penalty"]]=="AIC"){
      fs_si_aic = sel_inf_fs(x_mat, y, std=std)
    }else{
      fs_si_aic = sel_inf_fs(x_mat, y,mult= log(length(y)),std=std)
    }

    full_mod <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
    full_mod <- merge(full_mod, fs_si_aic, by = "term", all.x = TRUE)
    full_mod$ci_ln <- full_mod$conf.high - full_mod$conf.low
    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)

    all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
    full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)

    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])

    result <- list(
      model = full_mod, ci_avg_ratio = ci_avg_ratio, ci_median_ratio = ci_median_ratio,
      selection_method = "Stepwise", direction = model[["direction"]], penalty = model[["penalty"]],
      infmethod = method, nonselection = nonselection
    )
    class(result) <- "infer_ic"
    return(result)
  }
  else if (method == "selectiveinf" && nonselection == "confident_nulls") {

    x <-data.frame(model[["x_model"]], check.names = FALSE)
    x <- droplevels(x)
    y <- model[["y"]]
    x_mat= model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))[,-1]
    std=model[["std"]]


    if (model[["penalty"]]=="AIC"){
      fs_si_aic = sel_inf_fs(x_mat, y,std=std)
    }else{
      fs_si_aic = sel_inf_fs(x_mat, y,mult= log(length(y)),std=std)
    }

    all_terms<- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
    full_mod <- merge(all_terms, fs_si_aic, by = "term", all.x = TRUE,sort = FALSE)
    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])


    # Replace NAs
    full_mod$estimate   <- ifelse(is.na(full_mod$estimate),   0, full_mod$estimate)
    full_mod$p.value    <- ifelse(is.na(full_mod$p.value),    1, full_mod$p.value)
    full_mod$conf.low   <- ifelse(is.na(full_mod$conf.low),   0, full_mod$conf.low)
    full_mod$conf.high  <- ifelse(is.na(full_mod$conf.high),  0, full_mod$conf.high)
    full_mod$ci_ln <- full_mod$conf.high - full_mod$conf.low

    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <- median(full_mod$ci_ln[full_mod$term != "(Intercept)"], na.rm = T)

    result <- list(
      model = full_mod, ci_avg_ratio = ci_avg_ratio, ci_median_ratio = ci_median_ratio,
      selection_method = "Stepwise", direction = model[["direction"]], penalty = model[["penalty"]],
      infmethod = method, nonselection = nonselection
    )
    class(result) <- "infer_ic"
    result


  }
  else if (method == "selectiveinf" && nonselection == "uncertain_nulls") {

    x <-data.frame(model[["x_model"]], check.names = FALSE)
    x <- droplevels(x)
    x_mat= model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))[,-1]
    y <- model[["y"]]
    std=model[["std"]]

    if (model[["penalty"]]=="AIC"){
      fs_si = sel_inf_fs(x_mat, y, std=std)
    }else{
      fs_si = sel_inf_fs(x_mat, y,mult= log(length(y)),std=std)
    }

    # get the residuals
    # Extract relevant terms and coefficients
    terms <- fs_si$term[fs_si$term != "(Intercept)"]
    beta <- setNames(fs_si$estimate, fs_si$term)

    # Subset the design matrix and align coefficients
    x_subset <- x_mat[, terms, drop = FALSE]
    beta_vector <- beta[colnames(x_subset)]
    beta_vector[is.na(beta_vector)] <- 0

    # Compute XB
    xb <- x_subset %*% beta_vector + ifelse("(Intercept)" %in% names(beta), beta["(Intercept)"], 0)
    res = y-as.numeric(xb)

    all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
    full_mod <- merge(all_terms, fs_si, by = "term", all.x = TRUE)
    full_mod$std.error <- NA

    # Reorder columns to match desired output
    desired_order <- c("term", "estimate", "std.error",  "p.value", "conf.low", "conf.high")
    other_cols <- setdiff(names(full_mod), desired_order)
    full_mod <- full_mod[c(desired_order, other_cols)]

    x_dup<- as.data.frame( x_mat, check.names=FALSE)

    final_mod =get_uncertain_nulls (mod=full_mod, res=res, x=  x_dup)
    final_mod <- final_mod[, !(names(final_mod) %in% c("std.error", "statistic"))]
    matched_rows <- match(all_terms$term, final_mod$term)
    final_mod<- final_mod[matched_rows, ]
    final_mod<- cbind(all_terms, final_mod[ , setdiff(names(final_mod), "term")])


    ci_avg_ratio  <- mean(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(final_mod $ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  final_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio  ,
                   selection_method="Stepwise",direction = model[["direction"]],penalty= model[["penalty"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_ic"
    result



  }
  else if (method == "boot") {
    result <- boot(model, B=B, nonselection=nonselection,
                   n_cores= n_cores, save_beta=save_beta,
                   ...)
    return(result)
  }
}

