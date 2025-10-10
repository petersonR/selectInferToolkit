#' Stepwise forwad/backward/bidirectional selection with AIC/BIC
#'
#'
#' @description This function implements forward/backward/bidirectional stepwise regression,
#' for use in the selectInferToolkit package
#'
#'
#' @param x Dataframe/model matrix with predictors (without intercept)
#' @param y outcome vector
#' @param family currently gaussian supported
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
#' @return A list of class `selector_stepwise_ic` containing:#'
#' \item{beta}{a tibble containing term names and coefficients}
#' \item{std}{Was desing matrix standadrized}
#' \item{penalty}{penalty used (AIC or BIC)}
#' \item{direction}{the mode of step wise search}
#' \item{x}{the model dataframe used}
#' \item{y}{repsonse used in vector}
#' \item{model_sum}{the stepwise-selected model  details is returned}
#' @export

stepwise_ic <- function(x, y, family = "gaussian", std = FALSE,
                        penalty = "AIC", direction = "forward",
                        make_levels = FALSE, ...) {
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
  # Set penalty value
  k_val <- if (penalty == "AIC") 2 else if (penalty == "BIC") log(nrow(raw_data)) else stop("Unsupported penalty")

  # Prepare x_input and raw_data_fs if needed
  x_input <- if (make_levels) dummy_col_fn(x_std, TRUE) else x_std
  use_scope <- make_levels || direction == "forward"

  if (use_scope) {
    clean_colnames <- clean_colnames_fn(colnames(x_input))
    raw_data_fs <- as.data.frame(cbind(x_input, y))
    scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
    model <- stepAIC(
      lm(y ~ 1, data = raw_data_fs),
      scope = list(lower = ~1, upper = scope_formula),
      direction = direction,
      k = k_val,
      trace = 0,
      ...
    )
  } else {
    model <- stepAIC(
      lm(y ~ ., data = raw_data),
      direction = direction,
      trace = 0,
      k = k_val,
      ...
    )
  }

  # Extract model coefficients
  coefs <- coef(model)
  mod_df <- data.frame(term = names(coefs), estimate = as.numeric(coefs), stringsAsFactors = FALSE)
  mod_df$term <- gsub("`", "", mod_df$term)

  # Create final coefficient table
  all_terms <- if (make_levels) colnames(raw_data_fs)[colnames(raw_data_fs) != "y"] else full_model_df$term
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
    penalty = penalty,
    direction = direction,
    x_original = x,
    y = data[[1]],
    x_model = raw_data[, setdiff(names(raw_data), "y"), drop = FALSE],
    model_sum = model,
    make_levels = make_levels,
    family = family
  )
  class(val) <- "selector_stepwise_ic"
  val

}

#' Title
#'
#' @param x model of class `selector_stepwise_ic`
#' @param ... additional arguments (currently not supported)
#' @method print `selector_stepwise_ic`
#' @return returns x invisibly
#' @export
print.selector_stepwise_ic <- function(x, ...) {
  cat("Stepwise Model Selection Summary:\n")

  # Model direction
  cat("Direction of Selection: ", x$direction, "\n")

  # Penalty used for selection
  cat("Penalty used: ", x$penalty, "\n")

  # Standard errors used in the model
  if (x$std) {
    cat("Design Matrix standardized: TRUE\n")
  } else {
    cat("Design Matrix standardized: FALSE\n")
  }

  # Model coefficients (first few)
  cat("\nFinal Model Coefficients:\n")
  print(summary(x[["model_sum"]])[["coefficients"]][, 1])
}


#' Tidy-er for selector object (`selector_stepwise_ic`)
#'
#' @param x model of class `selector_stepwise_ic`
#' @param ... Additional arguments passed to the generic `tidy` function.
#' @importFrom tibble as_tibble
#' @importFrom broom tidy
#' @return  A tibble containing the tidied coefficients of the model.
#' @export
tidy.selector_stepwise_ic <- function(x, ...) {
  ret <- as_tibble(summary(x[["model_sum"]])$coefficients[,1], rownames = "term")
  names(ret)[2] <- "estimate"
  return(ret)
}


#' Inference for selector_stepwise_ic class except bootstrap which is it's own function
#'
#'
#' @param model model returned from selector_stepwise_ic  class
#' @param method A character string specifying method of post-selection inference.Currently "hybrid"or  "selectiveinf"
#' @param nonselection  A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @return infer_stepwise_ic class list with
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
#' @method infer selector_stepwise_ic
#'
#' @export

infer.selector_stepwise_ic <- function(
    model,
    method = c("hybrid", "selectiveinf", "boot"),
    nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
    B = 250,
    n_cores = 1,
    save_beta = FALSE,
    ...
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
    class(result) <- "infer_stepwise_ic"
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
    class(result) <- "infer_stepwise_ic"
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
    class(result) <- "infer_stepwise_ic"
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
    class(result) <- "infer_stepwise_ic"
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
    class(result) <- "infer_stepwise_ic"
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
    class(result) <- "infer_stepwise_ic"
    result



  }
  else if (method == "boot") {
    result <- boot(model, B=B, nonselection=nonselection,
                   n_cores= n_cores, save_beta=save_beta, ...)

    result$infmethod <- method
    result$ci_avg_ratio  <- mean(result$model$ci_ln[result$model$term != "(Intercept)"] , na.rm=T)
    result$ci_median_ratio <-  median(result$model$ci_ln[result$model$term != "(Intercept)"] , na.rm=T)

    class(result) <- "infer_stepwise_ic"
    return(result)
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

boot.selector_stepwise_ic <- function(model, B = 250,
                                      nonselection="ignored",
                                      n_cores = 1,
                                      make_levels=FALSE,
                                      save_beta = FALSE,...) {

  penalty <- model$penalty
  family <- model$family
  x <-model[["x_model"]]
  y <- model[["y"]]
  std <- model[["std"]]
  data <- data.frame(cbind(y,x),check.names = F)
  selected_terms  <- model[["beta"]][["term"]][! is.na(model[["beta"]][["estimate"]]) ]
  all_terms <- model[["beta"]][["term"]]
  direction <- model$direction

  if(n_cores > 1) {
    results <- boot_stepwise_parallel(x = x, y = y, family = family,
                                      std = std, n_cores = n_cores,
                                      B = B, direction = direction,
                                      nonselection=nonselection,
                                      penalty = penalty,
                                      save_beta = save_beta,
                                      make_levels = make_levels,
                                      all_terms = all_terms,
                                      data= data,
                                      selected_terms = selected_terms)
  } else {
    results <-  boot_stepwise(x = x, y = y, family = family, std = std,
                              B = B, direction = direction,
                              nonselection=nonselection, penalty = penalty,
                              save_beta = save_beta,
                              make_levels = make_levels,
                              all_terms = all_terms,
                              data= data,
                              selected_terms = selected_terms)
  }

  results
}

#' Title
#' @param x model of class `infer_stepwise_ic`
#' @return returns x invisibly
#' @export
print.infer_stepwise_ic <- function(x, ...) {

  # Penalty used for selection
  cat("Selection method: ", x[["selection_method"]], "  ", x[["penalty"]],".  Direction: " ,x[["direction"]], "\n", sep = "")

  if(x$infmethod == "boot")
    x$infmethod <- paste0("Bootstrap (B=", x$B,")")
  cat("Inference method: ", x[["infmethod"]], "\n", sep = "")

  cat ("Method for handling null: ", x[["nonselection"]], "\n", sep = "")

  # Average CI length
  cat ("Average confidence interval length ", x[["ci_avg_ratio"]], "\n", sep = "")

  # Median CI length
  cat ("Median confidence interval length ", x[["ci_median_ratio"]], "\n", sep = "")

  # coeff <-c(x[["model"]][["estimate"]])
  # names(coeff)<- x[["model"]][["term"]]
  #
  # cat("\nFinal Model Results:\n")
  # print( coeff)


}

#' Title
#' @param x model of class `infer_stepwise_ic`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom  tidy
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.infer_stepwise_ic <- function(x, ...) {

  ret<- as_tibble(x[["model"]])
  return(ret)
}

