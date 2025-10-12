#' Fit an MCP- or SCAD-penalized regression
#' @description
#' Performs k-fold cross validation for MCP- or SCAD-penalized regression
#' models over a grid of values for the regularization parameter lambda and returns the
#' coefficeints associated with either value of lambda that gives minimum cvm or largest value of lambda such that error is within 1 standard error of the minimum.
#'
#' @param x Dataframe/model matrix with predictors (without intercept)
#' @param y outcome vector
#' @param family currently gaussian supported
#' @param std if TRUE (default), standardize design matrix
#' @param penalty lasso or MCP
#' @param lambda  extra coefficients associated with "lambda.min" or "lambda.1se"
#' @param alpha Tuning parameter  small, but not exactly 0.
#' @param ... Additional arguments that can be passed with cv.ncvreg function in cv.ncvreg package
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix
#' @importFrom stats na.pass
#' @importFrom MASS stepAIC
#' @importFrom ncvreg cv.ncvreg
#' @return A list of class `selector_pen` containing:
#' \item{beta}{a tibble containing term names and coefficients}
#' \item{std}{Was desing matrix standadrized}
#' \item{penalty}{penalty used (lasso or MCP)}
#' \item{lambda}{are the coefficeint associated with "lambda.min" or "lambda.1se"}
#' \item{lambda.select}{numeric value of selected lambda}
#' \item{fold}{Which fold each observation belongs to. By default the observations are randomly assigned.}
#' \item{x}{ the model dataframe used}
#' \item{y}{repsonse used}
#' \item{alpha}{selected alpha for model fitting}
#' @export

pen_cv <- function(x, y, family = "gaussian", std = TRUE,
                   penalty = "lasso", lambda = "lambda.min",
                   alpha = 1, quiet = TRUE, ...){

  if (is.matrix(x) || is.data.frame(x)) {
    if (std) {
      # Standardize numeric columns
      x_std <- as.data.frame(x, check.names = FALSE)
      num_cols <- sapply(x_std, is.numeric)
      x_std[num_cols] <- lapply(x_std[num_cols], scale)

      # Preserve original column names
      colnames(x_std) <- colnames(x)

      # Create model matrix
      x_dup <- model.matrix(
        y ~ .,
        model.frame(~., data = data.frame(cbind(x_std, y), check.names = FALSE), na.action = na.pass)
      )[, -1]
    } else {
      # No standardization, directly build model matrix
      x_dup <- model.matrix(
        y ~ .,
        model.frame(~., data = data.frame(cbind(x, y), check.names = FALSE), na.action = na.pass)
      )[, -1]
    }
  }

  # drop zero variance columns of X
  x_var <- apply(x_dup, 2, var)
  if(any(x_var == 0))  {
    x_dup <- x_dup[,x_var != 0]
    if(!quiet)
      message("Note: dropping", sum(x_var == 0, na.rm = TRUE), " predictor(s) with no variance")
  }
  # Note: ncvreg standardizes the data and includes an intercept by default.

  raw_data = as.data.frame(cbind(x_dup,y))
  rownames(raw_data) <- NULL

  full_model <- lm(y ~ ., data= raw_data)
  coef_est <- coef(full_model)
  full_model_df <- data.frame(term = names(coef_est))
  full_model$term <- gsub("`", "", full_model$term)


  if (alpha==1 & penalty=="lasso"){
    #for glmnet alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
    fit <- cv.glmnet(x = x_dup, y = y, alpha = alpha, standardize = F, family = "gaussian", ...)
    foldid <-fit[["foldid"]]
    lmax <- max(fit$lambda)
    lambda_seq= fit[["lambda"]]

  }else{
    #alpha=1 is equivalent to MCP/SCAD penalty,
    fit <- cv.ncvreg(X = x_dup, y = y,  penalty = penalty, family="gaussian",alpha=alpha,...)
    foldid <-fit[["fold"]]
    lmax <- max(fit$lambda)
    lambda_seq=  fit[["lambda"]]

  }


  # Calculate correct lambda
  if(lambda=="lambda.min"){
    lambda_mod =  fit[["lambda.min"]]
  } else if (lambda=="lambda.1se"){
    if(alpha==1 & penalty=="lasso"){
      lambda_mod =  fit[["lambda.1se"]]
    }
    else{
      lambda_mod <- fit$lambda[which(fit$cve < min(fit$cve + fit$cvse))[1]]

    }
  }

  if (alpha==1 & penalty=="lasso"){
      b <- coef(fit, s=  lambda_mod, exact = TRUE)
      beta <- data.frame(term = rownames(b), estimate = as.vector(b))

  }else{
    tolerance <- 1e-6 # Define a small tolerance
    lambda_index <- which.min(abs(fit$lambda - lambda_mod)) # Find the closest lambda
    beta <- data.frame(term=names(coef(fit, which = lambda_index)) ,
                       estimate=unname(coef(fit, which = lambda_index)))

  }

  val <- list( beta=beta, std=std,penalty=penalty, lambda_seq=  lambda_seq, lambda=lambda,
               lambda.select= lambda_mod,lmax=lmax,
              fold=foldid, x=x_dup, y= y,
              alpha=alpha,    x_original=x, model=   fit,
              family = family)
  class(val) <- "selector_pen"
  val

}


#' Title
#'
#' @param x  model of class `selector_pen`
#' @importFrom tibble as_tibble
#' @method print `selector_pen`
#' @return returns x invisibly
#' @export
print.selector_pen <- function(x, ...) {
  cat("Penalized regression  Model Summary:\n")

  # Standard errors used in the model
  if(x$std) {
    cat("Design Matrix standardized: TRUE\n")
  } else {
    cat("Design Matrix standardized: FALSE\n")
  }

  # Penalty used for selection
  cat("Penalty used: ", x$penalty, " and alpha:",x$alpha, "\n")

  # lambda
  cat("Coefficient associated with : ", x$lambda, "\n")

  beta = x[["beta"]][x[["beta"]][["estimate"]]!=0,][,2]
  names(beta) =x[["beta"]][x[["beta"]][["estimate"]]!=0,][,1]

  # Model coefficients (first few)
  cat("\nFinal Model Non-Zero Coefficients:\n")
  print(beta)

}

#' Title
#' @param x   model of class `selector_pen`
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.selector_pen <- function(x, ...) {

  ret<- as_tibble(x[["beta"]])
  return(ret)
}

#' Inference for selector_pen class except bootstrap which is it's own function
#'
#' @param model model of selector_pen class returned from  pen_cv function
#' @param method A character string specifying method of post-selection inference. Currently "hybrid", "selectiveinf" or
#' "boot" supported
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @return "infer_pen" class list with
#' @importFrom broom tidy
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @rdname infer
#' @method infer selector_pen
#' @export
#'
infer.selector_pen <- function(model, method = "hybrid", nonselection = "ignored",
                               B = 250, n_cores = 1, save_beta=FALSE, boot_desparse=FALSE){
  x <-model[["x"]]
  y <- model[["y"]]


  if (method == "hybrid" && nonselection == "ignored") {
    non_zero_terms  <- model[["beta"]]$term[model[["beta"]]$estimate != 0]
    non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]

    selected_data <-  data.frame(y = model[["y"]], data.frame(model[["x"]],
                                                              check.names = FALSE),check.names = FALSE)
    if (length(non_zero_terms) == 0) {
      selected_data <- data.frame(y)
    } else {
      selected_data <- selected_data[, c("y", non_zero_terms)]
    }

    fit <- lm(y ~ ., data = selected_data)
    conf <- confint(fit)
    coefs <- coef(summary(fit))
    terms <- gsub("`", "", rownames(coefs))
    ci_ln <- conf[, 2] - conf[, 1]

    full_mod <- data.frame(
      term = terms,
      estimate = coefs[, "Estimate"],
      std.error = coefs[,"Std. Error"],
      p.value = coefs[, "Pr(>|t|)"],
      conf.low = conf[, 1],
      conf.high = conf[, 2],
      ci_ln = ci_ln,
      stringsAsFactors = FALSE
    )
    rownames(full_mod) <- NULL

    all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
    full_mod <-merge(all_terms, full_mod, by = "term", all.x = TRUE, sort = FALSE)

    matched_rows <- match(all_terms$term, full_mod$term)
    full_mod <- full_mod[matched_rows, ]
    full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])

    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  full_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection
    )
    class(result) <- "infer_pen"
    result


  }
  else if (method == "hybrid" && nonselection == "confident_nulls") {
    non_zero_terms  <- model[["beta"]]$term[model[["beta"]]$estimate != 0]
    non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]

    selected_data <-  data.frame(y = model[["y"]], data.frame(model[["x"]],
                                                              check.names = FALSE),check.names = FALSE)
    selected_data <- selected_data[, c("y", non_zero_terms)]
    if (length(non_zero_terms) == 0) {
      selected_data <- data.frame(y)
    } else {
      selected_data <- selected_data[, c("y", non_zero_terms)]
    }

    fit <- lm(y ~ ., data = selected_data)
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

    # Replace NAs
    full_mod$estimate   <- ifelse(is.na(full_mod$estimate),   0, full_mod$estimate)
    full_mod$p.value    <- ifelse(is.na(full_mod$p.value),    1, full_mod$p.value)
    full_mod$std.error  <- ifelse(is.na(full_mod$std.error),  0, full_mod$std.error)
    full_mod$conf.low   <- ifelse(is.na(full_mod$conf.low),   0, full_mod$conf.low)
    full_mod$conf.high  <- ifelse(is.na(full_mod$conf.high),  0, full_mod$conf.high)
    full_mod$ci_ln <- full_mod$conf.high - full_mod$conf.low

    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model= full_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_pen"
    result
  }
  else if (method == "hybrid" && nonselection == "uncertain_nulls") {
    non_zero_terms  <- model[["beta"]]$term[model[["beta"]]$estimate != 0]
    non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
    selected_data <-  data.frame(y = model[["y"]], data.frame(model[["x"]],
                                                              check.names = FALSE),check.names = FALSE)

    if (length(non_zero_terms) == 0) {
      selected_data <- data.frame(y)
    } else {
      selected_data <- selected_data[, c("y", non_zero_terms)]
    }

    # xbeta<- as.matrix(cbind("(Intercept)"=1, model[["x"]])) %*% model[["beta"]][["estimate"]]
    # res <- y - xbeta

    fit <- lm(y ~ ., data = selected_data)
    conf <- confint(fit)
    coefs <- coef(summary(fit))
    terms <- gsub("`", "", rownames(coefs))
    # Create full_mod data frame
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

    estimates <- full_mod$estimate
    estimates[is.na(estimates)] <- 0
    xbeta<- as.matrix(cbind("(Intercept)"=1, model[["x"]])) %*% estimates
    res <- y - xbeta

    final_mod= get_uncertain_nulls (mod= full_mod, res=res,
                                    x=data.frame(model[["x"]], check.names = FALSE))

    ci_avg_ratio  <- mean(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=   final_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_pen"
    return(result)
  }
  else if (method == "selectiveinf" && nonselection == "ignored"){

    lam=model[["lambda"]]
    std=model[["std"]]
    alpha = model[["alpha"]]


    if(sum(model[["beta"]][["estimate"]][model[["beta"]][["term"]] !="(Intercept)"]!=0) ==0){
      full_mod <- model[["beta"]]
      full_mod$estimate[full_mod$estimate==0] <- NA
      full_mod$conf.low <-  full_mod$conf.high <- full_mod$p.value<- full_mod$ci_ln <- NA

    }
    else{

      fit_lso= sel_inf(x,y,lam = lam, std=T, model=model,alpha =alpha  )

      all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, fit_lso, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      full_mod$ci_ln <- full_mod$conf.high - full_mod$conf.low
    }
    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=   full_mod ,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   infmethod = method, nonselection = nonselection,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]]
    )

    class(result) <- "infer_pen"
    return(result )
  }
  else if (method == "selectiveinf" && nonselection == "confident_nulls") {

    lam=model[["lambda"]]
    std=model[["std"]]
    alpha = model[["alpha"]]


    if(sum(model[["beta"]][["estimate"]][model[["beta"]][["term"]] !="(Intercept)"]!=0) ==0){
      full_mod <- model[["beta"]]
      full_mod$conf.low <-  full_mod$conf.high <- full_mod$p.value<- full_mod$ci_ln <- 0

    }
    else{
      fit_lso= sel_inf(x,y,lam = lam, std=std, model=model,alpha =alpha)

      all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, fit_lso, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      full_mod$estimate   <- ifelse(is.na(full_mod$estimate),   0, full_mod$estimate)
      full_mod$p.value    <- ifelse(is.na(full_mod$p.value),    1, full_mod$p.value)
      full_mod$conf.low   <- ifelse(is.na(full_mod$conf.low),   0, full_mod$conf.low)
      full_mod$conf.high  <- ifelse(is.na(full_mod$conf.high),  0, full_mod$conf.high)
      full_mod$ci_ln <- full_mod$conf.high - full_mod$conf.low
    }
    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=   full_mod ,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_pen"
    return(result )


  }
  else if (method == "selectiveinf" && nonselection == "uncertain_nulls"){

    lam=model[["lambda"]]
    std=model[["std"]]
    alpha = model[["alpha"]]

    if(sum(model[["beta"]][["estimate"]][model[["beta"]][["term"]] !="(Intercept)"]!=0) ==0){
      full_mod <- model[["beta"]]
      full_mod$estimate[full_mod$estimate==0] <- NA
      full_mod$conf.low <-  full_mod$conf.high <- full_mod$p.value<-  0
      full_mod <- full_mod [full_mod$term != "(Intercept)",]

    }
    else{

      fit_si= sel_inf(x,y,lam = lam, std=std, model=model,  alpha =  alpha )
      all_terms <- data.frame(term = model[["beta"]][["term"]], stringsAsFactors = FALSE)
      full_mod <-merge(all_terms, fit_si, by = "term", all.x = TRUE, sort = FALSE)
      matched_rows <- match(all_terms$term, full_mod$term)
      full_mod <- full_mod[matched_rows, ]
      full_mod <- cbind(all_terms, full_mod[ , setdiff(names(full_mod), "term")])
      full_mod <- full_mod [full_mod$term != "(Intercept)",]

    }

    xbeta<- as.matrix(cbind("(Intercept)"=1, model[["x"]])) %*% model[["beta"]][["estimate"]]
    res <- y - xbeta


    non_zero_terms  <- model[["beta"]]$term[model[["beta"]]$estimate != 0]
    non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
    selected_data <-  data.frame(y = model[["y"]], data.frame(model[["x"]],
                                                              check.names = FALSE),check.names = FALSE)
    if (length(non_zero_terms) == 0) {
      selected_data <- data.frame(y)
    } else {
      selected_data <- selected_data[, c("y", non_zero_terms)]
    }


    fit <- lm(y ~ ., data = selected_data)
    conf <- confint(fit)
    coefs <- coef(summary(fit))
    terms <- gsub("`", "", rownames(coefs))
    ols_mod <- data.frame(
      term = terms,
      estimate = coefs[, "Estimate"],
      std.error = coefs[,"Std. Error"],
      p.value = coefs[, "Pr(>|t|)"],
      conf.low = conf[, 1],
      conf.high = conf[, 2],
      stringsAsFactors = FALSE
    )
    rownames(ols_mod) <- NULL

    ols_intercept <- ols_mod [ols_mod $term == "(Intercept)",
                              c("term", "estimate",  "conf.low", "conf.high","p.value")]
    ols_intercept <- as.data.frame(ols_intercept, check.names = FALSE)


    si_mod_intercept <-  rbind(ols_intercept ,full_mod )
    si_mod_intercept$std.error <- NA
    si_mod_intercept <- si_mod_intercept [, c("term", "estimate", "std.error", "p.value","conf.low", "conf.high")]
    final_si = get_uncertain_nulls (mod= si_mod_intercept , res=res, x=data.frame(model[["x"]], check.names = FALSE))
    final_si= final_si[, c("term", "estimate", "conf.low","conf.high", "p.value", "selected",  "ci_ln",
                           "na_coeff" )]



    ci_avg_ratio  <- mean(final_si$ci_ln[final_si$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(final_si$ci_ln[final_si$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  final_si,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio  ,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_pen"
    result
  }
  else if (method == "PIPE") {

    stopifnot(class(model$model) %in% c("ncvreg", "cv.ncvreg"))
    warning("PIPE method experimental")

    pipe_results <- ncvreg::intervals(model$model)

    # ci_avg_ratio  <- mean(pipe_results$ci_ln[pipe_results$term != "(Intercept)"] , na.rm=T)
    # ci_median_ratio <-  median(pipe_results$ci_ln[pipe_results$term != "(Intercept)"] , na.rm=T)

    result <- list(model=  pipe_results,
                   # ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],
                   lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method,
                   nonselection = "N/A")
    class(result) <- "infer_pen"
    result
  }
  else if (method == "boot") {
    result <- boot(model, B=B, nonselection=nonselection,
                   n_cores= n_cores, save_beta=save_beta,
                   boot_desparse=boot_desparse,
                   infmethod = method)


    result$ci_avg_ratio  <- mean(result$model$ci_ln[result$model$term != "(Intercept)"] , na.rm=T)
    result$ci_median_ratio <-  median(result$model$ci_ln[result$model$term != "(Intercept)"] , na.rm=T)

    class(result) <- "infer_pen"
    return(result)
  }
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
#' @param n_cores should a cluster with detectCores()-1 be made and used
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
                              n_cores = 1, boot_desparse=FALSE,
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

  if(n_cores > 1) {
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

  results$lambda <- model$lambda
  results$infmethod <- "boot"
  results
}

#' Title
#'
#' @param x x model of class `infer_pen`
#' @return returns x invisibly
#' @export

print.infer_pen <- function(x, ...) {
  if (x[["alpha"]] ==1) {
    var_method <- c("lasso")
  } else{
    var_method <- c("elastic Net")
  }

  # Penalty used for selection
  cat("Selection method: ", var_method, ".  ","Choice of lambda: " ,x[["lambda"]], "\n", sep = "")

  if(x$infmethod == "boot")
    x$infmethod <- paste0("Bootstrap (B=", x$B,")")
  cat("Inference method: ", x[["infmethod"]], "\n", sep = "")

  cat ("Method for handling null: ", x[["nonselection"]], "\n", sep = "")

  # Average CI length
  cat ("Average confidence intervals length: ", x[["ci_avg_ratio"]],"\n", sep = "")

  # Median CI length
  cat ("Median confidence intervals length: ", x[["ci_median_ratio"]],"\n", sep = "")

  invisible(x)
}


#' Title
#' @param x model of class `infer_pen`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export

tidy.infer_pen <- function(x, ...) {

  ret<- as_tibble(x[["model"]])
  return(ret)
}

