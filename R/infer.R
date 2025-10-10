

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
#' @method infer selector_ic
#'
#' @export

infer.selector_ic <- function(
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
             boot_desparse=boot_desparse)
    class(result) <- "infer_boot_selector_pen"
    return(result)
  }
}

#' Inference for selector_ic class based on bootstrap
#' other methods are supposed in infer.selector_ic
#'
#' @param model model returned from selector_ic  class
#' @param B  Number of bootstraps samples
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param n_cores  Whether to run bootstrap in parallel or not
#' @param ... Additional arguments
#' @return "boot_ic" class list with
#'   \item{model}{A dataframe with model results including columns for term, mean_estimate,  conf.low, conf.high, median_p.value,ci_ln, prop.select, prop.reject}
#'   \item{ci_avg_ratio}{Average CI length across all variables in model}
#'   \item{ci_median_ratio}{median CI length across all variables in model}
#'   \item{selection_method}{Stepwsie}
#'   \item{direction}{selected the mode of step wise search}
#'   \item{penalty}{penalty used (AIC or BIC)}
#'   \item{infmethod}{Inference method chosen}
#'   \item{nonselection}{method chosen  to deal with non selection}
#'   \item{B}{The number of bootstrap replicates used}
#'
#' @rdname infer
#' @method infer boot_selector_ic
#' @export
#'
infer.boot_selector_ic  <- function(model, B = 250,
                                    nonselection=c("ignored", "uncertain", "confident_nulls"),
                                    n_cores= 1,
                                    make_levels=FALSE,
                                    save_beta =FALSE,  ...){
  direction=model[["direction"]]
  x <-model[["x_model"]]
  y <- model[["y"]]
  make_levels=model[["make_levels"]]
  nonselection <- match.arg(nonselection)
  penalty <- model$penalty
  direction <- model$direction
  family <- model$family

  results <- boot(model, B=B, nonselection = nonselection, ic = penalty,
                  n_cores = n_cores, make_levels = make_levels,
                  save_beta = save_beta, ...)


  ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
  ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)

  result <- list(model =  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                     selection_method="stepwise", direction = direction, penalty= penalty,
                     nonselection = nonselection, B=B)
  class(result) <- "infer_boot_selector_ic"
  result
}


#' Title Inference for selector_pen class based on bootstrap, other methods are supposed in infer.selector_pen

#'
#' @param model model of selector_pen class returned from  pen_cv function
#' @param B  The number of bootstrap replicates.
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param n_cores whether to run bootstrap in parallel for faster computation
#' @param ...
#'
#' @return "infer_boot_selector_pen" class list with
#' \item{model}{A dataframe with model results including columns for term, mean_estimate,  conf.low, conf.high,ci_ln, prop.select}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{median CI length across all variables in model}
#' \item{selection_method}{lasso' or 'MCP' Note: Same as in ncvreg, even with elasticnet-it says lasso but can be differntiated with value of alpha}
#' \item{lambda}{selected lambda for inference , either "lambda.min" or "lambda.1se"}
#' \item{alpha}{selected alpha for inference}
#' \item{infmethod}{Inference method chosen}
#' \item{nonselection}{method chosen  to deal with non selection}
#' \item{B}{The number of bootstrap replicates used}
#' @export
#'
#' @rdname infer
#' @method infer boot_selector_pen
#' @export

infer.boot_selector_pen <-function(model, B, nonselection, n_cores = 1, save_beta = FALSE, ...){

  results <- boot(model, B=B, nonselection = nonselection,  n_cores= n_cores, save_beta =save_beta, ... )

  ci_avg_ratio  <- mean(results_df$ci_ln[results_df$term != "(Intercept)"] , na.rm=T)
  ci_median_ratio <-  median(results_df$ci_ln[results_df$term != "(Intercept)"] , na.rm=T)

  result <- list(model=  results, B=B, ci_avg_ratio = ci_avg_ratio , ci_median_ratio =ci_median_ratio,
                 selection_method=model[["penalty"]], lambda= model[["lambda"]], alpha=model[["alpha"]],
                 nonselection = nonselection)
  class(result) <- "infer_boot_selector_pen"
  result
}

#' Post-selection inference
#'
#' @param model model of selector_ic or selector_pen class returned from step_ic or pen_cv function
#' @param method A character string specifying method of post-selection inference.Currently "hybrid", "selectiveinf" or
#' "boot" supported
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#'
#' @return A list of class `infer_ic` or `infer_pen` containing:#'
#' \item{model}{A data frame with post-selection inference results}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{median CI length across all variables in model}
#' \item{nonselection}{method chosen  to deal with non selection}
#' \item{infmethod}{Inference method chosen}
#' \item{selection_method}{Stepwsie,returned for selector_ic class only}
#' \item{direction}{the mode of step wise search, returned for selector_ic class only}
#' \item{penalty}{penalty used (AIC or BIC), returned for selector_ic class only}
#' \item{lambda}{selected lambda for inference , either "lambda.min" or "lambda.1se"; returned for selector_pen class only}
#' \item{alpha}{selected alpha for inference, returned for selector_pen class only}
#' \item{B}{The number of bootstrap replicates used (only for bootstrap selection method)}
#'
#' @importFrom broom tidy
#' @export
#'
#'
#'


infer <- function(model, method=c("hybrid", "boot", "selectiveinf", "PIPE"),
                          nonselection=c("ignored", "uncertain_nulls", "confident_nulls"),...){

  method <- match.arg(method)
  nonselection <- match.arg(nonselection)

  UseMethod("infer")
}




#' Title
#' @param x model of class `infer_ic`
#' @return returns x invisibly
#' @export
print.infer_ic <- function(x, ...) {

    # Penalty used for selection
    cat("Selection method: ", x[["selection_method"]], "  ", x[["penalty"]],".  Direction: " ,x[["direction"]], "\n", sep = "")

    # lambda
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
#' @param x model of class `boot_ic`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export
print.infer_boot_selector_ic <- function(x, ...) {

 # Penalty used for selection
    cat("Selection method: ", x[["selection_method"]], "  ",x[["penalty"]],".  Direction: " ,x[["direction"]], "\n", sep = "")

    # lambda
    cat("Inference method: ", x[["infmethod"]], "with ", x[["B"]],"bootstrap samples","\n", sep = "")

    cat ("Method for handling null: ", x[["nonselection"]], "\n", sep = "")

    # Average CI length
    cat ("Average confidence interval length: ", x[["ci_avg_ratio"]], "\n", sep = "")

    # Median CI length
    cat ("Median confidence interval length: ", x[["ci_median_ratio"]], "\n", sep = "")
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

  # lambda
  cat("Inference method: ", x[["infmethod"]], "\n", sep = "")

  cat ("Method for handling null: ", x[["nonselection"]], "\n", sep = "")

  # Average CI length
  cat ("Average confidence intervals length ", x[["ci_avg_ratio"]],"\n", sep = "")

  # Median CI length
  cat ("Median confidence intervals length ", x[["ci_median_ratio"]],"\n", sep = "")

  # # Model coefficients
  # coeff <-c(x[["model"]][["estimate"]])
  # names(coeff)<-x[["model"]][["term"]]
  #
  # cat("\nFinal Model Results:\n")
  # print( coeff)

}


#' Title
#' @param x x model of class `infer_boot_selector_pen`
#' @return returns x invisibly
#' @export
print.infer_boot_selector_ic <- function(x, ...) {
  if (x[["alpha"]] ==1) {
    var_method <- c("lasso")
  } else{
    var_method <- c("elastic Net")
  }

  # Penalty used for selection
  cat("Selection method: ", var_method, ".  ","Choice of lambda: " ,x[["lambda"]], "\n", sep = "")

  # lambda
  cat("Inference method: ", x[["infmethod"]], "with ", x[["B"]],"bootstrap samples","\n", sep = "")

  cat ("Method for handling null: ", x[["nonselection"]], "\n", sep = "")

  # Average CI length
  cat ("Average confidence intervals length ", x[["ci_avg_ratio"]],"\n", sep = "")

  # Median CI length
  cat ("Median confidence intervals length ", x[["ci_median_ratio"]],"\n", sep = "")

}


#' Title
#' @param x model of class `infer_ic`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom  tidy
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.infer_ic <- function(x, ...) {

  ret<- as_tibble(x[["model"]])
  return(ret)
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

#' Title
#' @param x model of class `infer_boot_selector_ic`
#' @importFrom tibble as_tibble
#' @importFrom broom tidy
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.infer_boot_selector_ic <- function(x, ...) {
  return(as_tibble(x$results))
}

#' Tidy for bootstrapped post-regularized inference objects
#'
#' @param x model of class `infer_boot_selector_pen`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.infer_boot_selector_pen<- function(x, ...) {
  return(as_tibble(x$results))
}
