#' Inference for selector_pen_cv class except bootstrap which is it's own function
#'
#' @param model model of selector_pen_cv class returned from  selector_pen_cv function
#' @param method A character string specifying method of post-selection inference. Currently "hybrid", "selectiveinf" or
#' "boot" supported
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @return "infer_pen_cv " class list with
#' @importFrom broom tidy
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @rdname infer
#' @export
#'
#'

# NEEDS WORK - this is a placeholder
infer_pipe <- function(model, method = "hybrid", nonselection = "ignored",
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
    class(result) <- "infer_pen_cv "
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
    class(result) <- "infer_pen_cv "
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
    class(result) <- "infer_pen_cv "
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

    class(result) <- "infer_pen_cv "
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
    class(result) <- "infer_pen_cv "
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
    class(result) <- "infer_pen_cv "
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
    class(result) <- "infer_pen_cv "
    result
  }
}

