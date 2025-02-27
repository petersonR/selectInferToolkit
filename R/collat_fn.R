
#' Computing mean/median CI ratio and significant Discoveries
#' @description
#' Perfroms full model boostrap, stepwise AIC, stepwise BIc, lasso and MCP and gives average CI for variables
#' adjusting for post-selectino inference using hybrid, bootstrap and selective inference
#' @param x Dataframe/model matrix with predictors (without intercept)
#' @param y outcome vector
#' @param nonselection  A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param std if TRUE (default), standardize design matrix
#' @param direction the mode of step wise search, can be one of "both", "backward", or "forward", with a default of "forward"
#' @param B #of bootstraps
#' @importFrom magrittr %>%
#' @importFrom broom tidy
#' @return A list with following data frames
#' \item{avg_ci_ln}{Mean of CI length across all variables in model excluding intercept}
#' \item{med_ci_ln}{Median of CI length across all variables in model excluding intercept}
#' \item{no_sign_disc}{# of significant discoveries in model excluding intercept}
#'
#' @export
#'
#'
#'


ciratio <- function(x,y,nonselection="ignored",std=TRUE, B=250,direction="forward"){

  if(is.matrix(x)){
    if(std==TRUE){
      x_std= data.frame(x, check.names = FALSE)%>%
        mutate_if(is.numeric, ~as.numeric(scale(.)))
      colnames(x_std) = colnames(x)
    } else{
      x_std= data.frame(x, check.names = FALSE)
    }
  } else if (is.data.frame(x)){
    if(std==TRUE){
      x_std = x %>%
        mutate_if(is.numeric, scale)
      colnames(x_std) = colnames(x)
      #x_dup<- model.matrix(y ~., model.frame(~ ., cbind(x_std,y), na.action=na.pass))[,-1]

    } else{
      x_std=x
      #x_dup<- model.matrix(y ~., model.frame(~ ., cbind(x,y), na.action=na.pass))[,-1]

    }
  }


  ## Full model no model selection
  raw_data = as.data.frame(cbind(x_std,y))
  full_mod <- lm(y~., data=raw_data, x= TRUE, y=TRUE)
  full_modsum <- broom::tidy(full_mod , conf.int = TRUE)[-1,] %>% dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
    mutate(ci_ln =conf.high-conf.low)
  full_ci_avg <- mean(full_modsum$ci_ln)
  full_ci_median <- median(full_modsum$ci_ln)
  full_dic<-  nrow(full_modsum)
  full_sdic<-  sum( full_modsum$p.value <0.05)


  ##  Full model bootstrap no model selection bootstrap
  nosel_boot <- full_boot(full_mod , B=B, family="gaussian",parallel = TRUE)
  nosel_boot_ci_avg <- mean(nosel_boot$ci_avg_ratio)
  nosel_boot_ci_median <- mean(nosel_boot$ci_median_ratio)
  nosel_boot_disc<- nrow(nosel_boot)
  nosel_boot_sdisc<- sum(!(nosel_boot$conf.low[nosel_boot$term != "(Intercept)"] < 0 & nosel_boot$conf.high [nosel_boot$term != "(Intercept)"]>0 ))

  ## Step wise AIC, hybrid
  aic_mod =step_ic (x=x,y=y,std = std, penalty = "AIC", direction = direction )
  aic_mod_hybrid = infer(aic_mod,method="hybrid", nonselection=nonselection)
  aichybrid_ci_avg <- aic_mod_hybrid[["ci_avg_ratio"]]
  aichybrid_ci_median <- aic_mod_hybrid[["ci_median_ratio"]]
  #aic_disc <-  nrow(aic_mod_hybrid[["model"]][,])
  aic_sdisc <- sum(aic_mod_hybrid[["model"]][["p.value"]][aic_mod_hybrid[["model"]][["term"]] != "(Intercept)"]<0.05, na.rm=T)

  ## Stepwise AIC bootstrap
  aic_mod_boot= infer(aic_mod,method="boot", nonselection=nonselection,B=B)
  aic_boot_ci_avg <- aic_mod_boot[["ci_avg_ratio"]]
  aic_boot_ci_median<- aic_mod_boot[["ci_median_ratio"]]
  #aic_boot_disc<- nrow(aic_boot)
  aic_boot_sdisc<-  sum(!(aic_mod_boot[["model"]][["conf.low"]] [aic_mod_boot[["model"]][["term"]] != "(Intercept)"] <= 0 &
                            aic_mod_boot[["model"]][["conf.high"]] [aic_mod_boot[["model"]][["term"]] != "(Intercept)"]>= 0 ), na.rm=T)

  ## stepwise AIC selective inference
  # run forward step wise and  compute p-values and confidence intervals after AIC stopping
  aic_mod_si= infer(aic_mod,method="selectiveinf", nonselection=nonselection)
  aic_si__ci_avg <- aic_mod_si[["ci_avg_ratio"]]
  aic_si_ci_median <- aic_mod_si[["ci_median_ratio"]]
  #si_aic_disc<-  nrow(fs_si_aic_ignore)
  aic_si_sdisc<- sum(aic_mod_si[["model"]][["p.value"]][aic_mod_si[["model"]][["term"]] != "(Intercept)"]<0.05, na.rm = T)

  ## Stepwise BIC hybrid
  bic_mod =step_ic (x=x,y=y,std = std, penalty = "BIC", direction = direction )
  bic_mod_hybrid = infer(bic_mod,method="hybrid", nonselection=nonselection)
  bichybrid_ci_avg <- bic_mod_hybrid[["ci_avg_ratio"]]
  bichybrid_ci_median <- bic_mod_hybrid[["ci_median_ratio"]]
  #bic_disc <-  nrow(bic_mod_hybrid[["model"]][,])
  bic_sdisc <- sum(bic_mod_hybrid[["model"]][["p.value"]][bic_mod_hybrid[["model"]][["term"]] != "(Intercept)"]<0.05, na.rm=T)


  ## Stepwise BIC boostrap
  bic_mod_boot= infer(bic_mod,method="boot", nonselection=nonselection, B=B)
  bic_boot_ci_avg <- bic_mod_boot[["ci_avg_ratio"]]
  bic_boot_ci_median<- bic_mod_boot[["ci_median_ratio"]]
  #bic_boot_disc<- nrow(bic_boot)
  bic_boot_sdisc<-  sum(!(bic_mod_boot[["model"]][["conf.low"]] [bic_mod_boot[["model"]][["term"]] != "(Intercept)"] <= 0 &
                            bic_mod_boot[["model"]][["conf.high"]] [bic_mod_boot[["model"]][["term"]] != "(Intercept)"] >= 0 ), na.rm=T)

  ## stepwise BIC selective inference
  bic_mod_si= infer(bic_mod,method="selectiveinf", nonselection=nonselection)
  bic_si_ci_avg <- bic_mod_si[["ci_avg_ratio"]]
  bic_si_ci_median <- bic_mod_si[["ci_median_ratio"]]
  #si_bic_disc<-  nrow(fs_si_bic_ignore)
  bic_si_sdisc <- sum(bic_mod_si[["model"]][["p.value"]][bic_mod_si[["model"]][["term"]] != "(Intercept)"]<0.05, na.rm = T)


  # Lasso min
  lso_min <- pen_cv(y = y, x = x, std=std,lambda="lambda.min")
  fit_lasso_hybrid <-infer(lso_min, method = "hybrid",nonselection=nonselection)
  lassomin_hybrid_ci_avg <- fit_lasso_hybrid[["ci_avg_ratio"]]
  lassomin_hybrid_ci_median <-fit_lasso_hybrid[["ci_median_ratio"]]
  #lassomin_ols_disc<-  nrow( fit_lso_mincv )
  lassomin_hybrid_sdisc<- sum(fit_lasso_hybrid [["model"]][["p.value"]][fit_lasso_hybrid [["model"]][["term"]] != "(Intercept)"]<0.05, na.rm=T)

  ## Lasso bootstrap
  boot_lasso =infer(lso_min, method = "boot",nonselection=nonselection, B=B)
  lassomin_boot_ci_avg <- boot_lasso[["ci_avg_ratio"]]
  lassomin_boot_ci_median <- boot_lasso[["ci_median_ratio"]]
  #lassomin_boot_disc<-  nrow( boot_lasso_ignore)
  lassomin_boot_sdisc<-  sum(!(boot_lasso[["model"]][["conf.low"]] [boot_lasso[["model"]][["term"]] != "(Intercept)"] <= 0 &
                                 boot_lasso[["model"]][["conf.high"]] [boot_lasso[["model"]][["term"]] != "(Intercept)"]>= 0 ), na.rm=T)


  ## Lasso min selective inference
  lassomin_si= infer(lso_min,method="selectiveinf", nonselection=nonselection)
  lassomin_si_ci_avg <- lassomin_si[["ci_avg_ratio"]]
  lassomin_si_ci_median <- lassomin_si[["ci_median_ratio"]]
  #si_lassomin_disc<-  nrow(lasso_si_mod )
  lassomin_si_ci_sdisc<- sum(  lassomin_si[["model"]][["p.value"]][lassomin_si[["model"]][["term"]] != "(Intercept)"]<0.05, na.rm = T)

  # LASSO 1Se
  lso_se <- pen_cv(y = y, x = x, lambda= "lambda.1se", std=std)
  fit_lassose_hybrid <-infer(lso_se, method = "hybrid",nonselection=nonselection)
  lassose_hybrid_ci_avg <- fit_lassose_hybrid [["ci_avg_ratio"]]
  lassose_hybrid_ci_median <-fit_lassose_hybrid [["ci_median_ratio"]]
  lassose_hybrid_sdisc <- sum(fit_lassose_hybrid [["model"]][["p.value"]][fit_lassose_hybrid [["model"]][["term"]] != "(Intercept)"]<0.05, na.rm=T)

  ## Lasso 1sebootstrap
  boot_lassose =infer(lso_se, method = "boot",nonselection=nonselection, B=B)
  lassose_boot_ci_avg <- boot_lassose[["ci_avg_ratio"]]
  lassose_boot_ci_median <- boot_lassose[["ci_median_ratio"]]
  lassose_boot_sdisc<-  sum(!(boot_lassose[["model"]][["conf.low"]] [boot_lassose[["model"]][["term"]] != "(Intercept)"] <= 0 &
                                 boot_lassose[["model"]][["conf.high"]] [boot_lassose[["model"]][["term"]] != "(Intercept)"]>=0 ), na.rm=T)

  ## Lasso 1se selective inference
  lassose_si= infer(lso_se,method="selectiveinf", nonselection=nonselection)
  lassose_si_ci_avg <- lassose_si[["ci_avg_ratio"]]
  lassose_si_ci_median <- lassose_si[["ci_median_ratio"]]
  #si_lassomin_disc<-  nrow(lasso_si_mod )
  lassomin_si_ci_sdisc<- sum(  lassose_si[["model"]][["p.value"]][lassose_si[["model"]][["term"]] != "(Intercept)"]<0.05, na.rm = T)

  # MCP min
  mcp_min <- pen_cv(y = y, x = x, std=std,lambda="lambda.min", penalty= "MCP" )
  fit_mcp_hybrid <-infer(mcp_min, method = "hybrid",nonselection=nonselection)
  mcpmin_hybrid_ci_avg <- fit_mcp_hybrid[["ci_avg_ratio"]]
  mcpmin_hybrid_ci_median <-fit_mcp_hybrid[["ci_median_ratio"]]
  #mcpmin_ols_disc<-  nrow( fit_lso_mincv )
  mcpmin_hybrid_sdisc<- sum(fit_mcp_hybrid [["model"]][["p.value"]][fit_mcp_hybrid [["model"]][["term"]] != "(Intercept)"]<0.05, na.rm=T)

  ## MCP bootstrap
  boot_mcp =infer( mcp_min, method = "boot",nonselection=nonselection, B=B)
  mcpmin_boot_ci_avg <- boot_mcp[["ci_avg_ratio"]]
  mcpmin_boot_ci_median <- boot_mcp[["ci_median_ratio"]]
  #mcpmin_boot_disc<-  nrow( boot_mcp_ignore)
  mcpmin_boot_sdisc <-  sum(!(boot_mcp[["model"]][["conf.low"]] [boot_mcp[["model"]][["term"]] != "(Intercept)"] <= 0 &
                                 boot_mcp[["model"]][["conf.high"]] [boot_mcp[["model"]][["term"]] != "(Intercept)"]>=0 ), na.rm=T)

  # MCP 1Se hybrid
  mcp_se <- pen_cv(y = y, x = x, lambda= "lambda.1se", std=std, penalty= "MCP")
  fit_mcpse_hybrid <-infer(mcp_se, method = "hybrid",nonselection=nonselection)
  mcpse_hybrid_ci_avg <- fit_mcpse_hybrid [["ci_avg_ratio"]]
  mcpse_hybrid_ci_median <-fit_mcpse_hybrid [["ci_median_ratio"]]
  mcpse_hybrid_sdisc <- sum(fit_mcpse_hybrid [["model"]][["p.value"]][fit_mcpse_hybrid [["model"]][["term"]] != "(Intercept)"]<0.05, na.rm=T)

  ## MCP 1sebootstrap
  boot_mcpse =infer(mcp_se, method = "boot",nonselection=nonselection, B=B)
  mcpse_boot_ci_avg <- boot_mcpse[["ci_avg_ratio"]]
  mcpse_boot_ci_median <- boot_mcpse[["ci_median_ratio"]]
  #mcpmin_boot_disc<-  nrow( boot_mcp_ignore)
  mcpse_boot_sdisc<-  sum(!(boot_mcpse[["model"]][["conf.low"]] [boot_mcpse[["model"]][["term"]] != "(Intercept)"] <= 0 &
                                boot_mcpse[["model"]][["conf.high"]] [boot_mcpse[["model"]][["term"]] != "(Intercept)"]>=0 ),na.rm=T)

  # return full results in table
  # CI length mean
  full_ci<- c(full_ci_avg ,nosel_boot_ci_avg, NA)
  stepwiseaic_ci<- c(aichybrid_ci_avg,aic_boot_ci_avg ,aic_si__ci_avg)
  stepwisebic_ci<- c(bichybrid_ci_avg, bic_boot_ci_avg ,bic_si_ci_avg  )
  lassomin_ci <- c(lassomin_hybrid_ci_avg,lassomin_boot_ci_avg,  lassomin_si_ci_avg )
  lassose_ci <- c(lassose_hybrid_ci_avg,lassose_boot_ci_avg,lassose_si_ci_avg)
  mcpmin_ci <- c(mcpmin_hybrid_ci_avg ,mcpmin_boot_ci_avg  ,NA)
  mcpse_ci <- c(mcpse_hybrid_ci_avg ,mcpse_boot_ci_avg ,NA)



  # CI length median
  full_ci_m <- c(full_ci_median, nosel_boot_ci_median, NA )
  stepwiseaic_ci_m<- c(aichybrid_ci_median,aic_boot_ci_median, aic_si_ci_median)
  stepwisebic_ci_m<- c(bichybrid_ci_median, bic_boot_ci_median, bic_si_ci_median)
  lassomin_ci_m <- c( lassomin_hybrid_ci_median,lassomin_boot_ci_median,lassomin_si_ci_median  )
  lassose_ci_m <- c(lassose_hybrid_ci_median,lassose_boot_ci_median, lassose_si_ci_median )
  mcpmin_ci_m <- c(mcpmin_hybrid_ci_median,mcpmin_boot_ci_median   ,NA)
  mcpse_ci_m <- c(mcpse_hybrid_ci_median,mcpse_boot_ci_median,NA)

  # total significant discoveries
  full_sdisc<- c(full_sdic,nosel_boot_sdisc,NA)
  stepwiseaic_sdisc<- c(aic_sdisc,aic_boot_sdisc,aic_si_sdisc)
  stepwisebic_sdisc<- c(bic_sdisc,bic_boot_sdisc,bic_si_sdisc)
  lassomin_sdisc <- c(lassomin_hybrid_sdisc,lassomin_boot_sdisc,lassomin_si_ci_sdisc)
  lassose_sdisc <- c( lassose_hybrid_sdisc, lassose_boot_sdisc,lassomin_si_ci_sdisc)
  mcpmin_sdisc<-c(mcpmin_hybrid_sdisc,mcpmin_boot_sdisc,NA)
  mcpse_sdisc<-c(mcpse_hybrid_sdisc ,mcpse_boot_sdisc,NA)



  ci_results <- rbind(full_ci,stepwiseaic_ci,stepwisebic_ci,lassomin_ci,lassose_ci,mcpmin_ci, mcpse_ci )
  rownames(ci_results)<- c("Full Model", "Stepwise AIC", "Stepwise BIC",  "Lasso CV (min)", "Lasso CV (se)","MCP CV (min)", "MCP CV (se)")
  colnames(ci_results) <- c("Hybrid OLS", "Bootstrap",  "Selective Inference")

  ci_results_med <- rbind(full_ci_m,stepwiseaic_ci_m,stepwisebic_ci_m,lassomin_ci_m ,lassose_ci_m ,mcpmin_ci_m, mcpse_ci_m )
  rownames(ci_results_med)<- c("Full Model", "Stepwise AIC", "Stepwise BIC",  "Lasso CV (min)", "Lasso CV (se)","MCP CV (min)", "MCP CV (se)")
  colnames(ci_results_med) <- c("Hybrid OLS", "Bootstrap",  "Selective Inference")

  sdic_results<- rbind(full_sdisc, stepwiseaic_sdisc,stepwisebic_sdisc,lassomin_sdisc, lassose_sdisc, mcpmin_sdisc, mcpse_sdisc  )
  rownames(sdic_results)<- c("Full Model", "Stepwise AIC", "Stepwise BIC",  "Lasso CV (min)", "Lasso CV (se)","MCP CV (min)", "MCP CV (se)")
  colnames(sdic_results) <- c("Hybrid OLS", "Bootstrap", "Selective Inference")

  # Combine the three data frames into a list
  results_list <- list(
    avg_ci_ln = ci_results,
    med_ci_ln = ci_results_med,
    no_sign_disc = sdic_results
  )

  results_list

}
