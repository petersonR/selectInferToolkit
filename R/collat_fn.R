
#' Computing mean/median CI ratio and significant Discoveries
#' @description
#' Perfroms full model boostrap, stepwise AIC, stepwise BIc, lasso and MCP and gives average CI for variables
#' adjusting for post-selectino inference using hybrid, bootstrap and selective inference
#' @param formula a formula
#' @param data data set
#' @param family outcome distributional family
#' @param nonselection  A character string specifying how to handle variables
#'   not selected by model selection procedure. One of "ignored",
#'   "confident_nulls" or "uncertain_nulls" supported
#' @param conf.level .95 by default
#' @param B Number of bootstraps
#' @param direction the mode of step wise search, can be one of "both", "backward", or "forward", with a default of "forward"
#' @param select_factors_together should categorical variables be jointly selected?
#' @param debias should estimates be debiased in bootstrap
#' @param inference_target is inference requested on all or selected only
#' @param ... additional arguments
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


ciratio <- function(formula, data,
                    family = c("gaussian", "binomial", "poisson"),
                    nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
                    conf.level = .95,
                    B=250,direction="forward",
                    select_factors_together =F,
                    debias=F,
                    inference_target ="selections",
                    ...){


  ## Full model no model selection
  fullmod <- select_full_model(formula = formula, data=data, ...)
  full_modsum <- tidy(infer_upsi(fullmod, data =data))
  full_ci_avg <- mean(full_modsum$ci_high[full_modsum$term != "(Intercept)"]-
                        full_modsum$ci_low[full_modsum$term != "(Intercept)"])
  full_ci_median <- median(full_modsum$ci_high[full_modsum$term != "(Intercept)"]-
                             full_modsum$ci_low[full_modsum$term != "(Intercept)"])
  full_dic<-  sum( !is.na( full_modsum $estimate[full_modsum$term != "(Intercept)"]))
  full_sdic<-  sum( full_modsum$p_value[full_modsum$term !="(Intercept)"] <0.05)


  ##  Full model bootstrap no model selection bootstrap
  nosel_boot <- infer_boot(fullmod , data=data, B=B, family="gaussian",parallel = FALSE)
  nosel_bootsum <- tidy( nosel_boot)
  nosel_boot_ci_avg <- mean(nosel_bootsum$ci_high[nosel_bootsum $term != "(Intercept)"] -
                              nosel_bootsum$ci_low[nosel_bootsum $term != "(Intercept)"])
  nosel_boot_ci_median <- median(nosel_bootsum$ci_high[nosel_bootsum $term != "(Intercept)"] -
                                   nosel_bootsum$ci_low[nosel_bootsum $term != "(Intercept)"])
  nosel_boot_disc<- sum( !is.na(   nosel_bootsum$estimate[nosel_bootsum $term != "(Intercept)"]))
  nosel_boot_sdisc<- sum(!(nosel_bootsum$ci_low[nosel_bootsum$term != "(Intercept)"] <= 0 &
                             nosel_bootsum$ci_high [nosel_bootsum$term != "(Intercept)"]>=0 ))

  ## Step wise AIC, UPSI
  aic_mod <- select_stepwise_ic(formula = formula, data=data,penalty= "AIC",
                            direction=direction, select_factors_together =  select_factors_together)
  aic_mod_upsi= tidy(infer_upsi( aic_mod, data = data, nonselection = nonselection))
  aicupsi_ci_avg <-  mean( aic_mod_upsi$ci_high[aic_mod_upsi$term!="(Intercept)"] -
                            aic_mod_upsi$ci_low[aic_mod_upsi$term!="(Intercept)"], na.rm = T)
  aicupsi_ci_median <- median( aic_mod_upsi$ci_high[aic_mod_upsi$term!="(Intercept)"] -
                                 aic_mod_upsi$ci_low[aic_mod_upsi$term!="(Intercept)"], na.rm = T)
  aicupsi_disc <- sum( aic_mod_upsi$selected[aic_mod_upsi$term!="(Intercept)"]==1 )
  aicupsi_sdisc <- sum( aic_mod_upsi$p_value[aic_mod_upsi$term !="(Intercept)"] <0.05, na.rm=T)

  ## Step wise AIC bootstrap
  aic_mod_boot= tidy( infer_boot(  aic_mod, data = data, B = B, debias = debias,
                                   inference_target = inference_target))
  aicboot_ci_avg <-  mean(  aic_mod_boot$ci_high[aic_mod_boot$term!="(Intercept)"] -
                              aic_mod_boot$ci_low[aic_mod_boot$term!="(Intercept)"], na.rm=T)
  aicboot_ci_median<- median(  aic_mod_boot$ci_high[aic_mod_boot$term!="(Intercept)"] -
                                 aic_mod_boot$ci_low[aic_mod_boot$term!="(Intercept)"], na.rm=T)
  aicboot_disc<-  sum( aic_mod_boot$selected[aic_mod_boot$term!="(Intercept)"]==1 )
  aicboot_sdisc<- sum(!(aic_mod_boot$ci_low[aic_mod_boot$term != "(Intercept)"] <= 0 &
                          aic_mod_boot$ci_high [aic_mod_boot$term != "(Intercept)"]>=0 ), na.rm = T)

  ## step wise AIC selective inference
  aic_mod_si= tidy( infer_selective( aic_mod, data = data, nonselection = nonselection))
  aicsi_ci_avg <- mean( aic_mod_si$ci_high[aic_mod_upsi $term != "(Intercept)"] -
                          aic_mod_si$ci_low[aic_mod_upsi $term != "(Intercept)"], na.rm = T)
  aicsi_ci_median <- median( aic_mod_si$ci_high[aic_mod_upsi $term != "(Intercept)"] -
                               aic_mod_si$ci_low[aic_mod_upsi $term != "(Intercept)"], na.rm = T)
  aicsi_disc <- sum( aic_mod_si$selected[aic_mod_si$term!="(Intercept)"]==1 )
  aicsi_sdisc<- sum( aic_mod_si$p_value[aic_mod_si$term !="(Intercept)"] <0.05, na.rm=T)

  ## Step wise bic, UPSI
  bic_mod <- select_stepwise_ic(formula = formula, data=data,penalty= "BIC",
                                direction=direction, select_factors_together =  select_factors_together)
  bic_mod_upsi= tidy(infer_upsi( bic_mod, data = data, nonselection = nonselection))
  bicupsi_ci_avg <-  mean( bic_mod_upsi$ci_high[bic_mod_upsi$term!="(Intercept)"] -
                             bic_mod_upsi$ci_low[bic_mod_upsi$term!="(Intercept)"], na.rm = T)
  bicupsi_ci_median <- median( bic_mod_upsi$ci_high[bic_mod_upsi$term!="(Intercept)"] -
                                 bic_mod_upsi$ci_low[bic_mod_upsi$term!="(Intercept)"], na.rm = T)
  bicupsi_disc <- sum( bic_mod_upsi$selected[bic_mod_upsi$term!="(Intercept)"]==1 )
  bicupsi_sdisc <- sum( bic_mod_upsi$p_value[bic_mod_upsi$term !="(Intercept)"] <0.05, na.rm=T)

  ## Step wise bic bootstrap
  bic_mod_boot= tidy( infer_boot(  bic_mod, data = data, B = B, debias = debias,
                                   inference_target = inference_target))
  bicboot_ci_avg <-  mean(  bic_mod_boot$ci_high[bic_mod_boot$term!="(Intercept)"] -
                              bic_mod_boot$ci_low[bic_mod_boot$term!="(Intercept)"], na.rm=T)
  bicboot_ci_median<- median(  bic_mod_boot$ci_high[bic_mod_boot$term!="(Intercept)"] -
                                 bic_mod_boot$ci_low[bic_mod_boot$term!="(Intercept)"], na.rm=T)
  bicboot_disc<-  sum( bic_mod_boot$selected[bic_mod_boot$term!="(Intercept)"]==1 )
  bicboot_sdisc<- sum(!(bic_mod_boot$ci_low[bic_mod_boot$term != "(Intercept)"] <= 0 &
                          bic_mod_boot$ci_high [bic_mod_boot$term != "(Intercept)"]>=0 ), na.rm = T)

  ## step wise bic selective inference
  bic_mod_si= tidy( infer_selective( bic_mod, data = data, nonselection = nonselection))
  bicsi_ci_avg <- mean( bic_mod_si$ci_high[bic_mod_upsi $term != "(Intercept)"] -
                          bic_mod_si$ci_low[bic_mod_upsi $term != "(Intercept)"], na.rm = T)
  bicsi_ci_median <- median( bic_mod_si$ci_high[bic_mod_upsi $term != "(Intercept)"] -
                               bic_mod_si$ci_low[bic_mod_upsi $term != "(Intercept)"], na.rm = T)
  bicsi_disc <- sum( bic_mod_si$selected[bic_mod_si$term!="(Intercept)"]==1 )
  bicsi_sdisc<- sum( bic_mod_si$p_value[bic_mod_si$term !="(Intercept)"] <0.05, na.rm=T)


  # Lasso min
  lso_min <- select_glmnet(formula = formula, data=data, lambda="best")
  lso_min_upsi <- tidy(infer_upsi(lso_min, data = data,nonselection = nonselection))
  lsominupsi_ci_avg <-  mean(lso_min_upsi$ci_high[lso_min_upsi$term!="(Intercept)"] -
                               lso_min_upsi$ci_low[lso_min_upsi$term!="(Intercept)"], na.rm = T)
  lsominupsi_ci_median <- median( lso_min_upsi$ci_high[lso_min_upsi$term!="(Intercept)"] -
                                    lso_min_upsi$ci_low[lso_min_upsi$term!="(Intercept)"], na.rm = T)
  lsominupsi_disc <- sum( lso_min_upsi$selected[lso_min_upsi$term!="(Intercept)"]==1 )
  lsominupsi_sdisc <- sum( lso_min_upsi$p_value[lso_min_upsi$term !="(Intercept)"] <0.05, na.rm=T)

  ## Lasso bootstrap
  lso_min_boot= tidy( infer_boot( lso_min , data = data, B = B, debias = debias,
                                   inference_target = inference_target))
  lsominboot_ci_avg <-  mean( lso_min_boot$ci_high[lso_min_boot$term!="(Intercept)"] -
                              lso_min_boot$ci_low[lso_min_boot$term!="(Intercept)"], na.rm=T)
  lsominboot_ci_median<- median(  lso_min_boot$ci_high[lso_min_boot$term!="(Intercept)"] -
                                 lso_min_boot$ci_low[lso_min_boot$term!="(Intercept)"], na.rm=T)
  lsominboot_disc<-  sum( lso_min_boot$selected[lso_min_boot$term!="(Intercept)"]==1 )
  lsominboot_sdisc<- sum(!(lso_min_boot$ci_low[lso_min_boot$term != "(Intercept)"] <= 0 &
                          lso_min_boot$ci_high [lso_min_boot$term != "(Intercept)"]>=0 ), na.rm = T)

  ## Lasso min selective inference
  lso_min_si= tidy( infer_selective(  lso_min, data = data, nonselection = nonselection))
  lsominsi_ci_avg <- mean( lso_min_si$ci_high[lso_min_si$term != "(Intercept)"] -
                              lso_min_si$ci_low[lso_min_si $term != "(Intercept)"], na.rm = T)
  lsominsi_ci_median <- median( lso_min_si$ci_high[lso_min_si$term != "(Intercept)"] -
                                 lso_min_si$ci_low[lso_min_si $term != "(Intercept)"], na.rm = T)
  lsominsi_disc <- sum( lso_min_si$selected[lso_min_si$term!="(Intercept)"]==1 )
  lsominsi_sdisc<- sum( lso_min_si$p_value[lso_min_si$term !="(Intercept)"] <0.05, na.rm=T)

  # Lasso se
  lso_se <- select_glmnet(formula = formula, data=data, lambda="compact")
  lso_se_upsi <- tidy(infer_upsi(lso_se, data = data,nonselection = nonselection))
  lsoseupsi_ci_avg <-  mean(lso_se_upsi$ci_high[lso_se_upsi$term!="(Intercept)"] -
                               lso_se_upsi$ci_low[lso_se_upsi$term!="(Intercept)"], na.rm = T)
  lsoseupsi_ci_median <- median( lso_se_upsi$ci_high[lso_se_upsi$term!="(Intercept)"] -
                                    lso_se_upsi$ci_low[lso_se_upsi$term!="(Intercept)"], na.rm = T)
  lsoseupsi_disc <- sum( lso_se_upsi$selected[lso_se_upsi$term!="(Intercept)"]==1 )
  lsoseupsi_sdisc <- sum( lso_se_upsi$p_value[lso_se_upsi$term !="(Intercept)"] <0.05, na.rm=T)

  ## Lasso bootstrap
  lso_se_boot= tidy( infer_boot( lso_se , data = data, B = B, debias = debias,
                                  inference_target = inference_target))
  lsoseboot_ci_avg <-  mean( lso_se_boot$ci_high[lso_se_boot$term!="(Intercept)"] -
                                lso_se_boot$ci_low[lso_se_boot$term!="(Intercept)"], na.rm=T)
  lsoseboot_ci_median<- median(  lso_se_boot$ci_high[lso_se_boot$term!="(Intercept)"] -
                                    lso_se_boot$ci_low[lso_se_boot$term!="(Intercept)"], na.rm=T)
  lsoseboot_disc<-  sum( lso_se_boot$selected[lso_se_boot$term!="(Intercept)"]==1 )
  lsoseboot_sdisc<- sum(!(lso_se_boot$ci_low[lso_se_boot$term != "(Intercept)"] <= 0 &
                             lso_se_boot$ci_high [lso_se_boot$term != "(Intercept)"]>=0 ), na.rm = T)

  ## Lasso se selective inference
  lso_se_si= tidy( infer_selective(  lso_se, data = data, nonselection = nonselection))
  lsosesi_ci_avg <- mean( lso_se_si$ci_high[lso_se_si$term != "(Intercept)"] -
                              lso_se_si$ci_low[lso_se_si $term != "(Intercept)"], na.rm = T)
  lsosesi_ci_median <- median( lso_se_si$ci_high[lso_se_si$term != "(Intercept)"] -
                                   lso_se_si$ci_low[lso_se_si $term != "(Intercept)"], na.rm = T)
  lsosesi_disc <- sum( lso_se_si$selected[lso_se_si$term!="(Intercept)"]==1 )
  lsosesi_sdisc<- sum( lso_se_si$p_value[lso_se_si$term !="(Intercept)"] <0.05, na.rm=T)



  # MCP min
  mcp_min <- select_ncvreg(formula = formula, data=data,  penalty ="MCP",...)
  mcp_min_upsi <- tidy(infer_upsi(mcp_min, data = data,nonselection = nonselection))
  mcpminupsi_ci_avg <-  mean(mcp_min_upsi$ci_high[mcp_min_upsi$term!="(Intercept)"] -
                               mcp_min_upsi$ci_low[mcp_min_upsi$term!="(Intercept)"], na.rm = T)
  mcpminupsi_ci_median <- median( mcp_min_upsi$ci_high[mcp_min_upsi$term!="(Intercept)"] -
                                    mcp_min_upsi$ci_low[mcp_min_upsi$term!="(Intercept)"], na.rm = T)
  mcpminupsi_disc <- sum( mcp_min_upsi$selected[mcp_min_upsi$term!="(Intercept)"]==1 )
  mcpminupsi_sdisc <- sum( mcp_min_upsi$p_value[mcp_min_upsi$term !="(Intercept)"] <0.05, na.rm=T)

  ## MCP bootstrap
  mcp_min_boot= tidy( infer_boot( mcp_min , data = data, B = B, debias = debias,
                                  inference_target = inference_target))
  mcpminboot_ci_avg <-  mean( mcp_min_boot$ci_high[mcp_min_boot$term!="(Intercept)"] -
                                mcp_min_boot$ci_low[mcp_min_boot$term!="(Intercept)"], na.rm=T)
  mcpminboot_ci_median<- median(  mcp_min_boot$ci_high[mcp_min_boot$term!="(Intercept)"] -
                                    mcp_min_boot$ci_low[mcp_min_boot$term!="(Intercept)"], na.rm=T)
  mcpminboot_disc<-  sum( mcp_min_boot$selected[mcp_min_boot$term!="(Intercept)"]==1 )
  mcpminboot_sdisc<- sum(!(mcp_min_boot$ci_low[mcp_min_boot$term != "(Intercept)"] <= 0 &
                             mcp_min_boot$ci_high [mcp_min_boot$term != "(Intercept)"]>=0 ), na.rm = T)

  # MCP se
  mcp_se <- select_ncvreg(formula = formula, data=data,  penalty ="MCP",   lambda = "compact", ...)
  mcp_se_upsi <- tidy(infer_upsi(mcp_se, data = data,nonselection = nonselection))
  mcpseupsi_ci_avg <-  mean(mcp_se_upsi$ci_high[mcp_se_upsi$term!="(Intercept)"] -
                               mcp_se_upsi$ci_low[mcp_se_upsi$term!="(Intercept)"], na.rm = T)
  mcpseupsi_ci_median <- median( mcp_se_upsi$ci_high[mcp_se_upsi$term!="(Intercept)"] -
                                    mcp_se_upsi$ci_low[mcp_se_upsi$term!="(Intercept)"], na.rm = T)
  mcpseupsi_disc <- sum( mcp_se_upsi$selected[mcp_se_upsi$term!="(Intercept)"]==1 )
  mcpseupsi_sdisc <- sum( mcp_se_upsi$p_value[mcp_se_upsi$term !="(Intercept)"] <0.05, na.rm=T)

  ## MCP bootstrap
  mcp_se_boot= tidy( infer_boot( mcp_se , data = data, B = B, debias = debias,
                                  inference_target = inference_target))
  mcpseboot_ci_avg <-  mean( mcp_se_boot$ci_high[mcp_se_boot$term!="(Intercept)"] -
                                mcp_se_boot$ci_low[mcp_se_boot$term!="(Intercept)"], na.rm=T)
  mcpseboot_ci_median<- median(  mcp_se_boot$ci_high[mcp_se_boot$term!="(Intercept)"] -
                                    mcp_se_boot$ci_low[mcp_se_boot$term!="(Intercept)"], na.rm=T)
  mcpseboot_disc<-  sum( mcp_se_boot$selected[mcp_se_boot$term!="(Intercept)"]==1 )
  mcpseboot_sdisc<- sum(!(mcp_se_boot$ci_low[mcp_se_boot$term != "(Intercept)"] <= 0 &
                             mcp_se_boot$ci_high [mcp_se_boot$term != "(Intercept)"]>=0 ), na.rm = T)

  # return full results in table
  # CI length mean
  full_ci<- c(full_ci_avg ,nosel_boot_ci_avg, NA)
  stepwiseaic_ci<- c(aicupsi_ci_avg,aicboot_ci_avg, aicsi_ci_avg)
  stepwisebic_ci<- c(bicupsi_ci_avg, bicboot_ci_avg ,bicsi_ci_avg )
  lassomin_ci <- c(lsominupsi_ci_avg,lsominboot_ci_avg,  lsominsi_ci_avg)
  lassose_ci <- c(lsoseupsi_ci_avg, lsoseboot_ci_avg, lsosesi_ci_avg)
  mcpmin_ci <- c(mcpminupsi_ci_avg ,mcpminboot_ci_avg ,NA)
  mcpse_ci <- c(mcpseupsi_ci_avg,mcpseboot_ci_avg ,NA)

  # CI length median
  full_ci_m <- c(full_ci_median ,nosel_boot_ci_median, NA)
  stepwiseaic_ci_m<- c(aicupsi_ci_median,aicboot_ci_median, aicsi_ci_median)
  stepwisebic_ci_m<- c(bicupsi_ci_median, bicboot_ci_median ,bicsi_ci_median)
  lassomin_ci_m <- c( lsominupsi_ci_median,lsominboot_ci_median,  lsominsi_ci_median )
  lassose_ci_m <- c(lsoseupsi_ci_median, lsoseboot_ci_median, lsosesi_ci_median)
  mcpmin_ci_m <- c(mcpminupsi_ci_median,mcpminboot_ci_median ,NA)
  mcpse_ci_m <- c(mcpseupsi_ci_median,mcpseboot_ci_median ,NA)

  # total significant discoveries
  full_sdisc<-  c(full_sdic ,nosel_boot_sdisc, NA)
  stepwiseaic_sdisc<- c(aicupsi_sdisc,aicboot_sdisc, aicsi_sdisc)
  stepwisebic_sdisc<- c(bicupsi_sdisc, bicboot_sdisc ,bicsi_sdisc)
  lassomin_sdisc <- c( lsominupsi_sdisc,lsominboot_sdisc,  lsominsi_sdisc)
  lassose_sdisc <- c(lsoseupsi_sdisc, lsoseboot_sdisc, lsosesi_sdisc)
  mcpmin_sdisc<-c(mcpminupsi_sdisc,mcpminboot_sdisc ,NA)
  mcpse_sdisc<-c(mcpseupsi_sdisc,mcpseboot_sdisc ,NA)

  ci_results <- rbind(full_ci,stepwiseaic_ci,stepwisebic_ci,lassomin_ci,lassose_ci,mcpmin_ci, mcpse_ci )
  rownames(ci_results)<- c("Full Model", "Stepwise AIC", "Stepwise BIC",  "Lasso CV (min)", "Lasso CV (se)","MCP CV (min)", "MCP CV (se)")
  colnames(ci_results) <- c("UPSI", "Bootstrap",  "Selective Inference")

  ci_results_med <- rbind(full_ci_m,stepwiseaic_ci_m,stepwisebic_ci_m,lassomin_ci_m ,lassose_ci_m ,mcpmin_ci_m, mcpse_ci_m )
  rownames(ci_results_med)<- c("Full Model", "Stepwise AIC", "Stepwise BIC",  "Lasso CV (min)", "Lasso CV (se)","MCP CV (min)", "MCP CV (se)")
  colnames(ci_results_med) <- c("UPSI", "Bootstrap",  "Selective Inference")

  sdic_results<- rbind(full_sdisc, stepwiseaic_sdisc,stepwisebic_sdisc,lassomin_sdisc, lassose_sdisc, mcpmin_sdisc, mcpse_sdisc  )
  rownames(sdic_results)<- c("Full Model", "Stepwise AIC", "Stepwise BIC",  "Lasso CV (min)", "Lasso CV (se)","MCP CV (min)", "MCP CV (se)")
  colnames(sdic_results) <- c("UPSI", "Bootstrap", "Selective Inference")

  # Combine the three data frames into a list
  results_list <- list(
    avg_ci_ln = ci_results,
    med_ci_ln = ci_results_med,
    no_sign_disc = sdic_results
  )

  results_list

}
