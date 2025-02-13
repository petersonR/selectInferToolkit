# # Test IRIS data
#
# data(iris)
# iris <- iris[1:100,]
#
# set.seed(123)
#
# # Add another unbalanced factor
# iris$Group <- factor(sample(c('A', 'B'), nrow(iris), replace = TRUE))
#
# # Add a nzv variable
# #iris$NotUseful <- 2
#
# # Add a binary variable
# iris$BV <- rbinom(nrow(iris), 1, prob = .5)
#
# # Add an unbalanced binary variable
# iris$UBV <- rbinom(nrow(iris), 1, prob = .02)
#
# x <- iris[, -which(names(iris) == "Sepal.Length")]
# y<- iris$Sepal.Length
#
#
#
# test_that("Full model lasso min works ", {
#   expect_silent({
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE)
#     obj1 <- infer(lasso_mod, method = "boot", B=5)
#     obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE)
#     obj7 <- infer(lasso_mod, method = "boot", B=5)
#     obj8 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj9 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#
#   })
# })
#
#
# test_that("Full model lasso 1se works ", {
#   expect_silent({
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)
#     obj1 <- infer(lasso_mod, method = "boot", B=5)
#     obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE)
#     obj7 <- infer(lasso_mod, method = "boot", B=5)
#     obj8 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj9 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#
#   })
# })
#
#
# test_that("Full model MCP min", {
#   expect_silent({
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=TRUE)
#     obj1 <- infer(lasso_mod, method = "boot", B=5)
#     obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=FALSE)
#     obj7 <- infer(lasso_mod, method = "boot", B=5)
#     obj8 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj9 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#   })
#
#   })
#
# test_that("Full model MCP 1se works ", {
#   expect_silent({
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)
#     obj1 <- infer(lasso_mod, method = "boot", B=5)
#     obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=FALSE)
#     obj7 <- infer(lasso_mod, method = "boot", B=5)
#     obj8 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj9 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#
#   })
# })
#
#
# test_that("Full model elastic net lambda min works ", {
#   expect_silent({
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE,alph=0.5)
#     obj1 <- infer(lasso_mod, method = "boot", B=5)
#     obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE,alph=0.5)
#     obj7 <- infer(lasso_mod, method = "boot", B=5)
#     obj8 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj9 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#
#   })
# })
#
# test_that("Full model elastic net lambda 1sr works ", {
#   expect_silent({
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)
#     obj1 <- infer(lasso_mod, method = "boot", B=5)
#     obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE, alph=0.5)
#     obj7 <- infer(lasso_mod, method = "boot", B=5)
#     obj8 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj9 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     #obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
#     #obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")
#
#
#   })
# })
#
