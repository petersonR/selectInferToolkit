##### Test IRIS data ######

data(iris)
iris <- iris[1:100,]

set.seed(123)

# Add another unbalanced factor
iris$Group <- factor(sample(c('A', 'B'), nrow(iris), replace = TRUE))

# Add a nzv variable
#iris$NotUseful <- 2

# Add a binary variable
iris$BV <- rbinom(nrow(iris), 1, prob = .5)

# Add an unbalanced binary variable
iris$UBV <- rbinom(nrow(iris), 1, prob = .02)

x <- iris[, -which(names(iris) == "Sepal.Length")]
y<- iris$Sepal.Length



test_that("Lasso min works ", {
  expect_no_error({
    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE)
    obj1 <- infer(lasso_mod, method = "boot", B=5)
    obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(lasso_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
    obj6 <-infer(lasso_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)

    lasso_mod_nostd <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE)
    obj7 <- infer(lasso_mod_nostd,  method = "boot", B=5)
    obj8 <-infer(lasso_mod_nostd , method = "boot", B=5, parallel=T)
    obj9 <- infer(lasso_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(lasso_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(lasso_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
    obj12 <-infer(lasso_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)


  })
})


test_that("Lasso 1se works ", {
  expect_no_error({
    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)
    obj1 <- infer(lasso_mod, method = "boot", B=5)
    obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(lasso_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
    obj6 <-infer(lasso_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)


    lasso_mod_nostd <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE)
    obj7 <- infer(lasso_mod_nostd,  method = "boot", B=5)
    obj8 <-infer(lasso_mod_nostd , method = "boot", B=5, parallel=T)
    obj9 <- infer(lasso_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(lasso_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(lasso_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
    obj12 <-infer(lasso_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)


  })
})


test_that("MCP min works ", {
  expect_no_error({
    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=TRUE)
    obj1 <- infer(mcp_mod, method = "boot", B=5)
    obj2 <-infer(mcp_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(mcp_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(mcp_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(mcp_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
    obj6 <-infer(mcp_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)


    mcp_mod_nostd <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=FALSE)
    obj7 <- infer(mcp_mod_nostd,  method = "boot", B=5)
    obj8 <-infer(mcp_mod_nostd , method = "boot", B=5, parallel=T)
    obj9 <- infer(mcp_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(mcp_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(mcp_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
    obj12 <-infer(mcp_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)


  })

  })

test_that("MCP 1se works ", {
  expect_no_error({
    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)
    obj1 <- infer(mcp_mod, method = "boot", B=5)
    obj2 <-infer(mcp_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(mcp_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(mcp_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(mcp_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
    obj6 <-infer(mcp_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)

    mcp_mod_nostd <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=FALSE)
    obj7 <- infer(mcp_mod_nostd,  method = "boot", B=5)
    obj8 <-infer(mcp_mod_nostd , method = "boot", B=5, parallel=T)
    obj9 <- infer(mcp_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(mcp_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(mcp_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
    obj12 <-infer(mcp_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)



  })
})


test_that("Elastic net lambda min works ", {
  expect_no_error({
    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE,alph=0.5)
    obj1 <- infer(enet_mod, method = "boot", B=5)
    obj2 <-infer(enet_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(enet_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(enet_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(enet_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
    obj6 <-infer(enet_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)

    enet_mod_nostd  <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE,alph=0.5)
    obj7 <- infer(enet_mod_nostd,  method = "boot", B=5)
    obj8 <-infer(enet_mod_nostd , method = "boot", B=5, parallel=T)
    obj9 <- infer(enet_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(enet_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(enet_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
    obj12 <-infer(enet_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)


  })
})

test_that("Elastic net lambda 1se works ", {
  expect_no_error({
    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)
    obj1 <- infer(enet_mod, method = "boot", B=5)
    obj2 <-infer(enet_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(enet_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(enet_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(enet_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
    obj6 <-infer(enet_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)


    enet_mod_nostd  <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE,alph=0.5)
    obj7 <- infer(enet_mod_nostd,  method = "boot", B=5)
    obj8 <-infer(enet_mod_nostd , method = "boot", B=5, parallel=T)
    obj9 <- infer(enet_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(enet_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(enet_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
    obj12 <-infer(enet_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)


  })
})

#testthat::test_file("tests/testthat/test-bootstrap_fns_pen.R")


# # # ##### Test HERS data ######
# # # Test HERS Data set
# y= raw_data$hdl1
# #x <-model.matrix(hdl1 ~., model.frame(~ ., raw_data, na.action=na.pass))[,-1]
# x <- raw_data %>% dplyr::select(-hdl1)
#
# test_that("Lasso min works ", {
#   expect_no_error({
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE)
#     obj1 <- infer(lasso_mod, method = "boot", B=5)
#     obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj5 <-infer(lasso_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj6 <-infer(lasso_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)
#
#     lasso_mod_nostd <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE)
#     obj7 <- infer(lasso_mod_nostd,  method = "boot", B=5)
#     obj8 <-infer(lasso_mod_nostd , method = "boot", B=5, parallel=T)
#     obj9 <- infer(lasso_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(lasso_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj11 <-infer(lasso_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj12 <-infer(lasso_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)
#
#
#   })
# })
#
#
# test_that("Lasso 1se works ", {
#   expect_no_error({
#     lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)
#     obj1 <- infer(lasso_mod, method = "boot", B=5)
#     obj2 <-infer(lasso_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(lasso_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj5 <-infer(lasso_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj6 <-infer(lasso_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)
#
#
#     lasso_mod_nostd <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE)
#     obj7 <- infer(lasso_mod_nostd,  method = "boot", B=5)
#     obj8 <-infer(lasso_mod_nostd , method = "boot", B=5, parallel=T)
#     obj9 <- infer(lasso_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(lasso_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj11 <-infer(lasso_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj12 <-infer(lasso_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)
#
#
#   })
# })
#
#
# test_that("MCP min works ", {
#   expect_no_error({
#     mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=TRUE)
#     obj1 <- infer(mcp_mod, method = "boot", B=5)
#     obj2 <-infer(mcp_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(mcp_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(mcp_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj5 <-infer(mcp_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj6 <-infer(mcp_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)
#
#
#     mcp_mod_nostd <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=FALSE)
#     obj7 <- infer(mcp_mod_nostd,  method = "boot", B=5)
#     obj8 <-infer(mcp_mod_nostd , method = "boot", B=5, parallel=T)
#     obj9 <- infer(mcp_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(mcp_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj11 <-infer(mcp_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj12 <-infer(mcp_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)
#
#
#   })
#
# })
#
# test_that("MCP 1se works ", {
#   expect_silent({
#     mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)
#     obj1 <- infer(mcp_mod, method = "boot", B=5)
#     obj2 <-infer(mcp_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(mcp_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(mcp_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj5 <-infer(mcp_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj6 <-infer(mcp_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)
#
#     mcp_mod_nostd <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=FALSE)
#     obj7 <- infer(mcp_mod_nostd,  method = "boot", B=5)
#     obj8 <-infer(mcp_mod_nostd , method = "boot", B=5, parallel=T)
#     obj9 <- infer(mcp_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(mcp_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj11 <-infer(mcp_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj12 <-infer(mcp_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)
#
#
#
#   })
# })
#
#
# test_that("Elastic net lambda min works ", {
#   expect_silent({
#     enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE,alph=0.5)
#     obj1 <- infer(enet_mod, method = "boot", B=5)
#     obj2 <-infer(enet_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(enet_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(enet_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj5 <-infer(enet_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj6 <-infer(enet_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)
#
#     enet_mod_nostd  <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE,alph=0.5)
#     obj7 <- infer(enet_mod_nostd,  method = "boot", B=5)
#     obj8 <-infer(enet_mod_nostd , method = "boot", B=5, parallel=T)
#     obj9 <- infer(enet_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(enet_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj11 <-infer(enet_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj12 <-infer(enet_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)
#
#
#   })
# })
#
# test_that("Elastic net lambda 1se works ", {
#   expect_silent({
#     enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)
#     obj1 <- infer(enet_mod, method = "boot", B=5)
#     obj2 <-infer(enet_mod, method = "boot", B=5, parallel=T)
#     obj3 <- infer(enet_mod, method = "boot", B=5, nonselection = "confident_nulls")
#     obj4 <- infer(enet_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj5 <-infer(enet_mod, method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj6 <-infer(enet_mod, method = "boot", B=5, nonselection = "uncertain_nulls",parallel=T)
#
#
#     enet_mod_nostd  <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE,alph=0.5)
#     obj7 <- infer(enet_mod_nostd,  method = "boot", B=5)
#     obj8 <-infer(enet_mod_nostd , method = "boot", B=5, parallel=T)
#     obj9 <- infer(enet_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
#     obj10 <- infer(enet_mod_nostd , method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
#     obj11 <-infer(enet_mod_nostd , method = "boot", B=5, nonselection = "uncertain_nulls")
#     obj12 <-infer(enet_mod_nostd , method = "boot", B=5,  nonselection = "uncertain_nulls",parallel=T)
#
#
#   })
# })
#
#


#testthat::test_file("tests/testthat/test-bootstrap_fns_pen.R")
