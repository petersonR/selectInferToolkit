####### Test IRIS data ########

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


test_that("Stepwsie aic bi-dirctional works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    #obj4 <- infer(aic_mod, method = "selectiveinf")
    #obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    #obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    #obj10 <- infer(aic_mod, method = "selectiveinf")
    #obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    #obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("Stepwsie bic bi-dirctional works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both",penalty= "BIC")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    #obj4 <- infer(aic_mod, method = "selectiveinf")
    #obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    #obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both",penalty= "BIC")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    #obj10 <- infer(aic_mod, method = "selectiveinf")
    #obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    #obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("Stepwsie aic forward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE)
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj4 <- infer(aic_mod, method = "selectiveinf")
    obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE)
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj10 <- infer(aic_mod, method = "selectiveinf")
    obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("Stepwsie bic forward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE,penalty= "BIC")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj4 <- infer(aic_mod, method = "selectiveinf")
    # obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE,penalty= "BIC")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj10 <- infer(aic_mod, method = "selectiveinf")
    # obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("Stepwsie aic backward works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj4 <- infer(aic_mod, method = "selectiveinf")
    # obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj10 <- infer(aic_mod, method = "selectiveinf")
    # obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("Stepwsie bic backward works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward",penalty= "BIC")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj4 <- infer(aic_mod, method = "selectiveinf")
    # obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward",penalty= "BIC")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj10 <- infer(aic_mod, method = "selectiveinf")
    # obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("Full model lasso min works ", {
  expect_silent({
    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE)
    obj1 <- infer(lasso_mod, method = "hybrid")
    obj2 <-infer(lasso_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(lasso_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj4 <- infer(lasso_mod, method = "selectiveinf")
    obj5 <-infer(lasso_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj6 <- infer(lasso_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE)
    obj7 <- infer(lasso_mod, method = "hybrid")
    obj8 <-infer(lasso_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(lasso_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj10 <- infer(lasso_mod, method = "selectiveinf")
    obj11 <-infer(lasso_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj12 <- infer(lasso_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("Full model lasso 1se works ", {
  expect_silent({
    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)
    obj1 <- infer(lasso_mod, method = "hybrid")
    obj2 <-infer(lasso_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(lasso_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj4 <- infer(lasso_mod, method = "selectiveinf")
    obj5 <-infer(lasso_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj6 <- infer(lasso_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE)
    obj7 <- infer(lasso_mod, method = "hybrid")
    obj8 <-infer(lasso_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(lasso_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj10 <- infer(lasso_mod, method = "selectiveinf")
    obj11 <-infer(lasso_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj12 <- infer(lasso_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("Full model MCP min works ", {
  expect_silent({
    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=TRUE)
    obj1 <- infer(mcp_mod, method = "hybrid")
    obj2 <-infer(mcp_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(mcp_mod, method = "hybrid", nonselection = "uncertain_nulls")

    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=FALSE)
    obj4 <- infer(mcp_mod, method = "hybrid")
    obj5 <-infer(mcp_mod, method = "hybrid", nonselection = "confident_nulls")
    obj6 <- infer(mcp_mod, method = "hybrid", nonselection = "uncertain_nulls")


  })
})


test_that("Full model MCP 1se works ", {
  expect_silent({
    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)
    obj1 <- infer(mcp_mod, method = "hybrid")
    obj2 <-infer(mcp_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(mcp_mod, method = "hybrid", nonselection = "uncertain_nulls")

    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=FALSE)
    obj4 <- infer(mcp_mod, method = "hybrid")
    obj5 <-infer(mcp_mod, method = "hybrid", nonselection = "confident_nulls")
    obj6 <- infer(mcp_mod, method = "hybrid", nonselection = "uncertain_nulls")


  })
})

test_that("Full model enet min works ", {
  expect_silent({
    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE,alph=0.5)
    obj1 <- infer(enet_mod, method = "hybrid")
    obj2 <-infer(enet_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(enet_mod, method = "hybrid", nonselection = "uncertain_nulls")

    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE,alph=0.5)
    obj4 <- infer(enet_mod, method = "hybrid")
    obj5 <-infer(enet_mod, method = "hybrid", nonselection = "confident_nulls")
    obj6 <- infer(enet_mod, method = "hybrid", nonselection = "uncertain_nulls")

  })
})

test_that("Full model enet 1se works ", {
  expect_silent({
    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)
    obj1 <- infer(enet_mod, method = "hybrid")
    obj2 <-infer(enet_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(enet_mod, method = "hybrid", nonselection = "uncertain_nulls")

    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE,alph=0.5)
    obj4 <- infer(enet_mod, method = "hybrid")
    obj5 <-infer(enet_mod, method = "hybrid", nonselection = "confident_nulls")
    obj6 <- infer(enet_mod, method = "hybrid", nonselection = "uncertain_nulls")


  })
})


###### Test HERS Data set####
y= raw_data$hdl1
#x <-model.matrix(hdl1 ~., model.frame(~ ., raw_data, na.action=na.pass))[,-1]
x <- raw_data %>% dplyr::select(-hdl1)

test_that("HERS Stepwsie aic bi-dirctional works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    #obj4 <- infer(aic_mod, method = "selectiveinf")
    #obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    #obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    #obj10 <- infer(aic_mod, method = "selectiveinf")
    #obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    #obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("HERS Stepwsie bic bi-dirctional works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both",penalty= "BIC")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    #obj4 <- infer(aic_mod, method = "selectiveinf")
    #obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    #obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both",penalty= "BIC")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    #obj10 <- infer(aic_mod, method = "selectiveinf")
    #obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    #obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("HERS Stepwsie aic forward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE)
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj4 <- infer(aic_mod, method = "selectiveinf")
    obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE)
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj10 <- infer(aic_mod, method = "selectiveinf")
    obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("HERStepwsie bic forward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE,penalty= "BIC")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj4 <- infer(aic_mod, method = "selectiveinf")
    # obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE,penalty= "BIC")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj10 <- infer(aic_mod, method = "selectiveinf")
    # obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("HERS Stepwsie aic backward works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj4 <- infer(aic_mod, method = "selectiveinf")
    # obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj10 <- infer(aic_mod, method = "selectiveinf")
    # obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("HERS Stepwsie bic backward works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward",penalty= "BIC")
    obj1 <- infer(aic_mod, method = "hybrid")
    obj2 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj4 <- infer(aic_mod, method = "selectiveinf")
    # obj5 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj6 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward",penalty= "BIC")
    obj7 <- infer(aic_mod, method = "hybrid")
    obj8 <-infer(aic_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(aic_mod, method = "hybrid", nonselection = "uncertain_nulls")
    # obj10 <- infer(aic_mod, method = "selectiveinf")
    # obj11 <-infer(aic_mod, method = "selectiveinf", nonselection = "confident_nulls")
    # obj12 <- infer(aic_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("HERS Full model lasso min works ", {
  expect_silent({
    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE)
    obj1 <- infer(lasso_mod, method = "hybrid")
    obj2 <-infer(lasso_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(lasso_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj4 <- infer(lasso_mod, method = "selectiveinf")
    obj5 <-infer(lasso_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj6 <- infer(lasso_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE)
    obj7 <- infer(lasso_mod, method = "hybrid")
    obj8 <-infer(lasso_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(lasso_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj10 <- infer(lasso_mod, method = "selectiveinf")
    obj11 <-infer(lasso_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj12 <- infer(lasso_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("HERS Full model lasso 1se works ", {
  expect_silent({
    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)
    obj1 <- infer(lasso_mod, method = "hybrid")
    obj2 <-infer(lasso_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(lasso_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj4 <- infer(lasso_mod, method = "selectiveinf")
    obj5 <-infer(lasso_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj6 <- infer(lasso_mod, method = "selectiveinf", nonselection = "uncertain_nulls")

    lasso_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE)
    obj7 <- infer(lasso_mod, method = "hybrid")
    obj8 <-infer(lasso_mod, method = "hybrid", nonselection = "confident_nulls")
    obj9 <- infer(lasso_mod, method = "hybrid", nonselection = "uncertain_nulls")
    obj10 <- infer(lasso_mod, method = "selectiveinf")
    obj11 <-infer(lasso_mod, method = "selectiveinf", nonselection = "confident_nulls")
    obj12 <- infer(lasso_mod, method = "selectiveinf", nonselection = "uncertain_nulls")


  })
})

test_that("HERS Full model MCP min works ", {
  expect_silent({
    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=TRUE)
    obj1 <- infer(mcp_mod, method = "hybrid")
    obj2 <-infer(mcp_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(mcp_mod, method = "hybrid", nonselection = "uncertain_nulls")

    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=FALSE)
    obj4 <- infer(mcp_mod, method = "hybrid")
    obj5 <-infer(mcp_mod, method = "hybrid", nonselection = "confident_nulls")
    obj6 <- infer(mcp_mod, method = "hybrid", nonselection = "uncertain_nulls")


  })
})


test_that("HERS Full model MCP 1se works ", {
  expect_silent({
    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)
    obj1 <- infer(mcp_mod, method = "hybrid")
    obj2 <-infer(mcp_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(mcp_mod, method = "hybrid", nonselection = "uncertain_nulls")

    mcp_mod <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=FALSE)
    obj4 <- infer(mcp_mod, method = "hybrid")
    obj5 <-infer(mcp_mod, method = "hybrid", nonselection = "confident_nulls")
    obj6 <- infer(mcp_mod, method = "hybrid", nonselection = "uncertain_nulls")


  })
})

test_that("HERS Full model enet min works ", {
  expect_silent({
    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE,alph=0.5)
    obj1 <- infer(enet_mod, method = "hybrid")
    obj2 <-infer(enet_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(enet_mod, method = "hybrid", nonselection = "uncertain_nulls")

    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE,alph=0.5)
    obj4 <- infer(enet_mod, method = "hybrid")
    obj5 <-infer(enet_mod, method = "hybrid", nonselection = "confident_nulls")
    obj6 <- infer(enet_mod, method = "hybrid", nonselection = "uncertain_nulls")

  })
})

test_that("HERS Full model enet 1se works ", {
  expect_silent({
    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)
    obj1 <- infer(enet_mod, method = "hybrid")
    obj2 <-infer(enet_mod, method = "hybrid", nonselection = "confident_nulls")
    obj3 <- infer(enet_mod, method = "hybrid", nonselection = "uncertain_nulls")

    enet_mod <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=FALSE,alph=0.5)
    obj4 <- infer(enet_mod, method = "hybrid")
    obj5 <-infer(enet_mod, method = "hybrid", nonselection = "confident_nulls")
    obj6 <- infer(enet_mod, method = "hybrid", nonselection = "uncertain_nulls")


  })
})




