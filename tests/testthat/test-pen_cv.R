# Test IRIS data

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

test_that("lasso  works", {
  expect_silent({
    obj1 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE)
    obj2 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE)

  })

  expect_silent({
    obj3 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)
    obj4 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)

  })

  expect_error(pen_cv (x=x,y=y,penalty= "Lasso",lambda="lambda.1se",std=TRUE))
  expect_error(pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda1se",std=TRUE))
  expect_error(pen_cv (x=x,y=y,penalty= "lasso",lambda="minlambda",std=TRUE))
  expect_error(pen_cv (x=x,y=y,penalty= "lasso",lambda="lambdamin",std=TRUE))


})


test_that("MCP works", {
  expect_silent({
    obj1 <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=TRUE)
    obj2 <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=FALSE)

  })

  expect_silent({
    obj3 <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)
    obj4 <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)

  })

})




test_that("elastic net works", {
  expect_silent({
    obj1 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE, alph=0.5)
    obj2 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE,alph=0.5)

  })

  expect_silent({
    obj3 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)
    obj4 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)

  })

})


# Test HERS Data set
y= raw_data$hdl1
#x <-model.matrix(hdl1 ~., model.frame(~ ., raw_data, na.action=na.pass))[,-1]
x <- raw_data %>% dplyr::select(-hdl1)


test_that("lasso  works, HERS", {
  expect_silent({
    obj1 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE)
    obj2 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE)

  })

  expect_silent({
    obj3 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)
    obj4 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE)

  })
})

test_that("MCP works, HERS", {
  expect_silent({
    obj1 <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=TRUE)
    obj2 <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.min",std=FALSE)

  })

  expect_silent({
    obj3 <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)
    obj4 <- pen_cv (x=x,y=y,penalty= "MCP",lambda="lambda.1se",std=TRUE)

  })

})


test_that("elastic net works HERS ", {
  expect_silent({
    obj1 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE, alph=0.5)
    obj2 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=FALSE,alph=0.5)

  })

  expect_silent({
    obj3 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)
    obj4 <- pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.1se",std=TRUE,alph=0.5)

  })

})
