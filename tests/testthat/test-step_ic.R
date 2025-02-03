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

test_that("bi-direction works", {
  expect_silent({
    obj1 <- step_ic (x=x,y=y,std = TRUE)
    obj2 <- step_ic (x=x,y=y,std = FALSE)

  })

  expect_silent({
    obj3 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC")
    obj4 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC")

  })

  expect_error(pen_cv (x=x,y=y,penalty= "aic"))
  expect_error(pen_cv (x=x,y=y,penalty= "bic"))
  #expect_error(pen_cv (x=x,y=y,direction = "bidirection"))
  #expect_error(pen_cv (x=x,y=y,direction = "Both"))
})

test_that("forward selection works", {
  expect_silent({
    obj1 <- step_ic (x=x,y=y,std = TRUE,direction = "forward")
    obj2 <- step_ic (x=x,y=y,std = FALSE,direction = "forward")

  })

  expect_silent({
    obj3 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC",direction = "forward")
    obj4 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC",direction = "forward")

  })

  expect_error(pen_cv (x=x,y=y,penalty= "aic",direction = "forward"))
  expect_error(pen_cv (x=x,y=y,penalty= "bic",direction = "forward"))
  #expect_error(pen_cv (x=x,y=y,direction = "Forward"))
})

test_that("backward selection works", {
  expect_silent({
    obj1 <- step_ic (x=x,y=y,std = TRUE,direction = "backward")
    obj2 <- step_ic (x=x,y=y,std = FALSE,direction = "backward")

  })

  expect_silent({
    obj3 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC",direction = "backward")
    obj4 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC",direction = "backward")

  })

  expect_error(pen_cv (x=x,y=y,penalty= "aic",direction = "backward"))
  expect_error(pen_cv (x=x,y=y,penalty= "bic",direction = "backward"))
  #expect_error(pen_cv (x=x,y=y,direction = "Backward"))
})














data(raw_data)
y= raw_data$hdl1
x <-model.matrix(hdl1 ~., model.frame(~ ., raw_data, na.action=na.pass))[,-1]

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
