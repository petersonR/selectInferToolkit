###### Test IRIS data. ########

data(iris)
iris <- iris[1:100,]

skip() # tests require updating, currently skipped

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
    obj1 <- step_ic (x=x,y=y,std = TRUE, direction = "both")
    obj2 <- step_ic (x=x,y=y,std = FALSE, direction = "both")

  })

  expect_silent({
    obj3 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC", direction = "both")
    obj4 <- step_ic(x=x,y=y,std = FALSE,penalty="BIC", direction = "both")

  })


})

test_that("forward selection works", {
  expect_silent({
    obj1 <- step_ic (x=x,y=y,std = TRUE,direction = "forward")
    obj2 <- step_ic (x=x,y=y,std = FALSE,direction = "forward")

  })

  expect_silent({
    obj3 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC",direction = "forward")
    obj4 <- step_ic(x=x,y=y,std = FALSE,penalty="BIC",direction = "forward")

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
    obj4 <- step_ic(x=x,y=y,std = FALSE,penalty="BIC",direction = "backward")

  })

  expect_error(pen_cv (x=x,y=y,penalty= "aic",direction = "backward"))
  expect_error(pen_cv (x=x,y=y,penalty= "bic",direction = "backward"))
  #expect_error(pen_cv (x=x,y=y,direction = "Backward"))
})



###### Test HERS Data set. ####
y= raw_data$hdl1
#x <-model.matrix(hdl1 ~., model.frame(~ ., raw_data, na.action=na.pass))[,-1]
x <- raw_data %>% dplyr::select(-hdl1)

test_that("HERS bi-direction works", {
  expect_silent({
    obj1 <- step_ic (x=x,y=y,std = TRUE, direction = "both")
    obj2 <- step_ic (x=x,y=y,std = FALSE, direction = "both")

  })

  expect_silent({
    obj3 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC", direction = "both")
    obj4 <- step_ic(x=x,y=y,std = FALSE,penalty="BIC", direction = "both")

  })

  expect_error(pen_cv (x=x,y=y,penalty= "aic"))
  expect_error(pen_cv (x=x,y=y,penalty= "bic"))

})

test_that("HERS forward selection works", {
  expect_silent({
    obj1 <- step_ic (x=x,y=y,std = TRUE,direction = "forward")
    obj2 <- step_ic (x=x,y=y,std = FALSE,direction = "forward")

  })

  expect_silent({
    obj3 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC",direction = "forward")
    obj4 <- step_ic(x=x,y=y,std = FALSE,penalty="BIC",direction = "forward")

  })

  expect_error(pen_cv (x=x,y=y,penalty= "aic",direction = "forward"))
  expect_error(pen_cv (x=x,y=y,penalty= "bic",direction = "forward"))
  #expect_error(pen_cv (x=x,y=y,direction = "Forward"))
})

test_that("HERS backward selection works", {
  expect_silent({
    obj1 <- step_ic (x=x,y=y,std = TRUE,direction = "backward")
    obj2 <- step_ic (x=x,y=y,std = FALSE,direction = "backward")

  })

  expect_silent({
    obj3 <- step_ic(x=x,y=y,std = TRUE,penalty="BIC",direction = "backward")
    obj4 <- step_ic(x=x,y=y,std = FALSE,penalty="BIC",direction = "backward")

  })

  expect_error(pen_cv (x=x,y=y,penalty= "aic",direction = "backward"))
  expect_error(pen_cv (x=x,y=y,penalty= "bic",direction = "backward"))
  #expect_error(pen_cv (x=x,y=y,direction = "Backward"))
})




#testthat::test_file("tests/testthat/test-step_ic.R")




