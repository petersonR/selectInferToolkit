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




test_that("Full model boot works ", {
  expect_silent({
    model<- lm(y~., data=cbind(y,x), x= TRUE, y=TRUE)
    obj1 <-full_boot(model, B=5, family="gaussian",parallel = FALSE)

  })

  expect_no_error(
    obj1 <- full_boot(model, B=5, family="gaussian",parallel = TRUE)

  )

})

test_that("Full model stepwsie aic boot bi-dirctional works ", {
  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })

})


test_that("Full model stepwsie aic boot forward selection works ", {
  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "forward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "forward")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "forward")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "forward")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie aic boot backward selection works ", {
  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    #obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie bic boot bi-dirctional works ", {
  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both",penalty = "BIC")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both",penalty = "BIC")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both",penalty = "BIC")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both", penalty = "BIC")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })

})

test_that("Full model stepwsie bic boot forward selection works ", {
  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "forward",penalty = "BIC")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "forward",penalty = "BIC")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "forward", penalty = "BIC")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "forward",penalty = "BIC")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie bic boot backward selection works ", {
  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward",penalty = "BIC")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward",penalty = "BIC")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward",penalty = "BIC")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward",penalty = "BIC")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })





})





# ##### Test HERS data ######
# Test HERS Data set
y= raw_data$hdl1
#x <-model.matrix(hdl1 ~., model.frame(~ ., raw_data, na.action=na.pass))[,-1]
x <- raw_data %>% dplyr::select(-hdl1)



test_that("Full model boot works ", {
  expect_silent({
    model<- lm(y~., data=cbind(y,x), x= TRUE, y=TRUE)
    obj1 <-full_boot(model, B=5, family="gaussian",parallel = FALSE)

  })

  expect_no_error(
    obj1 <- full_boot(model, B=5, family="gaussian",parallel = TRUE)

  )

})

test_that("Full model stepwsie aic boot bi-dirctional works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })

})


test_that("Full model stepwsie aic boot forward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "forward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "forward")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "forward")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "forward")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie aic boot backward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie bic boot bi-dirctional works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both",penalty = "BIC")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both",penalty = "BIC")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "both",penalty = "BIC")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "both", penalty = "BIC")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })

})

test_that("Full model stepwsie bic boot forward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "forward",penalty = "BIC")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "forward",penalty = "BIC")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "forward", penalty = "BIC")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "forward",penalty = "BIC")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie bic boot backward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward",penalty = "BIC")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward",penalty = "BIC")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=step_ic (x=x,y=y,std = TRUE, direction = "backward",penalty = "BIC")
    obj1 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE, direction = "backward",penalty = "BIC")
    obj4 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj5<- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })





})

#testthat::test_file("tests/testthat/test-bootstrap_fns_ic.R")
