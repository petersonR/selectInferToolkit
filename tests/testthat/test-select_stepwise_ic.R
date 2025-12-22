###### Test IRIS data continuous outcome ########

data(iris)
#iris <- iris[1:100,]

#skip() # tests require updating, currently skipped

set.seed(123)

iris$Species <- factor(iris $Species, levels = c("setosa", "versicolor", "virginica"))
# Add another unbalanced factor
iris$Group <- factor(sample(c('A', 'B'), nrow(iris), replace = TRUE))

# Add a nzv variable
iris$NotUseful <- 2

# Add a binary variable
iris$BV <- rbinom(nrow(iris), 1, prob = .5)

# Add an unbalanced binary variable
iris$UBV <- rbinom(nrow(iris), 1, prob = .02)

test_that("bi-direction works", {
  sel <- select_stepwise_ic(Sepal.Length ~ ., iris,  direction="both")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = iris))

  rsel <- reselect(sel, iris)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = iris), predict(rsel, newdata = iris))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(Sepal.Length ~ ., iris[1:50,])))
  )



  sel2 <- select_stepwise_ic(Sepal.Length ~ ., iris,  direction="both",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = iris))

  rsel2 <- reselect(sel2, iris)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = iris), predict(rsel2, newdata = iris))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(Sepal.Length ~ ., iris[1:50,],  direction="both",
                                  penalty ="BIC")))
  )


})

test_that("forward selection works", {
  sel <- select_stepwise_ic(Sepal.Length ~ ., iris,  direction="forward")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = iris))

  rsel <- reselect(sel, iris)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = iris), predict(rsel, newdata = iris))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(Sepal.Length ~ ., iris[1:50,])))
  )


  sel2 <- select_stepwise_ic(Sepal.Length ~ ., iris,  direction="forward",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = iris))

  rsel <- reselect(sel2, iris)
  expect_identical(coef(sel2), coef(rsel))
  expect_identical(predict(sel2, newdata = iris), predict(rsel, newdata = iris))
  expect_identical(tidy(sel2), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(Sepal.Length ~ ., iris[1:50,],direction="forward",
                                  penalty ="BIC")))
  )

})

test_that("backward selection works", {
  sel <- select_stepwise_ic(Sepal.Length ~ ., iris,  direction="backward")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = iris))
  rsel <- reselect(sel, iris)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = iris), predict(rsel, newdata = iris))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(Sepal.Length ~ ., iris[1:50,],  direction="backward")))
  )

  sel2 <- select_stepwise_ic(Sepal.Length ~ ., iris,  direction="backward",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = iris))

  rsel <- reselect(sel2, iris)
  expect_identical(coef(sel2), coef(rsel))
  expect_identical(predict(sel2, newdata = iris), predict(rsel, newdata = iris))
  expect_identical(tidy(sel2), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(Sepal.Length ~ ., iris[1:50,],direction="backward",
                                  penalty ="BIC")))
  )


})


###### Test IRIS data binary outcome ########

iris_binary = iris
iris$setosa_bin <- ifelse(iris$Species=="setosa",1,0)
iris$setosa_bin <-factor(iris$setosa_bin , levels = c(0,1),labels  = c("other","setosa"))
iris_binary = iris %>% select(-Species)

test_that("bi-direction works", {
  formula = "setosa_bin ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width"
  sel <- select_stepwise_ic(formula = as.formula(formula), iris_binary,  direction="both",
                            family = "binomial")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = iris_binary))

  rsel <- reselect(sel, iris_binary)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = iris_binary), predict(rsel, newdata = iris_binary))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris_binary[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(formula = as.formula(formula), iris_binary[1:50,],
                                  direction="both",
                                  family = "binomial")))
  )


  sel2 <- select_stepwise_ic(formula = as.formula(formula), iris_binary,  direction="both",
                             family = "binomial", penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = iris_binary))

  rsel <- reselect(sel2, iris_binary)
  expect_identical(coef(sel2), coef(rsel))
  expect_identical(predict(sel2, newdata = iris_binary), predict(rsel, newdata = iris_binary))
  expect_identical(tidy(sel2), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(as.formula(formula), iris_binary[1:50,],direction="both",
                                  penalty ="BIC",family = "binomial",)))
  )



})

test_that("forward selection works", {
  formula = "setosa_bin ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width"
  sel <- select_stepwise_ic(formula = as.formula(formula), iris_binary,  direction="forward",
                            family = "binomial")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = iris_binary))
  rsel <- reselect(sel, iris_binary)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = iris_binary), predict(rsel, newdata = iris_binary))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris_binary[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(formula = as.formula(formula), iris_binary[1:50,],
                                  direction="forward",
                                  family = "binomial")))
  )


  sel2 <- select_stepwise_ic(formula = as.formula(formula), iris_binary,  direction="forward",
                             family = "binomial", penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = iris_binary))

  rsel <- reselect(sel2, iris_binary)
  expect_identical(coef(sel2), coef(rsel))
  expect_identical(predict(sel2, newdata = iris_binary), predict(rsel, newdata = iris_binary))
  expect_identical(tidy(sel2), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris_binary[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(formula = as.formula(formula), iris_binary[1:50,],direction="forward",
                                  penalty ="BIC", family = "binomial")))
  )

})

test_that("backward selection works", {
  formula = "setosa_bin ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width"
  sel <- select_stepwise_ic(formula = as.formula(formula), iris_binary,  direction="backward",
                            family = "binomial")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = iris_binary))
  rsel <- reselect(sel, iris_binary)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = iris_binary), predict(rsel, newdata = iris_binary))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris_binary[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(formula = as.formula(formula), iris_binary[1:50,],
                                  direction="backward",
                                  family = "binomial")))
  )



  sel2 <- select_stepwise_ic(formula = as.formula(formula), iris_binary,  direction="backward",
                             family = "binomial", penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = iris_binary))
  rsel <- reselect(sel2, iris_binary)
  expect_identical(coef(sel2), coef(rsel))
  expect_identical(predict(sel2, newdata = iris), predict(rsel, newdata = iris_binary))
  expect_identical(tidy(sel2), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris_binary[1:50,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(formula = as.formula(formula), iris_binary[1:50,],direction="backward",
                                  penalty ="BIC",  family = "binomial")))
  )

})


###### Test HERS Data set continuous outcome ####
data("hers")
force(hers)
#str(hers)


test_that("bi-direction works", {
  sel <- select_stepwise_ic(hdl1 ~ ., hers,  direction="both")

  expect_no_warning({
    capture_output(print(sel))
    print( tidy(sel), n=50)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata =hers))

  rsel <- reselect(sel, hers)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:500,])))
  )



  sel2 <- select_stepwise_ic(hdl1 ~ ., hers,  direction="both",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers))

  rsel2 <- reselect(sel2, hers)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers), predict(rsel2, newdata = hers))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[1:50,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:50,],  direction="both",
                                  penalty ="BIC")))
  )


})

test_that("bi-direction works individual factors ", {
  sel <- select_stepwise_ic(hdl1 ~ ., hers,  direction="both",
                                select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel))
    print( tidy(sel), n=50)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })


  expect_equal(unname(predict.glm(sel)), predict(sel, newdata =hers))

  rsel <- reselect(sel, hers)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:500,],select_factors_together = F)))
  )



  sel2 <- select_stepwise_ic(hdl1 ~ ., hers,  direction="both",
                             penalty ="BIC",  select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers))

  rsel2 <- reselect(sel2, hers)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers), predict(rsel2, newdata = hers))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[1:50,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:50,],  direction="both",
                                  select_factors_together = F, penalty ="BIC")))
  )


})

test_that("forward selection works", {
  sel <- select_stepwise_ic(hdl1 ~ ., hers,  direction="forward")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = hers))

  rsel <- reselect(sel, hers)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:500,],  direction="forward")))
  )

  sel2 <- select_stepwise_ic(hdl1 ~ ., hers,  direction="forward",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers))

  rsel2 <- reselect(sel2, hers)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers), predict(rsel2, newdata = hers))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[1:50,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:50,],  direction="forward",
                                  penalty ="BIC")))
  )


})

test_that("forward selection works, individual factors", {
  sel <- select_stepwise_ic(hdl1 ~ ., hers,  direction="forward",
                            select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = hers))

  rsel <- reselect(sel, hers)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:500,],  direction="forward",
                                  select_factors_together = F)))
  )

  sel2 <- select_stepwise_ic(hdl1 ~ ., hers,  direction="forward",
                             penalty ="BIC", select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers))

  rsel2 <- reselect(sel2, hers)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers), predict(rsel2, newdata = hers))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[1:50,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:50,],  direction="forward",
                                  penalty ="BIC", select_factors_together = F)))
  )


})

test_that("backward selection works", {
  sel <- select_stepwise_ic(hdl1 ~ ., hers,  direction="backward")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = hers))
  rsel <- reselect(sel, hers)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:500,],direction="backward")))
  )


  sel2 <- select_stepwise_ic(hdl1 ~ ., hers,  direction="backward",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers))

  rsel2 <- reselect(sel2, hers)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers), predict(rsel2, newdata = hers))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[1:150,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:150,],  direction="backward",
                                  penalty ="BIC")))
  )



})

test_that("backward selection works  individual factors", {
  sel <- select_stepwise_ic(hdl1 ~ ., hers,  direction="backward",
                            select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = hers))
  rsel <- reselect(sel, hers)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:500,],direction="backward",
                                  select_factors_together = F)))
  )


  sel2 <- select_stepwise_ic(hdl1 ~ ., hers,  direction="backward",
                             penalty ="BIC", select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers))

  rsel2 <- reselect(sel2, hers)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers), predict(rsel2, newdata = hers))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[1:150,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(hdl1 ~ ., hers[1:150,],  direction="backward",
                                  penalty ="BIC", select_factors_together = F)))
  )



})

###### Test HERS Data set binary outcome ####

hers_diab <- hers  %>% select (-hdl1, -dmpills, -insulin)
head(hers_diab)

test_that("bi-direction works", {
  sel <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="both",family = "binomial")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata =hers_diab))
  rsel <- reselect(sel, hers_diab)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers_diab), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes ~ ., hers_diab[1:500,],direction="both",family = "binomial")))
  )

  sel2 <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="both",family = "binomial",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers_diab))

  rsel2 <- reselect(sel2, hers_diab)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers_diab), predict(rsel2, newdata = hers_diab))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers_diab[1:150,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes~ ., hers_diab[1:150,],  direction="both",family = "binomial",
                                  penalty ="BIC")))
  )



})

test_that("bi-direction works individual factors", {
  sel <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="both",
                            family = "binomial", select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata =hers_diab))
  rsel <- reselect(sel, hers_diab)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers_diab), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes ~ ., hers_diab[1:500,],direction="both",
                                  family = "binomial", select_factors_together = F)))
  )

  sel2 <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="both",family = "binomial",
                             penalty ="BIC", select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers_diab))

  rsel2 <- reselect(sel2, hers_diab)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers_diab), predict(rsel2, newdata = hers_diab))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers_diab[1:150,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes~ ., hers_diab[1:150,],  direction="both",family = "binomial",
                                  penalty ="BIC", select_factors_together = F)))
  )



})

test_that("forward selection works", {
  sel <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="forward",family = "binomial")


  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata =hers_diab))
  rsel <- reselect(sel, hers_diab)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers_diab), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes ~ ., hers_diab[1:500,],direction="forward",family = "binomial")))
  )


  sel2 <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="forward",family = "binomial",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers_diab))

  rsel2 <- reselect(sel2, hers_diab)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers_diab), predict(rsel2, newdata = hers_diab))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers_diab[1:150,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes~ ., hers_diab[1:150,],  direction="forward",family = "binomial",
                                  penalty ="BIC")))
  )

})

test_that("forward selection works individual factors", {
  sel <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="forward",
                            family = "binomial", select_factors_together = F)


  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata =hers_diab))
  rsel <- reselect(sel, hers_diab)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers_diab), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes ~ ., hers_diab[1:500,],direction="forward",
                                  family = "binomial", select_factors_together = F)))
  )


  sel2 <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="forward",family = "binomial",
                             penalty ="BIC", select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers_diab))

  rsel2 <- reselect(sel2, hers_diab)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers_diab), predict(rsel2, newdata = hers_diab))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers_diab[1:150,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes~ ., hers_diab[1:150,],  direction="forward",family = "binomial",
                                  penalty ="BIC", select_factors_together = F)))
  )

})

test_that("backward selection works", {
  sel <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="backward",family = "binomial")

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata =hers_diab))
  rsel <- reselect(sel, hers_diab)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers_diab), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes ~ ., hers_diab[1:500,],direction="backward",family = "binomial")))
  )


  sel2 <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="backward",family = "binomial",
                             penalty ="BIC")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers_diab))
  rsel2 <- reselect(sel2, hers_diab)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers_diab), predict(rsel2, newdata = hers_diab))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers_diab[1:150,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes~ ., hers_diab[1:150,],  direction="backward",family = "binomial",
                                  penalty ="BIC")))
  )


})


test_that("backward selection works indiviudal factors", {
  sel <- select_stepwise_ic(diabetes ~ ., hers_diab,
                            direction="backward",family = "binomial",
                            select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata =hers_diab))
  rsel <- reselect(sel, hers_diab)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = hers_diab), predict(rsel, newdata = hers))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[1:500,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes ~ ., hers_diab[1:500,],
                                  direction="backward",family = "binomial",select_factors_together = F)))
  )


  sel2 <- select_stepwise_ic(diabetes ~ ., hers_diab,  direction="backward",family = "binomial",
                             penalty ="BIC", select_factors_together = F)

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  expect_equal(unname(predict.glm(sel2)), predict(sel2, newdata = hers_diab))
  rsel2 <- reselect(sel2, hers_diab)
  expect_identical(coef(sel2), coef(rsel2))
  expect_identical(predict(sel2, newdata = hers_diab), predict(rsel2, newdata = hers_diab))
  expect_identical(tidy(sel2), tidy(rsel2))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers_diab[1:150,])
  #should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(diabetes~ ., hers_diab[1:150,],  direction="backward",family = "binomial",
                                  penalty ="BIC", select_factors_together = F)))
  )


})


#testthat::test_file("tests/testthat/test-select_stepwise_ic.R")


