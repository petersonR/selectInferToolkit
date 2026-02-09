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


test_that("lasso  works", {
  set.seed(1)
  sel <- select_glmnet(Sepal.Length ~ ., iris)
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  rsel <- reselect(sel, iris)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris[-7,])
  set.seed(1)
  sel_rep <- select_glmnet(Sepal.Length ~ ., iris)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(1)
  sel2 <- select_glmnet(Sepal.Length ~ ., iris,lambda = "compact")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  rsel <- reselect(sel2, iris)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris[-7,])
  set.seed(1)
  sel_rep2 <- select_glmnet(Sepal.Length ~ ., iris, lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))


})

test_that("elastic net works", {
  set.seed(1)
  sel <- select_glmnet(Sepal.Length ~ ., iris, alpha=0.5)
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  rsel <- reselect(sel, iris)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris[-7,])
  set.seed(1)
  sel_rep <- select_glmnet(Sepal.Length ~ ., iris,  alpha=0.5)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(1)
  sel2 <- select_glmnet(Sepal.Length ~ ., iris,alpha=0.5,lambda = "compact")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  rsel <- reselect(sel2, iris)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris[-7,])

  set.seed(1)
  sel_rep2 <- select_glmnet(Sepal.Length ~ ., iris,  alpha=0.5,lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))

})

###### Test IRIS data binary outcome ########

iris_binary = iris
iris$setosa_bin <- ifelse(iris$Species=="setosa",1,0)
iris$setosa_bin <-factor(iris$setosa_bin , levels = c(0,1),labels  = c("other","setosa"))
iris_binary = iris %>% select(-Species)

test_that("lasso  works", {
  formula = "setosa_bin ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width"
  set.seed(2)
  sel <- select_glmnet(formula = as.formula(formula), iris_binary,
                       family = "binomial")
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

  rsel <- reselect(sel, iris_binary)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris_binary[-7,])
  set.seed(1)
  sel_rep <- select_glmnet(formula = as.formula(formula), iris_binary, family = "binomial")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(1)
  sel2 <- select_glmnet(formula = as.formula(formula), iris_binary,
                        lambda = "compact", family = "binomial")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,],)
    predict(sel2, newdata = iris)
  })

  rsel <- reselect(sel2, iris_binary)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris_binary[-7,])

  set.seed(1)
  sel_rep2 <- select_glmnet(formula = as.formula(formula), iris_binary,
                            lambda = "compact",family = "binomial")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))


})

test_that("elastic net works", {
  formula = "setosa_bin ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width"
  set.seed(3)
  sel <- select_glmnet(formula = as.formula(formula), iris_binary, alpha=0.5,
                       family = "binomial")
    expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =iris[1:5,])
    predict(sel, newdata = iris)
  })

   rsel <- reselect(sel, iris_binary)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = iris_binary[-7,])
  set.seed(3)
  sel_rep <- select_glmnet(formula = as.formula(formula), iris_binary, family = "binomial",
                           alpha=0.5)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(3)
  formula = "setosa_bin ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width"
  sel2 <- select_glmnet(formula = as.formula(formula), iris_binary, alpha=0.5,
                         family = "binomial",lambda = "compact")

  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =iris[1:5,])
    predict(sel2, newdata = iris)
  })

  rsel <- reselect(sel2, iris_binary)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = iris_binary[-7,])

  set.seed(3)
  sel_rep2 <- select_glmnet(formula = as.formula(formula), iris_binary, alpha=0.5,
                            family = "binomial",lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))

})

###### Test HERS Data set continuous outcome ####
data("hers")
force(hers)

test_that("lasso  works", {
  set.seed(34)
  sel <- select_glmnet(hdl1 ~ ., hers)
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })


  rsel <- reselect(sel, hers)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[-7,])
  set.seed(34)
  sel_rep <- select_glmnet(hdl1 ~ ., hers)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(34)
  sel2 <- select_glmnet(hdl1 ~ ., hers, lambda = "compact")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })


  rsel <- reselect(sel2, hers)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[-7,])
  set.seed(34)
  sel_rep2 <- select_glmnet(hdl1 ~ ., hers, lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))


})

test_that("elastic net works", {
  set.seed(34)
  sel <- select_glmnet(hdl1 ~ ., hers, alpha=0.5)
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })


  rsel <- reselect(sel, hers)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[-7,])
  set.seed(34)
  sel_rep <- select_glmnet(hdl1 ~ ., hers,alpha=0.5)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(34)
  sel2 <- select_glmnet(hdl1 ~ ., hers,alpha=0.5,lambda = "compact")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })


  rsel <- reselect(sel2, hers)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[-7,])
  set.seed(34)
  sel_rep2 <- select_glmnet(hdl1 ~ ., hers, lambda = "compact",alpha=0.5)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))

})

###### Test HERS Data set binary outcome ####
hers_diab <- hers  %>% select (-hdl1, -dmpills, -insulin)


test_that("lasso  works", {
  set.seed(10)
  sel <- select_glmnet(diabetes ~ ., hers_diab, family = "binomial")
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  rsel <- reselect(sel, hers_diab)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[-7,])
  set.seed(10)
  sel_rep <- select_glmnet(diabetes ~ ., hers_diab, family = "binomial")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))


  set.seed(10)
  sel2 <- select_glmnet(diabetes ~ ., hers_diab, family = "binomial",lambda = "compact")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  rsel <- reselect(sel2, hers_diab)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers_diab[-7,])
  set.seed(10)
  sel_rep2 <- select_glmnet(diabetes ~ ., hers_diab, family = "binomial",lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))

})

test_that("elastic net works", {
  set.seed(10)
  sel <- select_glmnet(diabetes ~ ., hers_diab, alpha=0.5,family = "binomial")
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  rsel <- reselect(sel, hers_diab)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[-7,])
  set.seed(10)
  sel_rep <- select_glmnet(diabetes ~ ., hers_diab, alpha=0.5,family = "binomial")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(31)
  sel2 <- select_glmnet(diabetes ~ ., hers_diab,alpha=0.5,family = "binomial",
                        lambda = "compact")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  rsel <- reselect(sel2, hers_diab)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers_diab[-7,])
  set.seed(31)
  sel_rep2 <- select_glmnet(diabetes ~ ., hers_diab,alpha=0.5,family = "binomial",
                           lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))

})

#testthat::test_file("tests/testthat/test-select_glmnet.R")
