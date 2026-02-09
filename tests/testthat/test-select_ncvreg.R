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
  sel <- select_ncvreg(Sepal.Length ~ ., iris, penalty ="lasso", alpha=1)
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
  sel_rep <- select_ncvreg(Sepal.Length ~ ., iris, penalty ="lasso", alpha=1)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

 set.seed(1)
  sel2 <- select_ncvreg(Sepal.Length ~ ., iris,lambda = "compact", penalty ="lasso", alpha=1)
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
  sel_rep2 <- select_ncvreg(Sepal.Length ~ ., iris, penalty ="lasso", alpha=1,lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))



})

test_that("MCP works", {
  set.seed(2)
  sel <-select_ncvreg(Sepal.Length ~ ., iris, penalty ="MCP", alpha=1)
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
  set.seed(2)
  sel_rep <- select_ncvreg(Sepal.Length ~ ., iris, penalty ="MCP", alpha=1)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))


  set.seed(2)
  sel2 <- select_ncvreg(Sepal.Length ~ ., iris,lambda = "compact", penalty ="MCP", alpha=1)
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

  set.seed(2)
  sel_rep2 <- select_ncvreg(Sepal.Length ~ ., iris, penalty ="MCP", alpha=1,lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))

})


###### Test HERS Data set continuous outcome ####
data("hers")
force(hers)

test_that("lasso  works", {
  set.seed(10)
  sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1)
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    print(tidy(sel,scale_coef = F))
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })
  set.seed(10)
  rsel <- reselect(sel, hers)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[-7,])
  set.seed(10)
  sel_rep <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(11)
  sel2 <- select_ncvreg(hdl1 ~ ., hers, lambda = "compact", penalty ="lasso", alpha=1)
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers[1:5,])
    predict(sel2, newdata = hers)
  })

  set.seed(11)
  rsel <- reselect(sel2, hers)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[-7,])

  set.seed(11)
  sel_rep2 <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1,lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))


})

test_that("MCP works", {
  set.seed(12)
  sel <- select_ncvreg(hdl1 ~ ., hers, alpha=1, penalty ="MCP")
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers[1:5,])
    predict(sel, newdata = hers)
  })

  set.seed(12)
  rsel <- reselect(sel, hers)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers[-7,])
  set.seed(1)
  sel_rep <- select_ncvreg(hdl1 ~ .,  hers, penalty ="MCP", alpha=1)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))


  sel2 <- select_ncvreg(hdl1 ~ ., hers, alpha=1,lambda = "compact", penalty ="MCP")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata = hers[1:5,])
    predict(sel2, newdata = hers)
  })


  rsel <- reselect(sel2, hers)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel2, newdata = hers[-7,])

  set.seed(1)
  sel_rep2 <- select_ncvreg(hdl1 ~ ., hers, penalty ="MCP", alpha=1,lambda = "compact")
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep2 ), tidy(sel2))


})

###### Test HERS Data set binary outcome ####
hers_diab <- hers  %>% select (-hdl1, -dmpills, -insulin)


test_that("lasso  works", {
  set.seed(2020)
  sel <- select_ncvreg(diabetes ~ ., hers_diab, family = "binomial", penalty ="lasso", alpha=1)
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  set.seed(2020)
  rsel <- reselect(sel, hers_diab)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = hers_diab[-7,])
  set.seed(2020)
  sel_rep <- select_ncvreg(diabetes ~ ., hers_diab, family = "binomial",  penalty ="lasso", alpha=1)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel))

  set.seed(2020)
  sel2 <- select_ncvreg(diabetes ~ ., hers_diab, family = "binomial",lambda = "compact", penalty ="lasso", alpha=1)
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

  set.seed(2020)
  rsel2 <- reselect(sel2, hers_diab)
  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel2)$coef - tidy(rsel2)$coef, na.rm = TRUE)
  #expect_lt(rsel_error, .05)
  # Try to re-fit with re-select to "new" data
  rsel3 <- reselect(sel2, newdata = hers_diab[-7,])
  set.seed(2020)
  sel_rep <- select_ncvreg(diabetes ~ ., hers_diab, family = "binomial", lambda = "compact", penalty ="lasso", alpha=1)
  # No variance across same method with same seed
  expect_equal(tidy( sel_rep ), tidy(sel2))


})

test_that("MCP works", {
  sel <- select_ncvreg(diabetes ~ ., hers_diab, family = "binomial",penalty ="MCP", alpha=1)
  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    tidy(sel,scale_coef = F)
    predict(sel, newdata =hers_diab[1:5,])
    predict(sel, newdata = hers_diab)
  })

  sel2 <- select_ncvreg(diabetes ~ ., hers_diab,alpha=1,family = "binomial",penalty ="MCP",
                        lambda = "compact")
  expect_no_warning({
    capture_output(print(sel2))
    tidy(sel2)
    tidy(sel2,scale_coef = F)
    predict(sel2, newdata =hers_diab[1:5,])
    predict(sel2, newdata = hers_diab)
  })

})

#testthat::test_file("tests/testthat/test-select_ncvreg.R")
