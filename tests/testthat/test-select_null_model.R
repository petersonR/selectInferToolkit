###### Test IRIS data continuous outcome ########

data(iris)
#iris <- iris[1:100,]

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


set.seed(123)
nullmod <- select_null_model(Sepal.Length ~ ., iris)


test_that("basic inferrer UPSI functionality", {

  expect_no_warning({
    inf <- infer_upsi(nullmod, data =iris)
    tidy(inf)
  })

  expect_no_warning({
    inf2 <- infer_upsi(nullmod, data = iris, nonselection = "uncertain_nulls")
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_upsi(nullmod, data = iris, nonselection = "confident_nulls")
    tidy(inf3)
  })

})

test_that("basic inferrer bootstrap functionality; null model", {

  expect_equal(unname(predict.glm(nullmod)), predict(nullmod, newdata = iris))

  # run vanilla version
  expect_no_warning({
    inf1 <- infer_boot(nullmod , data = iris, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
    sum(vals1$selected==0) ==nrow(vals1)-1
  })

  # try debiasing everything
  expect_no_warning({
    inf2 <- infer_boot(nullmod , data = iris, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
    sum(vals2$selected==0) ==nrow(vals2)-1

  })

  # try for "all" inference target
  expect_no_warning({
    inf3 <- infer_boot(nullmod, data = iris, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
    sum(vals3$selected==0) ==nrow(vals3)-1


  })

  # try debiasing
  expect_no_warning({
    inf4 <- infer_boot(nullmod,, data = iris, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
    sum(vals4$selected==0) ==nrow(vals4)-1


  })

  # re-sampling
  rsel <- reselect(nullmod, iris)
  expect_identical(coef(nullmod), coef(rsel))
  expect_identical(predict(nullmod, newdata = iris), predict(rsel, newdata = iris))
  expect_identical(tidy(nullmod), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(nullmod, newdata = iris[1:50,])
  # in theory should at least have same selections
  # in practice a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_null_model(Sepal.Length ~ ., iris[1:50,])))
    )


})

###### Test IRIS data binary outcome ########

iris_binary = iris
iris$setosa_bin <- ifelse(iris$Species=="setosa",1,0)
iris$setosa_bin <-factor(iris$setosa_bin , levels = c(0,1),labels  = c("other","setosa"))
iris_binary = iris %>% select(-Species)


formula = "setosa_bin ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width"
nullmod <- select_null_model(as.formula(formula), iris_binary,family = "binomial" )

test_that("basic inferrer UPSI functionality", {

  expect_no_error({
    inf <- infer_upsi(nullmod, data =iris_binary)
    tidy(inf)
  })

  expect_no_error({
    inf2 <- infer_upsi(nullmod, data = iris_binary, nonselection = "uncertain_nulls")
    tidy(inf2)
  })

  expect_no_error({
    inf3 <- infer_upsi(nullmod, data = iris_binary, nonselection = "confident_nulls")
    tidy(inf3)
  })

})

test_that("basic inferrer bootstrap functionality; null model", {

  # run vanilla version
  expect_no_error({
    inf1 <- infer_boot(nullmod , data = iris_binary, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
    sum(vals1$selected==0) ==nrow(vals1)-1

  })

  # try debiasing everything
  expect_no_error({
    inf2 <- infer_boot(nullmod , data = iris_binary, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
    sum(vals2$selected==0) ==nrow(vals2)-1


  })

  # try for "all" inference target
  expect_no_error({
    inf3 <- infer_boot(nullmod, data = iris_binary, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
    sum(vals3$selected==0) ==nrow(vals3)-1


  })

  # try debiasing
  expect_no_error({
    inf4 <- infer_boot(nullmod,, data = iris_binary, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
    sum(vals4$selected==0) ==nrow(vals4)-1


  })

  # re-sampling
  rsel <- reselect(nullmod, iris_binary)
  expect_identical(coef(nullmod), coef(rsel))
  expect_identical(predict(nullmod, newdata = iris_binary), predict(rsel, newdata = iris_binary))
  expect_identical(tidy(nullmod), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(nullmod, newdata = iris_binary[1:50,])
  # in theory should at least have same selections
  # in practice a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_null_model(as.formula(formula), iris_binary[1:50,],family = "binomial")))
  )

})



###### Test HERS Data set continuous outcome ####
data("hers")
force(hers)
nullmod <- select_null_model(hdl1 ~ ., hers)

test_that("basic inferrer UPSI functionality", {

  expect_no_warning({
    inf <- infer_upsi(nullmod, data =hers)
    tidy(inf)
  })

  expect_no_warning({
    inf2 <- infer_upsi(nullmod, data = hers, nonselection = "uncertain_nulls")
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_upsi(nullmod, data = hers, nonselection = "confident_nulls")
    tidy(inf3)
  })

})


test_that("basic inferrer bootstrap functionality; null model", {

  # run vanilla version
  expect_no_warning({
    inf1 <- infer_boot(nullmod , data = hers, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
    sum(vals1$selected==0) ==nrow(vals1)-1
  })

  # try debiasing everything
  expect_no_warning({
    inf2 <- infer_boot(nullmod , data = hers, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
    sum(vals2$selected==0) ==nrow(vals2)-1

  })

  # try for "all" inference target
  expect_no_warning({
    inf3 <- infer_boot(nullmod, data = hers, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
    sum(vals3$selected==0) ==nrow(vals3)-1

  })

  # try debiasing
  expect_no_warning({
    inf4 <- infer_boot(nullmod,, data = hers, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
    sum(vals4$selected==0) ==nrow(vals4)-1

  })


  # re-sampling
  rsel <- reselect(nullmod, hers)
  expect_identical(coef(nullmod), coef(rsel))
  expect_identical(predict(nullmod, newdata = hers), predict(rsel, newdata = hers))
  expect_identical(tidy(nullmod), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(nullmod, newdata = hers[1:500,])
  # in theory should at least have same selections
  # in practice a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_null_model(hdl1  ~ ., hers[1:500,])))
  )
})

###### Test HERS Data set binary outcome ####
hers_diab <- hers  %>% select (-hdl1, -dmpills, -insulin)
#head(hers_diab)

nullmod <- select_null_model(diabetes ~ ., hers_diab, family = "binomial")

test_that("basic inferrer UPSI functionality", {

  expect_no_warning({
    inf <- infer_upsi(nullmod, data =hers_diab)
    tidy(inf)
  })

  expect_no_warning({
    inf2 <- infer_upsi(nullmod, data = hers_diab, nonselection = "uncertain_nulls")
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_upsi(nullmod, data = hers_diab, nonselection = "confident_nulls")
    tidy(inf3)
  })

})



test_that("basic inferrer bootstrap functionality; null model", {

  # run vanilla version
  expect_no_warning({
    inf1 <- infer_boot(nullmod , data = hers_diab, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
    sum(vals1$selected==0) ==nrow(vals1)-1
  })

  # try debiasing everything
  expect_no_warning({
    inf2 <- infer_boot(nullmod , data = hers_diab, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
    sum(vals2$selected==0) ==nrow(vals2)-1


  })

  # try for "all" inference target
  expect_no_warning({
    inf3 <- infer_boot(nullmod, data = hers_diab, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
    sum(vals3$selected==0) ==nrow(vals3)-1

  })

  # try debiasing
  expect_no_warning({
    inf4 <- infer_boot(nullmod,, data = hers_diab, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
    sum(vals4$selected==0) ==nrow(vals4)-1

  })


  # re-sampling
  rsel <- reselect(nullmod, hers_diab)
  expect_identical(coef(nullmod), coef(rsel))
  expect_identical(predict(nullmod, newdata = hers_diab), predict(rsel, newdata = hers_diab))
  expect_identical(tidy(nullmod), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(nullmod, newdata = hers_diab[1:500,])
  # in theory should at least have same selections
  # in practice a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_null_model(diabetes  ~ ., hers_diab[1:500,],family = "binomial")))
  )
})

#testthat::test_file("tests/testthat/test-select_null_model.R")

