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


fullmod <- select_full_model(Sepal.Length ~ ., iris)

test_that("basic inferrer UPSI functionality", {

  expect_no_warning({
    inf <- infer_upsi(fullmod, data =iris)
    tidy(inf)
  })

  expect_no_warning({
    inf2 <- infer_upsi(fullmod, data = iris, nonselection = "uncertain_nulls")
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_upsi(fullmod, data = iris, nonselection = "confident_nulls")
    tidy(inf3)
  })

})

test_that("basic inferrer bootstrap functionality; full model", {

  expect_equal(unname(predict.glm(fullmod)), predict(fullmod, newdata = iris))


  # run vanilla version
  expect_no_warning({
    set.seed(1)
    inf1 <- infer_boot(fullmod , data = iris, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
    sum(vals1$selected==0) ==0
    sum(vals1$prop_selected==1) ==nrow(vals1)
  })

  # try debiasing everything
  expect_no_warning({
    set.seed(1)
    inf2 <- infer_boot(fullmod , data = iris, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
    sum(vals2$selected==0) ==0
    sum(vals2$prop_selected==1) ==nrow(vals2)


  })

  # try for "all" inference target
  expect_no_warning({
    set.seed(1)
    inf3 <- infer_boot(fullmod, data = iris, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
    sum(vals3$selected==0) ==0
    sum(vals3$prop_selected==1) ==nrow(vals3)

  })

  # try debiasing
  expect_no_warning({
    set.seed(1)
    inf4 <- infer_boot(fullmod,, data = iris, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
    sum(vals4$selected==0) ==0
    sum(vals4$prop_selected==1) ==nrow(vals4)

  })

  # with no selections and same seed, models should have same result
  expect_equal( vals1, vals2)
  expect_equal( vals3, vals4)


  # re-sampling
  rsel <- reselect(fullmod, iris)
  expect_identical(coef(fullmod), coef(rsel))
  expect_identical(predict(fullmod, newdata = iris), predict(rsel, newdata = iris))
  expect_identical(tidy(fullmod), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(fullmod, newdata = iris[1:50,])
  # in theory should at least have same selections
  # in practice a bit different due to pre-processing
  # expect_equal(
  #   names(coef(rsel2)),
  #   names(coef(select_full_model(Sepal.Length ~ ., iris[1:50,])))
  # )
})

###### Test IRIS data binary outcome ########

iris_binary = iris
iris$setosa_bin <- ifelse(iris$Species=="setosa",1,0)
iris$setosa_bin <-factor(iris$setosa_bin , levels = c(0,1),labels  = c("other","setosa"))
iris_binary = iris %>% select(-Species)


formula = "setosa_bin ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width"
fullmod <- select_full_model(as.formula(formula), iris_binary,family = "binomial" )


test_that("basic inferrer bootstrap functionality; full model", {

  expect_equal(unname(predict.glm(fullmod)), predict(fullmod, newdata = iris_binary))

  # run vanilla version
  expect_no_warning({
    set.seed(2)
    inf1 <- infer_boot(fullmod , data = iris_binary, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
    sum(vals1$selected==0) ==0
    sum(vals1$prop_selected==1) ==nrow(vals1)
    expect_equal(unname(predict.glm(fullmod)), predict(fullmod, newdata = mtcars))

  })

  # try debiasing everything
  expect_no_warning({
    set.seed(2)
    inf2 <- infer_boot(fullmod , data = iris_binary, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
    sum(vals2$selected==0) ==0
    sum(vals2$prop_selected==1) ==nrow(vals2)


  })

  # try for "all" inference target
  expect_no_warning({
    set.seed(2)
    inf3 <- infer_boot(fullmod, data = iris_binary, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
    sum(vals3$selected==0) ==0
    sum(vals3$prop_selected==1) ==nrow(vals3)

  })

  # try debiasing
  expect_no_warning({
    set.seed(2)
    inf4 <- infer_boot(fullmod,, data = iris_binary, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
    sum(vals4$selected==0) ==0
    sum(vals4$prop_selected==1) ==nrow(vals4)

  })

  expect_equal( vals1, vals2)
  expect_equal( vals3, vals4)

  # re-sampling
  rsel <- reselect(fullmod, iris_binary)
  expect_identical(coef(fullmod), coef(rsel))
  expect_identical(predict(fullmod, newdata = iris_binary), predict(rsel, newdata = iris_binary))
  expect_identical(tidy(fullmod), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(fullmod, newdata = iris_binary[1:50,])

  expect_equal(
    names(coef(rsel2)),
    names(coef(select_full_model(as.formula(formula), iris_binary[1:50,],
                                 family = "binomial")))
  )

})

###### Test HERS Data set continuous outcome ####
data("hers")
force(hers)
fullmod <- select_full_model(hdl1 ~ ., hers)

test_that("basic inferrer bootstrap functionality; full model", {

  expect_equal(unname(predict.glm(fullmod)), predict(fullmod, newdata = hers))


  # run vanilla version
  expect_no_warning({
    set.seed(3)
    inf1 <- infer_boot(fullmod , data = hers, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
    sum(vals1$selected==0) ==0
    sum(vals1$prop_selected==1) ==nrow(vals1)
  })

  # try debiasing everything
  expect_no_warning({
    set.seed(3)
    inf2 <- infer_boot(fullmod , data = hers, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
    sum(vals2$selected==0) ==0
    sum(vals2$prop_selected==1) ==nrow(vals2)


  })

  # try for "all" inference target
  expect_no_warning({
    set.seed(3)
    inf3 <- infer_boot(fullmod, data = hers, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
    sum(vals3$selected==0) ==0
    sum(vals3$prop_selected==1) ==nrow(vals3)

  })

  # try debiasing
  expect_no_warning({
    set.seed(3)
    inf4 <- infer_boot(fullmod,, data = hers, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
    sum(vals4$selected==0) ==0
    sum(vals4$prop_selected==1) ==nrow(vals4)

  })

  expect_equal( vals1, vals2)
  expect_equal( vals3, vals4)


  # re-sampling
  rsel <- reselect(fullmod, hers)
  expect_identical(coef(fullmod), coef(rsel))
  expect_identical(predict(fullmod, newdata = hers), predict(rsel, newdata = hers))
  expect_identical(tidy(fullmod), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(fullmod, newdata = hers[1:500,])
  # in theory should at least have same selections
  # in practice a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_full_model(hdl1  ~ ., hers[1:500,])))
  )
})

###### Test HERS Data set binary outcome ####
hers_diab <- hers  %>% select (-hdl1, -dmpills, -insulin)
#head(hers_diab)

fullmod <- select_full_model(diabetes ~ ., hers_diab, family = "binomial")

test_that("basic inferrer bootstrap functionality; full model", {

  expect_equal(unname(predict.glm(fullmod)), predict(fullmod, newdata = hers_diab))

  # run vanilla version
  expect_no_warning({
    set.seed(4)
    inf1 <- infer_boot(fullmod , data = hers_diab, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
    sum(vals1$selected==0) ==0
    sum(vals1$prop_selected==1) ==nrow(vals1)
  })

  # try debiasing everything
  expect_no_warning({
    set.seed(4)
    inf2 <- infer_boot(fullmod , data = hers_diab, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
    sum(vals2$selected==0) ==0
    sum(vals2$prop_selected==1) ==nrow(vals2)


  })

  # try for "all" inference target
  expect_no_warning({
    set.seed(4)
    inf3 <- infer_boot(fullmod, data = hers_diab, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
    sum(vals3$selected==0) ==0
    sum(vals3$prop_selected==1) ==nrow(vals3)

  })

  # try debiasing
  expect_no_warning({
    set.seed(4)
    inf4 <- infer_boot(fullmod,, data = hers_diab, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
    sum(vals4$selected==0) ==0
    sum(vals4$prop_selected==1) ==nrow(vals4)

  })

  expect_equal( vals1, vals2)
  expect_equal( vals3, vals4)


  # re-sampling
  rsel <- reselect(fullmod, hers_diab)
  expect_identical(coef(fullmod), coef(rsel))
  expect_identical(predict(fullmod, newdata = hers_diab), predict(rsel, newdata = hers_diab))
  expect_identical(tidy(fullmod), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(fullmod, newdata = hers_diab[1:500,])
  # in theory should at least have same selections
  # in practice a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_full_model(diabetes  ~ ., hers_diab[1:500,],family = "binomial")))
  )
})

