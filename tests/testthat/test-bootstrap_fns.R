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




test_that("Full model boot works ", {
  expect_silent({
    model<- lm(y~., data=cbind(y,x), x= TRUE, y=TRUE)
    obj1 <- full_boot(model, B=5, family="gaussian",parallel = TRUE)
    obj2 <-full_boot(model, B=5, family="gaussian",parallel = FALSE)

  })

})

test_that("Full model stepwsie aic boot bi-dirctional works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE)
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE)
    obj7 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj8 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj9 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })
})

test_that("Full model stepwsie aic boot forward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE,direction = "forward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE,direction = "forward")
    obj7 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj8 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj9 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")



  })
})

test_that("Full model stepwsie aic boot backward selection works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE,direction = "backward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod_nostd=step_ic (x=x,y=y,std = FALSE,direction = "backward")
    obj7 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj8 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T)
    obj9 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj12 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

  })
})

test_that("Full model stepwsie BIC boot bi-dirctional works ", {
  expect_silent({
    bic_mod=step_ic (x=x,y=y,std = TRUE,penalty = "BIC")
    obj1 <- infer(bic_mod, method = "boot", B=5)
    obj2 <-infer(bic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(bic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj6 <-infer(bic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    bic_mod_nostd=step_ic (x=x,y=y,std = FALSE,penalty = "BIC")
    obj7 <- infer(bic_mod_nostd, method = "boot", B=5)
    obj8 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=T)
    obj9 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj12 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })
})

test_that("Full model stepwsie BIC boot forward selection works ", {
  expect_silent({
    bic_mod=step_ic (x=x,y=y,std = TRUE,direction = "forward",penalty = "BIC")
    obj1 <- infer(bic_mod, method = "boot", B=5)
    obj2 <-infer(bic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(bic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj6 <-infer(bic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    bic_mod_nostd=step_ic (x=x,y=y,std = FALSE,direction = "forward",penalty = "BIC")
    obj7 <- infer(bic_mod_nostd, method = "boot", B=5)
    obj8 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=T)
    obj9 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj12 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")



  })
})

test_that("Full model stepwsie BIC boot backward selection works ", {

  expect_silent({
    bic_mod=step_ic (x=x,y=y,std = TRUE,direction = "backward",penalty = "BIC")
    obj1 <- infer(bic_mod, method = "boot", B=5)
    obj2 <-infer(bic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj5 <-infer(bic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj6 <-infer(bic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    bic_mod_nostd=step_ic (x=x,y=y,std = FALSE,direction = "backward",penalty = "BIC")
    obj7 <- infer(bic_mod_nostd, method = "boot", B=5)
    obj8 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=T)
    obj9 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj10 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",parallel=T)
    obj11 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")
    obj12 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

  })

})


