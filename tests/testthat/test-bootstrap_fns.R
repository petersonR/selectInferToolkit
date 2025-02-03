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

test_that("Full model stepwsie aic boot works ", {
  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE)
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")

    aic_mod=step_ic (x=x,y=y,std = FALSE)
    obj5 <- infer(aic_mod, method = "boot", B=5)
    obj6 <-infer(aic_mod, method = "boot", B=5, parallel=T)

  })

  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE,direction = "forward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })

  expect_silent({
    aic_mod=step_ic (x=x,y=y,std = TRUE,direction = "backward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <-infer(aic_mod, method = "boot", B=5, parallel=T)
    obj3 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj4 <-infer(aic_mod, method = "boot", B=5, parallel=T, nonselection = "uncertain_nulls")


  })
})

