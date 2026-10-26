test_that("infer_boot() only accepts a cluster object", {
  d <- fixture_numeric()
  sel <- select_full_model(y ~ ., d)
  expect_error(infer_boot(sel, d, B = 2, cluster = 2),
               "must be NULL or a cluster")
})

test_that("resampling leaves the session's generator, one draw on", {
  d <- fixture_numeric()
  sel <- select_full_model(y ~ ., d)
  set.seed(5)
  suppressMessages(infer_boot(sel, d, B = 3))
  expect_identical(RNGkind()[1], "Mersenne-Twister")
  after_boot <- runif(1)
  set.seed(5)
  runif(1)
  expect_identical(after_boot, runif(1))
})

test_that("resamples draw from L'Ecuyer-CMRG streams of the session's state", {
  d <- fixture_numeric()
  sel <- select_full_model(y ~ ., d)
  set.seed(5)
  inf <- suppressMessages(infer_boot(sel, d, B = 2, pad = "none"))

  # rebuilt by hand: the streams start from the generator seeded by the session's
  set.seed(5)
  RNGkind("L'Ecuyer-CMRG")
  s1 <- parallel::nextRNGStream(.Random.seed)
  s2 <- parallel::nextRNGStream(s1)
  rows <- lapply(list(s1, s2), function(s) {
    assign(".Random.seed", s, envir = globalenv())
    sample(1:nrow(d), replace = TRUE)
  })
  RNGkind("Mersenne-Twister")
  X <- scale(model.matrix(y ~ ., d)[, -1])
  b <- vapply(rows, function(i) coef(lm(d$y[i] ~ X[i, ]))[[2]], numeric(1))
  expect_equal(inf$estimate[inf$term == colnames(X)[1]], b,
               ignore_attr = TRUE)
})

test_that("a Rounding sampler doesn't make every resample warn", {
  d <- fixture_numeric()
  sel <- select_full_model(y ~ ., d)
  old <- RNGkind()
  on.exit(RNGkind(old[1], old[2], old[3]))
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  set.seed(5)
  expect_no_warning(suppressMessages(infer_boot(sel, d, B = 3, pad = "none")))
  expect_identical(RNGkind()[3], "Rounding")
})

test_that("a cluster gives the same results as sequential resampling", {
  skip_on_cran()
  skip_on_os("windows")  # fork clusters share this session's package build

  d <- fixture_mixed()
  f <- y ~ x1 + x2 + x3 + x4 + g3 + sp + bin
  cl <- parallel::makeForkCluster(2)
  on.exit(parallel::stopCluster(cl))

  sels <- list(
    select_stepwise_ic(f, d),
    select_ncvreg(f, d, penalty = "MCP"),
    select_glmnet(f, d),
    # reselection draws random numbers (in selectiveInference::fsInf())
    select_stepwise_ic(f, d, criterion = "cp")
  )
  for (sel in sels) {
    set.seed(7)
    serial <- suppressMessages(suppressWarnings(infer_boot(sel, d, B = 8)))
    set.seed(7)
    par <- suppressMessages(suppressWarnings(
      infer_boot(sel, d, B = 8, cluster = cl)))
    expect_equal(tidy(par), tidy(serial))
  }

  # the workers don't keep the data once infer_boot() returns
  left <- parallel::clusterEvalQ(cl, length(ls(
    get(".boot_worker", asNamespace("selectInferToolkit")))))
  expect_true(all(unlist(left) == 0))

  # and get back the streams they had
  parallel::clusterSetRNGStream(cl, 11)
  before <- parallel::clusterEvalQ(cl, .Random.seed)
  suppressMessages(suppressWarnings(infer_boot(sels[[1]], d, B = 4, cluster = cl)))
  expect_identical(parallel::clusterEvalQ(cl, .Random.seed), before)

  # and the results don't depend on how many workers there are
  cl1 <- parallel::makeForkCluster(1)
  on.exit(parallel::stopCluster(cl1), add = TRUE)
  set.seed(7)
  one <- suppressMessages(suppressWarnings(
    infer_boot(sels[[2]], d, B = 8, cluster = cl1)))
  set.seed(7)
  two <- suppressMessages(suppressWarnings(
    infer_boot(sels[[2]], d, B = 8, cluster = cl)))
  expect_equal(tidy(one), tidy(two))
})

test_that("warnings from the resample fits are reported the same with a cluster", {
  skip_on_cran()
  skip_on_os("windows")

  # strong effect, small n: some resamples separate, and glm() warns
  set.seed(3)
  n <- 40
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- rbinom(n, 1, plogis(4 * d$x1))
  sel <- suppressWarnings(select_stepwise_ic(y ~ x1 + x2, d, family = "binomial"))

  collect <- function(...) {
    ws <- character()
    set.seed(8)
    withCallingHandlers(suppressMessages(infer_boot(sel, d, B = 20, pad = "none", ...)),
      warning = function(w) {
        ws <<- c(ws, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    ws
  }
  cl <- parallel::makeForkCluster(2)
  on.exit(parallel::stopCluster(cl))
  serial <- collect()
  par <- collect(cluster = cl)
  expect_match(serial, "^In [0-9]+ of 20 resamples: ", all = FALSE)
  expect_identical(par, serial)
})
