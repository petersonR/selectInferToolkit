test_that("infer_boot() only accepts a cluster object", {
  d <- fixture_numeric()
  sel <- select_full_model(y ~ ., d)
  expect_error(infer_boot(sel, d, B = 2, cluster = 2),
               "must be NULL or a cluster")
})

test_that("sequential resampling leaves the RNG stream where the seeds left it", {
  d <- fixture_numeric()
  sel <- select_full_model(y ~ ., d)
  set.seed(5)
  suppressMessages(infer_boot(sel, d, B = 3))
  after_boot <- runif(1)
  set.seed(5)
  sample.int(.Machine$integer.max, 3)
  expect_identical(after_boot, runif(1))
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
