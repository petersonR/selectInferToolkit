# Characterization ("golden") tests: pin down the current numeric output of
# every selector and inference method so that a refactor shows up as a
# reviewable snapshot diff. Term names are normalized (helper golden()), so
# a pure renaming does not show up as a change.
#
# After an intentional change, review with testthat::snapshot_review("golden")
# and accept with testthat::snapshot_accept("golden").
#
# Skipped on CRAN: these are a development safety net, not a check of
# correctness, and the bootstrap makes them the slowest tests here.

skip_on_cran()

dm <- fixture_mixed()
dn <- fixture_numeric()
f_mixed <- y ~ x1 + x2 + x3 + x4 + g3 + sp + bin
f_bin <- y01 ~ x1 + x2 + x3 + x4 + g3 + sp + bin

# Printed tables rather than serialized values, so a diff reads row by row.
# Numbers are rounded to 6 significant digits to absorb platform noise.
expect_golden <- function(x, label) {
  local_reproducible_output(width = 200)
  x <- golden(x, digits = 6)
  expect_snapshot({
    cat(label, "\n")
    print(x, row.names = FALSE, digits = 6)
  })
}

golden_selector <- function(sel, data) {
  expect_golden(tidy(sel), "tidy(selector)")
  expect_golden(tidy(sel, scale_coef = FALSE), "tidy(selector, scale_coef = FALSE)")
  expect_golden(data.frame(pred = predict(sel, data[1:10, ])), "predict(selector)")
}

golden_upsi <- function(sel, data) {
  expect_golden(tidy(infer_upsi(sel, data)), "infer_upsi()")
  expect_golden(tidy(infer_upsi(sel, data, nonselection = "uncertain_nulls"),
                     scale_coef = FALSE),
                "infer_upsi(uncertain_nulls), unscaled")
}

golden_boot <- function(sel, data, ...) {
  set.seed(42)
  inf <- suppressMessages(suppressWarnings(infer_boot(sel, data, B = 10, ...)))
  expect_golden(tidy(inf), "infer_boot(B = 10)")
}

test_that("golden: full and null models", {
  sel <- select_full_model(f_mixed, dm)
  golden_selector(sel, dm)
  golden_upsi(sel, dm)

  sel <- select_null_model(f_mixed, dm)
  golden_selector(sel, dm)
  golden_upsi(sel, dm)
})

test_that("golden: stepwise, per dummy", {
  sel <- select_stepwise_ic(f_mixed, dm, direction = "both")
  golden_selector(sel, dm)
  golden_upsi(sel, dm)
  golden_boot(sel, dm)
})

test_that("golden: stepwise, factors together", {
  sel <- select_stepwise_ic(f_mixed, dm, direction = "backward",
                            penalty = "BIC", select_factors_together = TRUE)
  golden_selector(sel, dm)
  golden_upsi(sel, dm)
  golden_boot(sel, dm, pad = "none")
})

test_that("golden: stepwise binomial", {
  sel <- select_stepwise_ic(f_bin, dm, family = "binomial", penalty = "BIC")
  golden_selector(sel, dm)
  golden_upsi(sel, dm)
})

test_that("golden: stepwise cp with selective inference", {
  sel <- select_stepwise_ic(y ~ ., dn, criterion = "cp", penalty = "BIC")
  golden_selector(sel, dn)
  expect_golden(tidy(infer_selective(sel, dn)), "infer_selective()")
})

test_that("golden: glmnet lasso", {
  set.seed(11)
  sel <- select_glmnet(f_mixed, dm)
  golden_selector(sel, dm)
  golden_upsi(sel, dm)
  golden_boot(sel, dm, pad = "all")
  expect_golden(tidy(suppressWarnings(infer_selective(sel, dm))),
                "infer_selective()")
})

test_that("golden: glmnet binomial", {
  set.seed(12)
  sel <- select_glmnet(f_bin, dm, family = "binomial")
  golden_selector(sel, dm)
  golden_upsi(sel, dm)
  expect_golden(tidy(suppressWarnings(infer_selective(sel, dm))),
                "infer_selective()")
})

test_that("golden: ncvreg MCP", {
  set.seed(13)
  sel <- select_ncvreg(f_mixed, dm, penalty = "MCP")
  golden_selector(sel, dm)
  golden_upsi(sel, dm)
  golden_boot(sel, dm)
})
