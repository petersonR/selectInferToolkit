# Edge cases in predictor types and factor levels. The core check is
# coef_tidy_consistent(): every nonzero coefficient must show up in tidy() as
# selected with the same value. Combinations that are currently broken are
# marked with expect_known_bug(), which turns into a failure once they pass.

edge_data <- function(f_values, n = 80, seed = 201) {
  set.seed(seed)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$f <- f_values(n)
  # a strong signal on the factor so that selectors keep it
  d$y <- d$x1 + 2 * as.numeric(as.factor(d$f)) + rnorm(n)
  d
}

edge_types <- list(
  syntactic = function(n) factor(sample(c("lo", "mid", "hi"), n, TRUE)),
  spaces    = function(n) factor(sample(c("much less", "same", "more"), n, TRUE)),
  punct     = function(n) factor(sample(c("a-b", "c/d", "e+f"), n, TRUE)),
  digit     = function(n) factor(sample(c("0", "1"), n, TRUE)),
  ordered   = function(n) factor(sample(c("lo", "mid", "hi"), n, TRUE),
                                 levels = c("lo", "mid", "hi"), ordered = TRUE),
  character = function(n) sample(c("p", "q"), n, TRUE),
  logical   = function(n) sample(c(TRUE, FALSE), n, TRUE),
  empty_lev = function(n) factor(sample(c("a", "b"), n, TRUE),
                                 levels = c("a", "b", "c"))
)

edge_selectors <- list(
  full     = function(d) select_full_model(y ~ ., d),
  stepwise = function(d) select_stepwise_ic(y ~ ., d, direction = "backward"),
  together = function(d) select_stepwise_ic(y ~ ., d, direction = "backward",
                                            select_factors_together = TRUE),
  glmnet   = function(d) { set.seed(1); select_glmnet(y ~ ., d) },
  ncvreg   = function(d) { set.seed(1); select_ncvreg(y ~ ., d) }
)

# Known failures as of this writing, "type/selector" -> description
known_bugs <- c(
  "digit/full"         = "#29 digit-leading levels reported as unselected",
  "digit/stepwise"     = "#29 digit-leading levels reported as unselected",
  "digit/glmnet"       = "#29 digit-leading levels reported as unselected",
  "digit/ncvreg"       = "#29 digit-leading levels reported as unselected",
  "ordered/full"       = "ordered factors error in select_full_model()",
  "ordered/glmnet"     = "ordered factors silently dropped by select_glmnet()",
  "ordered/ncvreg"     = "ordered factors error in select_ncvreg()",
  "character/full"     = "character predictors error in select_full_model()",
  "character/glmnet"   = "character predictors silently dropped by select_glmnet()",
  "character/ncvreg"   = "character predictors error in select_ncvreg()",
  "logical/full"       = "logical predictors error in select_full_model()",
  "logical/glmnet"     = "logical predictors reported as unselected",
  "logical/ncvreg"     = "logical predictors reported as unselected"
)

for (type in names(edge_types)) {
  for (s in names(edge_selectors)) {
    test_that(paste0("coef() and tidy() agree: ", type, " / ", s), {
      d <- edge_data(edge_types[[type]])
      ok <- tryCatch(
        suppressWarnings({
          sel <- edge_selectors[[s]](d)
          coef_tidy_consistent(sel) &&
            # the factor carries real signal; dropping it is a failure too
            any(grepl("^f", names(coef(sel)))) &&
            all(is.finite(predict(sel, d)))
        }),
        error = function(e) FALSE)
      key <- paste0(type, "/", s)
      if (key %in% names(known_bugs))
        expect_known_bug(ok, known_bugs[[key]])
      else
        expect_true(ok)
    })
  }
}

test_that("a single-level factor errors rather than returning nonsense", {
  d <- edge_data(function(n) factor(rep("a", n)))
  expect_error(select_full_model(y ~ ., d))
  expect_error(select_stepwise_ic(y ~ ., d))
})

test_that("reselect() on a resample missing a level keeps the design fixed", {
  d <- edge_data(edge_types$syntactic)
  sel <- select_full_model(y ~ ., d)
  part <- d[d$f != "mid", ]   # "hi" is the reference level
  rsel <- reselect(sel, part)
  expect_equal(tidy(rsel)$term, tidy(sel)$term)

  set.seed(3)
  sel_g <- select_glmnet(y ~ ., d)
  set.seed(3)
  expect_no_error(rsel_g <- reselect(sel_g, part))
  expect_equal(tidy(rsel_g)$term, tidy(sel_g)$term)

  # the absent level's dummy cannot be estimated, so it should not be
  # selected; currently its NA coefficient is reported as selected with 0
  expect_known_bug(tidy(rsel)$selected[tidy(rsel)$term == "fmid"] == 0,
                   "aliased (NA) coefficients reported as selected")
})

test_that("predict() with a level unseen in training does not return a number silently", {
  d <- edge_data(edge_types$syntactic)
  sel <- select_full_model(y ~ ., d)
  new <- d[1:3, ]
  new$f <- factor(c("lo", "mid", "new"))
  p <- tryCatch(suppressWarnings(predict(sel, new)), error = function(e) NULL)
  expect_true(is.null(p) || is.na(p[3]))
})
