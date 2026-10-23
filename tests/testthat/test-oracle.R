# Oracle tests: rebuild each answer outside the package (raw model matrix,
# hand-scaled design, direct calls to lm/glm, MASS, glmnet, ncvreg) and
# compare. Unlike the self-consistency tests elsewhere, these catch errors
# that would be consistent across the whole package.

d <- fixture_mixed()
f <- y ~ x1 + x2 + x3 + x4 + g3 + sp + bin
X_raw <- raw_design(f, d)
X_std <- std_design(f, d)
sds <- attr(X_std, "scaled:scale")

slopes <- function(td) {
  td <- td[td$term != "(Intercept)", ]
  setNames(td$coef, norm_names(td$term))
}

test_that("full model: unscaled slopes equal lm() on the raw data", {
  sel <- select_full_model(f, d)
  ref <- coef(lm(f, d))[-1]
  names(ref) <- norm_names(names(ref))
  expect_equal(slopes(tidy(sel, scale_coef = FALSE))[names(ref)], ref)
  # scaled slopes are per SD of each design column, dummies included
  expect_equal(slopes(tidy(sel))[names(ref)], ref * sds[names(ref)])
})

test_that("full model: predict() equals lm() on unseen rows", {
  sel <- select_full_model(f, d[1:60, ])
  new <- d[61:80, ]
  expect_equal(predict(sel, new), unname(predict(lm(f, d[1:60, ]), new)))
  # a new-data set that lacks a factor level still predicts correctly
  new_lo <- new[new$g3 != "hi", ]
  expect_equal(predict(sel, new_lo),
               unname(predict(lm(f, d[1:60, ]), new_lo)))
})

test_that("stepwise (per dummy): same selections as MASS::stepAIC on raw columns", {
  sel <- select_stepwise_ic(f, d, direction = "both")
  raw <- data.frame(y = d$y, X_raw)
  ref <- MASS::stepAIC(glm(y ~ 1, data = raw),
                       scope = list(lower = ~ 1,
                                    upper = reformulate(colnames(X_raw))),
                       direction = "both", trace = 0)
  ref_b <- coef(ref)[-1]
  expect_setequal(names(slopes(tidy(sel)))[slopes(tidy(sel)) != 0],
                  names(ref_b))
  # and the unscaled coefficients are that model's raw coefficients
  expect_equal(slopes(tidy(sel, scale_coef = FALSE))[names(ref_b)], ref_b)
})

test_that("stepwise (factors together): same terms as MASS::stepAIC on raw data", {
  sel <- select_stepwise_ic(f, d, direction = "backward", penalty = "BIC",
                            select_factors_together = TRUE)
  ref <- MASS::stepAIC(glm(f, data = d), direction = "backward",
                       k = log(nrow(d)), trace = 0)
  expect_setequal(attr(sel, "selected_terms"),
                  attr(terms(ref), "term.labels"))
})

test_that("glmnet: coefficients equal glmnet() on the hand-scaled design", {
  set.seed(5)
  sel <- select_glmnet(f, d)
  lam <- attr(sel, "meta")$lambda_used
  ref <- glmnet::glmnet(X_std, d$y, lambda = sel$lambda, standardize = FALSE)
  b <- as.matrix(coef(ref, s = lam))[-1, 1]
  expect_equal(slopes(tidy(sel))[names(b)], b, tolerance = 1e-6)
})

test_that("glmnet binomial: coefficients equal glmnet() on the hand-scaled design", {
  set.seed(6)
  sel <- select_glmnet(update(f, y01 ~ .), d, family = "binomial")
  lam <- attr(sel, "meta")$lambda_used
  ref <- glmnet::glmnet(X_std, d$y01, family = "binomial",
                        lambda = sel$lambda, standardize = FALSE)
  b <- as.matrix(coef(ref, s = lam))[-1, 1]
  expect_equal(slopes(tidy(sel))[names(b)], b, tolerance = 1e-5)
})

test_that("ncvreg: coefficients equal ncvreg() on the hand-scaled design", {
  set.seed(7)
  sel <- select_ncvreg(f, d, penalty = "MCP")
  lam <- attr(sel, "meta")$lambda_used
  ref <- ncvreg::ncvreg(X_std, d$y, penalty = "MCP", lambda = sel$lambda)
  b <- coef(ref, lambda = lam)[-1]
  expect_equal(slopes(tidy(sel))[names(b)], b, tolerance = 1e-6)
})

test_that("UPSI: estimates and p-values equal lm() on the selected hand-scaled columns", {
  set.seed(5)
  sel <- select_glmnet(f, d, lambda = "compact")
  S <- names(which(slopes(tidy(sel)) != 0))
  fit <- lm(d$y ~ X_std[, S, drop = FALSE])
  ref <- summary(fit)$coefficients[-1, , drop = FALSE]
  ref_ci <- confint(fit)[-1, , drop = FALSE]

  inf <- tidy(infer_upsi(sel, d))
  inf <- inf[match(S, norm_names(inf$term)), ]
  expect_equal(inf$estimate, unname(ref[, "Estimate"]))
  expect_equal(inf$p_value, unname(ref[, "Pr(>|t|)"]))

  # unscaled estimates are the scaled ones divided by each column's SD
  un <- tidy(infer_upsi(sel, d), scale_coef = FALSE)
  un <- un[match(S, norm_names(un$term)), ]
  expect_equal(un$estimate, unname(ref[, "Estimate"] / sds[S]))
  expect_equal(un$ci_high - un$ci_low, (inf$ci_high - inf$ci_low) / sds[S],
               ignore_attr = TRUE)

  # Gaussian intervals come from glm() via broom, which uses normal
  # quantiles, while the p-values (summary.glm) use t: the intervals are
  # slightly too narrow and can disagree with the p-values
  expect_known_bug(isTRUE(all.equal(inf$ci_low, unname(ref_ci[, 1]))),
                   "UPSI gaussian intervals use z, not t")

  # uncertain_nulls: a nonselected term gets the estimate from adding it
  # alone to the selected model
  j <- setdiff(colnames(X_std), S)[1]
  ref_j <- coef(lm(d$y ~ X_std[, c(S, j)]))[length(S) + 2]
  unc <- tidy(infer_upsi(sel, d, nonselection = "uncertain_nulls"))
  expect_equal(unc$estimate[norm_names(unc$term) == j], unname(ref_j))
})

test_that("UPSI binomial: estimates equal glm() on the selected hand-scaled columns", {
  set.seed(6)
  sel <- select_glmnet(update(f, y01 ~ .), d, family = "binomial")
  S <- names(which(slopes(tidy(sel)) != 0))
  skip_if(length(S) == 0)
  ref <- coef(glm(d$y01 ~ X_std[, S, drop = FALSE], family = binomial))[-1]
  inf <- tidy(infer_upsi(sel, d))
  expect_equal(inf$estimate[match(S, norm_names(inf$term))], unname(ref),
               tolerance = 1e-6)
})
