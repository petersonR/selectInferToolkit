# Computing mean/median CI ratio and significant Discoveries

Perfroms full model boostrap, stepwise AIC, stepwise BIc, lasso and MCP
and gives average CI for variables adjusting for post-selectino
inference using hybrid, bootstrap and selective inference

## Usage

``` r
ciratio(
  formula,
  data,
  family = c("gaussian", "binomial", "poisson"),
  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
  conf.level = 0.95,
  B = 250,
  direction = "forward",
  select_factors_together = F,
  debias = F,
  inference_target = "selections",
  ...
)
```

## Arguments

- formula:

  a formula

- data:

  data set

- family:

  outcome distributional family

- nonselection:

  A character string specifying how to handle variables not selected by
  model selection procedure. One of "ignored", "confident_nulls" or
  "uncertain_nulls" supported

- conf.level:

  .95 by default

- B:

  Number of bootstraps

- direction:

  the mode of step wise search, can be one of "both", "backward", or
  "forward", with a default of "forward"

- select_factors_together:

  should categorical variables be jointly selected?

- debias:

  should estimates be debiased in bootstrap

- inference_target:

  is inference requested on all or selected only

- ...:

  additional arguments

## Value

A list with following data frames

- avg_ci_ln:

  Mean of CI length across all variables in model excluding intercept

- med_ci_ln:

  Median of CI length across all variables in model excluding intercept

- no_sign_disc:

  \# of significant discoveries in model excluding intercept
