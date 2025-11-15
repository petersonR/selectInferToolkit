# Computing mean/median CI ratio and significant Discoveries

Perfroms full model boostrap, stepwise AIC, stepwise BIc, lasso and MCP
and gives average CI for variables adjusting for post-selectino
inference using hybrid, bootstrap and selective inference

## Usage

``` r
ciratio(
  x,
  y,
  nonselection = "ignored",
  std = TRUE,
  B = 250,
  direction = "forward"
)
```

## Arguments

- x:

  Dataframe/model matrix with predictors (without intercept)

- y:

  outcome vector

- nonselection:

  A character string specifying how to handle variables not selected by
  model selection procedure. One of "ignored", "confident_nulls" or
  "uncertain_nulls" supported

- std:

  if TRUE (default), standardize design matrix

- B:

  \#of bootstraps

- direction:

  the mode of step wise search, can be one of "both", "backward", or
  "forward", with a default of "forward"

## Value

A list with following data frames

- avg_ci_ln:

  Mean of CI length across all variables in model excluding intercept

- med_ci_ln:

  Median of CI length across all variables in model excluding intercept

- no_sign_disc:

  \# of significant discoveries in model excluding intercept
