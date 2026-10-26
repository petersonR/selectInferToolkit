
<!-- README.md is generated from README.Rmd. Please edit that file -->

# selectInferToolkit

<!-- badges: start -->

[![R-CMD-check](https://github.com/petersonR/selectInferToolkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/petersonR/selectInferToolkit/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/petersonR/selectInferToolkit/graph/badge.svg)](https://app.codecov.io/gh/petersonR/selectInferToolkit)
<!-- badges: end -->

The `selectInferToolkit` package addresses a critical challenge in
modern statistical modeling: **conducting valid inference after
performing model selection.** In the era of big data and complex
regression problems, researchers often employ model selection procedures
like stepwise regression or penalized methods (e.g., lasso) to identify
important variables. However, classical statistical inference assumes
the model was chosen *a priori*—independent of the data. When this
assumption is violated, standard statistical tests and confidence
intervals can become overly optimistic, leading to unreliable
conclusions.`selectInferToolkit` provides a user-friendly framework in R
to facilitate post-selection inferential methods.

## Installation

You can install the development version of selectInferToolkit like so:

``` r
devtools::install_github("petersonR/selectInferToolkit")
```

## Package Overview

`selectInferToolkit` offers a comprehensive suite of tools, covering
both model selection and the crucial post-selection inference steps.

### Model Selection Methods

The package supports two broad classes of popular model selection
techniques:

- **Stepwise Regression:** Implement forward, backward, and
  bidirectional stepwise regression. User can use either the **Akaike
  Information Criterion (AIC)** or the **Bayesian Information Criterion
  (BIC)** as selection criteria, leveraging the `stepAIC` function from
  the `MASS` package.
- **Penalized Regression:** Perform lasso and elastic net regression
  with `glmnet`, or MCP, SCAD, and lasso regression with `ncvreg`.
  `selectInferToolkit` simplifies the process of obtaining models
  associated with either $\lambda_{min}$ or $\lambda_{1se}$.
  - $\lambda_{min}$ represents the regularization parameter $\lambda$
    that minimizes the cross-validation error, aiming for the best
    predictive performance.
  - $\lambda_{1se}$ is a more conservative choice, selecting the largest
    $\lambda$ where the cross-validation error is within one standard
    error of $\lambda_{min}$. This often results in a sparser, more
    parsimonious model.

### Post-Selection Inference Methods

The core contribution of `selectInferToolkit` lies in its ability to
provide straightforward implementations of four post-selection inference
methods, applicable after you’ve performed model selection using one of
the methods above:

1.  **UPSI**: A baseline approach that performs no adjustment for the
    selection process. We refer to this as “unadjusted post-selection
    inference”, or “UPSI” for short. Others refer to this as
    “hybrid-OLS” (in the Gaussian outcome case).
2.  **Bootstrap:** A non-parametric bootstrap method that resamples data
    to account for selection uncertainty.
3.  **Selective Inference:** A theoretically grounded approach,
    currently available for lasso and forward stepwise selection through
    integration with the `selectiveInference` package.
4.  **PIPE**: \[currently experimental\]

### Handling Non-Selected Variables

`selectInferToolkit` is explicit in its handling of non-selections. For
`infer_upsi()` and `infer_selective()`, the `nonselection` argument can
be set to:

- **Ignored:** Treating non-selected variables as irrelevant.
- **Confident (point-mass) nulls:** Assuming these variables have
  exactly zero effect.
- **Uncertain nulls:** Aim to make inference on all variables but
  without the assumption that non-selections certainly have a null
  effect

The bootstrap handles non-selections through its `inference_target` and
`pad` arguments instead (below), and PIPE gives inference for every
variable. See `?infer_upsi` for exactly what each `nonselection` mode
returns.

### Bootstrapping considerations

In bootstrapping, the selection is re-run on each resample and a
non-selected coefficient is recorded as 0. The user specifies their
`inference_target` as either `"selections"` or `"all"`.

Recording non-selections as 0 makes the percentile interval of a small,
rarely selected coefficient collapse toward $[0, 0]$. The `pad` argument
corrects this. The default, `pad = "zero_side"`, pushes the endpoints of
any interval that contains 0 out to at least $\pm\delta$, a region of
*reasonably confident null* that tends to shrink as $n$ grows
($\delta = \gamma\lambda_{\text{eff}}$ for MCP/SCAD,
$\sqrt{k}\cdot\mathrm{SE}$ for stepwise IC); intervals that exclude 0
are unchanged. `pad = "all"` widens every interval by $\delta$ (the
alternative to consider for the lasso, for which zero-side padding
warns), and `pad = "none"` returns the raw percentile intervals
(confident nulls). The radius is reported in the `null_radius` column.

Resamples can be fit in parallel by passing a cluster you’ve made with
the `parallel` package, e.g. `cl <- parallel::makeCluster(4)`, then
`infer_boot(sel, data, cluster = cl)` and `parallel::stopCluster(cl)`
when you’re done. Results for a given seed are the same with or without
a cluster, and a progress bar is shown either way.

## Example

Let’s say you are wanting to predict gas mileage based on all variables
in the `mtcars` data set.

``` r
library(selectInferToolkit)
#> Loading required package: broom
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

data("mtcars")

fit_full <- lm(mpg ~ ., data = mtcars)
summary(fit_full)
#> 
#> Call:
#> lm(formula = mpg ~ ., data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.4506 -1.6044 -0.1196  1.2193  4.6271 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept) 12.30337   18.71788   0.657   0.5181  
#> cyl         -0.11144    1.04502  -0.107   0.9161  
#> disp         0.01334    0.01786   0.747   0.4635  
#> hp          -0.02148    0.02177  -0.987   0.3350  
#> drat         0.78711    1.63537   0.481   0.6353  
#> wt          -3.71530    1.89441  -1.961   0.0633 .
#> qsec         0.82104    0.73084   1.123   0.2739  
#> vs           0.31776    2.10451   0.151   0.8814  
#> am           2.52023    2.05665   1.225   0.2340  
#> gear         0.65541    1.49326   0.439   0.6652  
#> carb        -0.19942    0.82875  -0.241   0.8122  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.65 on 21 degrees of freedom
#> Multiple R-squared:  0.869,  Adjusted R-squared:  0.8066 
#> F-statistic: 13.93 on 10 and 21 DF,  p-value: 3.793e-07
```

Hmm, $R^2$ is high but nothing is “significant”. What is going on? The
model is probably over-specified. Let’s use `selectInferToolkit` to
narrow in on what we think the most important factors are. The
`select_stepwise_ic()` function does forward selection via AIC by
default (backward and bidirectional searches are available through
`direction`).

### Stepwise selection

``` r
fit_aic <- select_stepwise_ic(mpg ~ ., data = mtcars, penalty = "AIC") 
fit_aic
#> Stepwise IC-based selector
#> 3 of 10 variables selected: hp, cyl, wt 
#> Meta information: family=gaussian, select_factors_together=FALSE, penalty=AIC, direction=forward, criterion=deviance, sigma_arg=NULL, sigma_used=NULL 
#> Default `infer()` method: boot
```

Functions prefixed with `select_` create a `selector` class object. We
can pass `selector` objects to `coef`, `predict`, and `tidy()`
functions:

``` r
coef(fit_aic)
#> (Intercept)          wt         cyl          hp 
#>   20.090625   -3.098748   -1.681654   -1.236744
tidy(fit_aic)
#> # A tibble: 11 × 3
#>    term        selected  coef
#>    <chr>          <dbl> <dbl>
#>  1 (Intercept)        1 20.1 
#>  2 cyl                1 -1.68
#>  3 disp               0  0   
#>  4 hp                 1 -1.24
#>  5 drat               0  0   
#>  6 wt                 1 -3.10
#>  7 qsec               0  0   
#>  8 vs                 0  0   
#>  9 am                 0  0   
#> 10 gear               0  0   
#> 11 carb               0  0
```

Note that the predictors are standardized before selection, so by
default these coefficients are the change in `mpg` per standard
deviation of each predictor. Use `tidy(fit_aic, scale_coef = FALSE)` for
per-unit effects (see `?tidy.selector`).

So we find that AIC selects `wt`, `cyl`, and `hp`. But where are the
p-values?!

This is where post-selection inference comes in; p-values that do not
adjust for the selective process will be too small. Again, we call this
“unadjusted post-selection inference” (or UPSI, for short).

``` r
fit_aic_infer_upsi <- infer_upsi(fit_aic, data = mtcars)
fit_aic_infer_upsi
#> Unadjusted Post-Selection Inference (UPSI) inference applied post Stepwise IC-based Selection 
#> 3 of 10 variables selected. 
#> Nonselections were considered ignored 
#> Meta information: none 
#> Median CI length = 3.2; mean = 3; max = 4
tidy(fit_aic_infer_upsi) %>% dplyr::filter(selected == 1)
#> # A tibble: 4 × 7
#>   term        selected  coef estimate ci_low ci_high  p_value
#>   <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>    <dbl>
#> 1 (Intercept)        1 20.1     20.1   19.2   21.0   9.94e-28
#> 2 cyl                1 -1.68    -1.68  -3.70   0.334 9.85e- 2
#> 3 hp                 1 -1.24    -1.24  -2.90   0.431 1.40e- 1
#> 4 wt                 1 -3.10    -3.10  -4.58  -1.61  1.99e- 4
```

Thus, `wt` is significant! We know this confidence interval may be
biased and too precise, as we’ve used the data both to select the model
and now are using the same data to perform inference (UPSI!). What if we
wanted to adjust our uncertainty quantification for the selective
process?

Each selection process has a default `infer` method that is, for the
most part, not an UPSI method. The default for stepwise selection via
information criteria is bootstrapping. The code below is equivalent to
calling `infer_boot`.

``` r
set.seed(1)
fit_aic_infer_boot <- infer(fit_aic, data = mtcars, B = 100)
#> Default inference method is bootstrapping (infer_boot). This may take a moment. Set a seed for reproducibility, or use infer_upsi() / infer_selective() for faster alternatives.
#> Bootstrapping with B = 100 replicates...
fit_aic_infer_boot
#> Bootstrap inference applied post Stepwise IC-based Selection 
#> 3 of 10 variables selected. 
#> Nonselections were considered N/A 
#> Meta information: B=100, inference_target=selections, pad=zero_side 
#> Median CI length = 6.8; mean = 6.4; max = 9.8
tidy(fit_aic_infer_boot) 
#> # A tibble: 11 × 8
#>    term        selected  coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercept)        1 20.1    20.3    19.2    21.1           1          NA   
#>  2 cyl                1 -1.68   -0.515  -3.65    3.90          0.56        2.64
#>  3 disp               0  0      NA      NA      NA            NA           3.13
#>  4 hp                 1 -1.24   -0.706  -3.44    2.67          0.49        2.11
#>  5 drat               0  0      NA      NA      NA            NA           1.24
#>  6 wt                 1 -3.10   -3.46   -7.19    2.62          0.94        2.62
#>  7 qsec               0  0      NA      NA      NA            NA           1.85
#>  8 vs                 0  0      NA      NA      NA            NA           1.50
#>  9 am                 0  0      NA      NA      NA            NA           1.45
#> 10 gear               0  0      NA      NA      NA            NA           1.56
#> 11 carb               0  0      NA      NA      NA            NA           1.89
```

These results are less promising. Here we can also use a wrapper for the
`selectiveInference` method. Selective inference for stepwise selection
conditions on the Mallows-Cp form of the criterion, so we select with
`criterion = "cp"` to make the selection and the inference agree (with
the default `criterion = "deviance"`, the two can stop at different
steps, and `infer_selective()` warns and falls back to conditioning on a
fixed number of steps):

``` r
fit_aic_cp <- select_stepwise_ic(mpg ~ ., data = mtcars, penalty = "AIC", criterion = "cp")
fit_aic_SI <- infer_selective(fit_aic_cp, data = mtcars)
fit_aic_SI 
#> Selective inference applied post Stepwise IC-based Selection 
#> 3 of 10 variables selected. 
#> Nonselections were considered ignored 
#> Meta information: conditioning=aic 
#> Median CI length = Inf; mean = Inf; max = Inf
tidy(fit_aic_SI) 
#> # A tibble: 11 × 7
#>    term        selected  coef estimate ci_low ci_high p_value
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>   <dbl>
#>  1 (Intercept)        1 20.1     NA      NA     NA     NA    
#>  2 cyl                1 -1.68    -1.68 -Inf     30.4    0.235
#>  3 disp               0  0       NA      NA     NA     NA    
#>  4 hp                 1 -1.24    -1.24  -25.5  Inf      0.795
#>  5 drat               0  0       NA      NA     NA     NA    
#>  6 wt                 1 -3.10    -3.10  -11.6    5.87   0.224
#>  7 qsec               0  0       NA      NA     NA     NA    
#>  8 vs                 0  0       NA      NA     NA     NA    
#>  9 am                 0  0       NA      NA     NA     NA    
#> 10 gear               0  0       NA      NA     NA     NA    
#> 11 carb               0  0       NA      NA     NA     NA
```

Here we see that after adjusting for the selective process, we are
unable to claim significance of any of these effects, and the CIs are
very wide (some are infinite).

Lastly, as stated above, `infer_upsi()` and `infer_selective()` provide
three approaches to handling non-selections: `ignored` (ignoring them),
`confident_nulls` (treating them as confident (point-mass) degenerate
nulls at zero), and `uncertain_nulls` (treating them as uncertain). For
details about each of these, see `?infer_upsi`.

### Penalized regression

#### Lasso via glmnet

We can also try selecting the most important factors are by penalized
models. We can fit the lasso with cross validation and selects
coefficients associated with the `best` predicting model (default,
i.e. `lambda.min`) or the most `compact` model that predicts about as
well as the best model, i.e. `lambda.1se`.

``` r
set.seed(12)
fit_lso <- select_glmnet(mpg ~ ., data = mtcars) 
fit_lso 
#> Penalized `glmnet`-based selector
#> 3 of 10 variables selected: hp, cyl, wt 
#> Meta information: family=gaussian, lambda=best, lambda_used=0.788, cv_info=list(...), ellipses=list(...), alpha=1 
#> Default `infer()` method: selective
tidy(fit_lso)
#> # A tibble: 11 × 3
#>    term        selected   coef
#>    <chr>          <dbl>  <dbl>
#>  1 (Intercept)        1 20.1  
#>  2 cyl                1 -1.58 
#>  3 disp               0  0    
#>  4 hp                 1 -0.801
#>  5 drat               0  0    
#>  6 wt                 1 -2.65 
#>  7 qsec               0  0    
#>  8 vs                 0  0    
#>  9 am                 0  0    
#> 10 gear               0  0    
#> 11 carb               0  0
```

The lasso selects the same 3 variables as stepwise AIC, with
coefficients shrunk toward zero. The lasso alone does not provide any
p-value or CI to do inference, but UPSIs aside we can again perform a
(questionable) hybrid inferential process where ordinary least squares
theory is used for inference as though we never used the data for
selection.

``` r
fit_lasso_infer_upsi <-infer_upsi(fit_lso, data = mtcars) 
tidy(fit_lasso_infer_upsi)
#> # A tibble: 11 × 7
#>    term        selected   coef estimate ci_low ci_high   p_value
#>    <chr>          <dbl>  <dbl>    <dbl>  <dbl>   <dbl>     <dbl>
#>  1 (Intercept)        1 20.1      20.1   19.2   21.0    9.94e-28
#>  2 cyl                1 -1.58     -1.68  -3.70   0.334  9.85e- 2
#>  3 disp               0  0        NA     NA     NA     NA       
#>  4 hp                 1 -0.801    -1.24  -2.90   0.431  1.40e- 1
#>  5 drat               0  0        NA     NA     NA     NA       
#>  6 wt                 1 -2.65     -3.10  -4.58  -1.61   1.99e- 4
#>  7 qsec               0  0        NA     NA     NA     NA       
#>  8 vs                 0  0        NA     NA     NA     NA       
#>  9 am                 0  0        NA     NA     NA     NA       
#> 10 gear               0  0        NA     NA     NA     NA       
#> 11 carb               0  0        NA     NA     NA     NA
```

These results are close to the UPSI results for stepwise AIC, since the
two selected models share `cyl`, `hp`, and `wt`; `wt` is still seemingly
significant.

*Selective inference*

``` r
fit_lasso_infer_SI <- infer_selective(fit_lso, data = mtcars)
tidy(fit_lasso_infer_SI)
#> # A tibble: 11 × 7
#>    term        selected   coef estimate ci_low ci_high    p_value
#>    <chr>          <dbl>  <dbl>    <dbl>  <dbl>   <dbl>      <dbl>
#>  1 (Intercept)        1 20.1      NA     NA      NA    NA        
#>  2 cyl                1 -1.58     -1.68  -4.82    1.24  0.112    
#>  3 disp               0  0        NA     NA      NA    NA        
#>  4 hp                 1 -0.801    -1.24  -3.15    2.36  0.244    
#>  5 drat               0  0        NA     NA      NA    NA        
#>  6 wt                 1 -2.65     -3.10  -4.70   -1.57  0.0000908
#>  7 qsec               0  0        NA     NA      NA    NA        
#>  8 vs                 0  0        NA     NA      NA    NA        
#>  9 am                 0  0        NA     NA      NA    NA        
#> 10 gear               0  0        NA     NA      NA    NA        
#> 11 carb               0  0        NA     NA      NA    NA
```

Here we see that after adjusting for the selective process with
selective inference, we are able to assert a significant effect of `wt`
even after the selective process, though its CI is considerably wider
than the UPSI interval.

*Bootstrapping*

The lasso’s bias does not vanish for large effects, so zero-side padding
warns for a lasso selector. The alternative is to pad every interval by
$\hat\lambda$:

``` r
set.seed(1)
fit_lasso_infer_boot <- infer_boot(fit_lso, data = mtcars, B = 100, inference_target = "all", pad = "all") 
#> Bootstrapping with B = 100 replicates...
tidy(fit_lasso_infer_boot) 
#> # A tibble: 11 × 8
#>    term        selected   coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>          <dbl>  <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercept)        1 20.1   20.3     19.3    21.0            1         NA    
#>  2 cyl                1 -1.58  -1.29    -3.66    0.788          0.89       0.788
#>  3 disp               0  0     -0.0466  -1.50    0.788          0.12       0.788
#>  4 hp                 1 -0.801 -0.850   -2.90    0.788          0.83       0.788
#>  5 drat               0  0      0.250   -0.788   2.05           0.4        0.788
#>  6 wt                 1 -2.65  -2.31    -4.52    0.459          0.98       0.788
#>  7 qsec               0  0      0.0500  -0.788   1.48           0.09       0.788
#>  8 vs                 0  0      0.0591  -0.788   1.37           0.2        0.788
#>  9 am                 0  0      0.229   -0.788   1.89           0.45       0.788
#> 10 gear               0  0      0.00571 -0.788   0.788          0.02       0.788
#> 11 carb               0  0     -0.182   -1.80    0.788          0.41       0.788
```

Compare these to the raw percentile intervals (`pad = "none"`), where
coefficients that are rarely selected collapse toward $[0, 0]$:

``` r
set.seed(1)
fit_lasso_infer_boot <- infer_boot(fit_lso, data = mtcars, B = 100, inference_target = "all", pad = "none") 
#> Bootstrapping with B = 100 replicates...
tidy(fit_lasso_infer_boot) 
#> # A tibble: 11 × 7
#>    term        selected   coef estimate ci_low ci_high prop_selected
#>    <chr>          <dbl>  <dbl>    <dbl>  <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1   20.3     19.3    21.0            1   
#>  2 cyl                1 -1.58  -1.29    -2.87    0              0.89
#>  3 disp               0  0     -0.0466  -0.714   0              0.12
#>  4 hp                 1 -0.801 -0.850   -2.11    0              0.83
#>  5 drat               0  0      0.250    0       1.26           0.4 
#>  6 wt                 1 -2.65  -2.31    -3.73   -0.329          0.98
#>  7 qsec               0  0      0.0500   0       0.694          0.09
#>  8 vs                 0  0      0.0591   0       0.584          0.2 
#>  9 am                 0  0      0.229    0       1.10           0.45
#> 10 gear               0  0      0.00571  0       0              0.02
#> 11 carb               0  0     -0.182   -1.01    0              0.41
```

For a more in depth tutorial and examples for different methods, please
consult the package vignette.

#### MCP via ncvreg

``` r
set.seed(12)
fit_mcp <- select_ncvreg(mpg ~ ., data = mtcars) 
fit_mcp 
#> Penalized `ncvreg`-based selector
#> 3 of 10 variables selected: am, qsec, wt 
#> Meta information: family=gaussian, penalty=MCP, lambda=best, lambda_used=0.389, lambda_seq=list(...), cv_info=list(...), ellipses=list(...), lambda_eff=0.339 
#> Default `infer()` method: boot
tidy(fit_mcp)
#> # A tibble: 11 × 3
#>    term        selected  coef
#>    <chr>          <dbl> <dbl>
#>  1 (Intercept)        1 20.1 
#>  2 cyl                0  0   
#>  3 disp               0  0   
#>  4 hp                 0  0   
#>  5 drat               0  0   
#>  6 wt                 1 -3.83
#>  7 qsec               1  2.19
#>  8 vs                 0  0   
#>  9 am                 1  1.46
#> 10 gear               0  0   
#> 11 carb               0  0
```

``` r
fit_mcp_infer_upsi <- infer_upsi(fit_mcp, data = mtcars) 
tidy(fit_mcp_infer_upsi)
#> # A tibble: 11 × 7
#>    term        selected  coef estimate  ci_low ci_high   p_value
#>    <chr>          <dbl> <dbl>    <dbl>   <dbl>   <dbl>     <dbl>
#>  1 (Intercept)        1 20.1     20.1  19.2      21.0   5.53e-28
#>  2 cyl                0  0       NA    NA        NA    NA       
#>  3 disp               0  0       NA    NA        NA    NA       
#>  4 hp                 0  0       NA    NA        NA    NA       
#>  5 drat               0  0       NA    NA        NA    NA       
#>  6 wt                 1 -3.83    -3.83 -5.26     -2.41  6.95e- 6
#>  7 qsec               1  2.19     2.19  1.13      3.25  2.16e- 4
#>  8 vs                 0  0       NA    NA        NA    NA       
#>  9 am                 1  1.46     1.46  0.0228    2.91  4.67e- 2
#> 10 gear               0  0       NA    NA        NA    NA       
#> 11 carb               0  0       NA    NA        NA    NA
```

*Bootstrapping*

MCP is unbiased for large effects, so the default zero-side padding
applies: intervals that contain 0 are padded out to
$\pm\gamma\lambda_{\text{eff}}$, and the others are left alone. Here
$\lambda_{\text{eff}}$ is the smallest penalty giving the selected
model, and the resamples are refit at it (see `?infer_boot`).

``` r
set.seed(12)
fit_mcp_infer_boot <- infer_boot(fit_mcp, data = mtcars, B = 100, inference_target = "all") 
#> Bootstrapping with B = 100 replicates...
tidy(fit_mcp_infer_boot) 
#> # A tibble: 11 × 8
#>    term        selected  coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercept)        1 20.1  20.1      18.9    20.9           1          NA   
#>  2 cyl                0  0    -0.791    -3.45    1.02          0.32        1.02
#>  3 disp               0  0    -0.293    -3.40    1.02          0.09        1.02
#>  4 hp                 0  0    -0.225    -2.91    1.02          0.08        1.02
#>  5 drat               0  0     0.289    -1.02    2.35          0.21        1.02
#>  6 wt                 1 -3.83 -2.75     -5.81    1.02          0.79        1.02
#>  7 qsec               1  2.19  0.948    -1.02    2.91          0.49        1.02
#>  8 vs                 0  0    -0.00186  -1.02    1.47          0.06        1.02
#>  9 am                 1  1.46  0.897    -1.02    3.49          0.48        1.02
#> 10 gear               0  0     0.145    -1.02    2.07          0.09        1.02
#> 11 carb               0  0    -0.986    -3.20    1.02          0.58        1.02
```

``` r
set.seed(12)
fit_mcp_infer_boot <- infer_boot(fit_mcp, data = mtcars, B = 100, inference_target = "all", pad = "none") 
#> Bootstrapping with B = 100 replicates...
tidy(fit_mcp_infer_boot) 
#> # A tibble: 11 × 7
#>    term        selected  coef estimate ci_low ci_high prop_selected
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1  20.1      18.9    20.9           1   
#>  2 cyl                0  0    -0.791    -3.45    0             0.32
#>  3 disp               0  0    -0.293    -3.40    0             0.09
#>  4 hp                 0  0    -0.225    -2.91    0             0.08
#>  5 drat               0  0     0.289     0       2.35          0.21
#>  6 wt                 1 -3.83 -2.75     -5.81    0             0.79
#>  7 qsec               1  2.19  0.948     0       2.91          0.49
#>  8 vs                 0  0    -0.00186   0       1.47          0.06
#>  9 am                 1  1.46  0.897     0       3.49          0.48
#> 10 gear               0  0     0.145     0       2.07          0.09
#> 11 carb               0  0    -0.986    -3.20    0             0.58
```

*PIPE*: available for `ncvreg` only

``` r
fit_mcp_infer_pipe <- infer_pipe(fit_mcp, data = mtcars)
tidy(fit_mcp_infer_pipe)
#> # A tibble: 11 × 7
#>    term        selected  coef estimate  ci_low ci_high       p_value
#>    <chr>          <dbl> <dbl>    <dbl>   <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1  NA       NA       NA     NA           
#>  2 cyl                0  0    -0.0765  -2.50     2.35   0.951       
#>  3 disp               0  0     0.111   -2.36     2.59   0.931       
#>  4 hp                 0  0    -0.246   -2.10     1.61   0.798       
#>  5 drat               0  0     0.131   -1.18     1.45   0.852       
#>  6 wt                 1 -3.83 -3.83    -5.17    -2.49   0.0000000209
#>  7 qsec               1  2.19  2.19     1.19     3.18   0.0000155   
#>  8 vs                 0  0    -0.00198 -1.63     1.63   0.998       
#>  9 am                 1  1.46  1.46     0.0211   2.81   0.0342      
#> 10 gear               0  0    -0.0380  -1.39     1.31   0.958       
#> 11 carb               0  0    -0.328   -1.59     0.931  0.625
```
