
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
*reasonably confident null* that shrinks with $n$
($\delta = \gamma\hat\lambda$ for MCP/SCAD, $\sqrt{k}\cdot\mathrm{SE}$
for stepwise IC); intervals that exclude 0 are unchanged. `pad = "all"`
widens every interval by $\delta$ (the valid choice for the lasso), and
`pad = "none"` returns the raw percentile intervals (confident nulls).
The radius is reported in the `null_radius` column.

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
#> Median CI length = 3; mean = 2.9; max = 3.9
tidy(fit_aic_infer_upsi) %>% dplyr::filter(selected == 1)
#> # A tibble: 4 × 7
#>   term        selected  coef estimate ci_low ci_high  p_value
#>   <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>    <dbl>
#> 1 (Intercept)        1 20.1     20.1   19.2   21.0   9.94e-28
#> 2 cyl                1 -1.68    -1.68  -3.61   0.247 9.85e- 2
#> 3 hp                 1 -1.24    -1.24  -2.83   0.359 1.40e- 1
#> 4 wt                 1 -3.10    -3.10  -4.52  -1.68  1.99e- 4
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
#> Median CI length = 7.9; mean = 7.6; max = 10
tidy(fit_aic_infer_boot) 
#> # A tibble: 11 × 8
#>    term        selected  coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercept)        1 20.1    20.2    18.2    22.6           1          NA   
#>  2 cyl                1 -1.68   -0.386  -4.07    5.15          0.5         2.64
#>  3 disp               0  0      NA      NA      NA            NA           3.13
#>  4 hp                 1 -1.24   -0.834  -4.48    2.18          0.43        2.11
#>  5 drat               0  0      NA      NA      NA            NA           1.24
#>  6 wt                 1 -3.10   -3.28   -7.34    2.62          0.91        2.62
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
#> 5 of 10 variables selected: carb, am, hp, cyl, wt 
#> Meta information: family=gaussian, lambda=best, lambda_used=0.543, cv_info=list(...), ellipses=list(...), alpha=1 
#> Default `infer()` method: selective
tidy(fit_lso)
#> # A tibble: 11 × 3
#>    term        selected    coef
#>    <chr>          <dbl>   <dbl>
#>  1 (Intercept)        1 20.1   
#>  2 cyl                1 -1.56  
#>  3 disp               0  0     
#>  4 hp                 1 -0.956 
#>  5 drat               0  0     
#>  6 wt                 1 -2.66  
#>  7 qsec               0  0     
#>  8 vs                 0  0     
#>  9 am                 1  0.169 
#> 10 gear               0  0     
#> 11 carb               1 -0.0934
```

The lasso selects the same 3 variables as stepwise AIC, plus `am` and
`carb` (with small coefficients). The lasso alone does not provide any
p-value or CI to do inference, but UPSIs aside we can again perform a
(questionable) hybrid inferential process where ordinary least squares
theory is used for inference as though we never used the data for
selection.

``` r
fit_lasso_infer_upsi <-infer_upsi(fit_lso, data = mtcars) 
tidy(fit_lasso_infer_upsi)
#> # A tibble: 11 × 7
#>    term        selected    coef estimate ci_low ci_high   p_value
#>    <chr>          <dbl>   <dbl>    <dbl>  <dbl>   <dbl>     <dbl>
#>  1 (Intercept)        1 20.1      20.1   19.2    21.0    2.67e-26
#>  2 cyl                1 -1.56     -1.46  -3.51    0.588  1.74e- 1
#>  3 disp               0  0        NA     NA      NA     NA       
#>  4 hp                 1 -0.956    -1.08  -3.24    1.08   3.37e- 1
#>  5 drat               0  0        NA     NA      NA     NA       
#>  6 wt                 1 -2.66     -2.31  -4.12   -0.500  1.90e- 2
#>  7 qsec               0  0        NA     NA      NA     NA       
#>  8 vs                 0  0        NA     NA      NA     NA       
#>  9 am                 1  0.169     1.04  -0.470   2.54   1.89e- 1
#> 10 gear               0  0        NA     NA      NA     NA       
#> 11 carb               1 -0.0934   -0.815 -2.30    0.666  2.91e- 1
```

These results are close to the UPSI results for stepwise AIC, since the
two selected models share `cyl`, `hp`, and `wt`; `wt` is still seemingly
significant.

*Selective inference*

``` r
fit_lasso_infer_SI <- infer_selective(fit_lso, data = mtcars)
tidy(fit_lasso_infer_SI)
#> # A tibble: 11 × 7
#>    term        selected    coef estimate ci_low ci_high  p_value
#>    <chr>          <dbl>   <dbl>    <dbl>  <dbl>   <dbl>    <dbl>
#>  1 (Intercept)        1 20.1      NA      NA     NA     NA      
#>  2 cyl                1 -1.56     -1.46   -6.90   2.72   0.204  
#>  3 disp               0  0        NA      NA     NA     NA      
#>  4 hp                 1 -0.956    -1.08  -20.6    3.78   0.157  
#>  5 drat               0  0        NA      NA     NA     NA      
#>  6 wt                 1 -2.66     -2.31  -13.3   -0.569  0.00873
#>  7 qsec               0  0        NA      NA     NA     NA      
#>  8 vs                 0  0        NA      NA     NA     NA      
#>  9 am                 1  0.169     1.04  -13.6    2.25   0.710  
#> 10 gear               0  0        NA      NA     NA     NA      
#> 11 carb               1 -0.0934   -0.815  -2.20  24.2    0.834
```

Here we see that after adjusting for the selective process with
selective inference, we are able to assert a significant effect of `wt`
even after the selective process, though its CI is considerably wider
than the UPSI interval.

*Bootstrapping*

The lasso’s bias does not vanish for large effects, so for a lasso
selector the uniformly valid choice is to pad every interval by
$\hat\lambda$:

``` r
set.seed(1)
fit_lasso_infer_boot <- infer_boot(fit_lso, data = mtcars, B = 100, inference_target = "all", pad = "all") 
#> Bootstrapping with B = 100 replicates...
tidy(fit_lasso_infer_boot) 
#> # A tibble: 11 × 8
#>    term       selected    coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>         <dbl>   <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercep…        1 20.1     20.1    19.3    21.0            1         NA    
#>  2 cyl               1 -1.56    -1.15   -3.39    0.543          0.85       0.543
#>  3 disp              0  0       -0.0570 -1.28    0.543          0.08       0.543
#>  4 hp                1 -0.956   -0.901  -2.80    0.543          0.85       0.543
#>  5 drat              0  0        0.278  -0.543   1.87           0.48       0.543
#>  6 wt                1 -2.66    -2.45   -4.46   -0.293          1          0.543
#>  7 qsec              0  0        0.106  -0.543   1.31           0.18       0.543
#>  8 vs                0  0        0.0973 -0.543   1.27           0.28       0.543
#>  9 am                1  0.169    0.320  -0.543   1.98           0.5        0.543
#> 10 gear              0  0        0.0300 -0.543   0.940          0.1        0.543
#> 11 carb              1 -0.0934  -0.286  -1.75    0.543          0.63       0.543
```

Compare these to the raw percentile intervals (`pad = "none"`), where
coefficients that are rarely selected collapse toward $[0, 0]$:

``` r
set.seed(1)
fit_lasso_infer_boot <- infer_boot(fit_lso, data = mtcars, B = 100, inference_target = "all", pad = "none") 
#> Bootstrapping with B = 100 replicates...
tidy(fit_lasso_infer_boot) 
#> # A tibble: 11 × 8
#>    term       selected    coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>         <dbl>   <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercep…        1 20.1     20.1    19.3    21.0            1             NA
#>  2 cyl               1 -1.56    -1.15   -2.85    0              0.85          NA
#>  3 disp              0  0       -0.0570 -0.740   0              0.08          NA
#>  4 hp                1 -0.956   -0.901  -2.26    0              0.85          NA
#>  5 drat              0  0        0.278   0       1.33           0.48          NA
#>  6 wt                1 -2.66    -2.45   -3.92   -0.837          1             NA
#>  7 qsec              0  0        0.106   0       0.769          0.18          NA
#>  8 vs                0  0        0.0973  0       0.729          0.28          NA
#>  9 am                1  0.169    0.320   0       1.44           0.5           NA
#> 10 gear              0  0        0.0300  0       0.397          0.1           NA
#> 11 carb              1 -0.0934  -0.286  -1.21    0              0.63          NA
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
#> Meta information: family=gaussian, penalty=MCP, lambda=best, lambda_used=0.339, lambda_seq=list(...), cv_info=list(...), ellipses=list(...) 
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
#>  1 (Intercept)        1 20.1     20.1  19.2      20.9   5.53e-28
#>  2 cyl                0  0       NA    NA        NA    NA       
#>  3 disp               0  0       NA    NA        NA    NA       
#>  4 hp                 0  0       NA    NA        NA    NA       
#>  5 drat               0  0       NA    NA        NA    NA       
#>  6 wt                 1 -3.83    -3.83 -5.20     -2.47  6.95e- 6
#>  7 qsec               1  2.19     2.19  1.18      3.20  2.16e- 4
#>  8 vs                 0  0       NA    NA        NA    NA       
#>  9 am                 1  1.46     1.46  0.0851    2.84  4.67e- 2
#> 10 gear               0  0       NA    NA        NA    NA       
#> 11 carb               0  0       NA    NA        NA    NA
```

*Bootstrapping*

MCP is unbiased for large effects, so the default zero-side padding
applies: intervals that contain 0 are padded out to
$\pm\gamma\hat\lambda$, and the others are left alone.

``` r
set.seed(12)
fit_mcp_infer_boot <- infer_boot(fit_mcp, data = mtcars, B = 100, inference_target = "all") 
#> Bootstrapping with B = 100 replicates...
tidy(fit_mcp_infer_boot) 
#> # A tibble: 11 × 8
#>    term        selected  coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercept)        1 20.1   20.1     19.2    21.2           1          NA   
#>  2 cyl                0  0     -0.801   -3.42    1.02          0.33        1.02
#>  3 disp               0  0     -0.209   -3.24    1.02          0.07        1.02
#>  4 hp                 0  0     -0.396   -3.98    1.02          0.11        1.02
#>  5 drat               0  0      0.378   -1.02    2.18          0.22        1.02
#>  6 wt                 1 -3.83  -2.83    -5.73    1.02          0.79        1.02
#>  7 qsec               1  2.19   1.01    -1.02    4.11          0.52        1.02
#>  8 vs                 0  0     -0.0886  -2.59    1.02          0.08        1.02
#>  9 am                 1  1.46   0.763   -1.02    3.20          0.38        1.02
#> 10 gear               0  0      0.133   -1.02    1.77          0.14        1.02
#> 11 carb               0  0     -0.710   -3.57    1.02          0.52        1.02
```

``` r
set.seed(12)
fit_mcp_infer_boot <- infer_boot(fit_mcp, data = mtcars, B = 100, inference_target = "all", pad = "none") 
#> Bootstrapping with B = 100 replicates...
tidy(fit_mcp_infer_boot) 
#> # A tibble: 11 × 8
#>    term        selected  coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercept)        1 20.1   20.1     19.2   21.2            1             NA
#>  2 cyl                0  0     -0.801   -3.42   0              0.33          NA
#>  3 disp               0  0     -0.209   -3.24   0              0.07          NA
#>  4 hp                 0  0     -0.396   -3.98   0              0.11          NA
#>  5 drat               0  0      0.378    0      2.18           0.22          NA
#>  6 wt                 1 -3.83  -2.83    -5.73   0              0.79          NA
#>  7 qsec               1  2.19   1.01     0      4.11           0.52          NA
#>  8 vs                 0  0     -0.0886  -2.59   0.752          0.08          NA
#>  9 am                 1  1.46   0.763    0      3.20           0.38          NA
#> 10 gear               0  0      0.133    0      1.77           0.14          NA
#> 11 carb               0  0     -0.710   -3.57   0              0.52          NA
```

*PIPE*: available for `ncvreg` only

``` r
fit_mcp_infer_pipe <- infer_pipe(fit_mcp, data = mtcars)
tidy(fit_mcp_infer_pipe)
#> # A tibble: 11 × 7
#>    term        selected  coef estimate  ci_low ci_high       p_value
#>    <chr>          <dbl> <dbl>    <dbl>   <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1  NA       NA       NA     NA           
#>  2 cyl                0  0    -0.0765  -2.51     2.36   0.951       
#>  3 disp               0  0     0.111   -2.37     2.59   0.931       
#>  4 hp                 0  0    -0.246   -2.11     1.62   0.798       
#>  5 drat               0  0     0.131   -1.20     1.47   0.852       
#>  6 wt                 1 -3.83 -3.83    -5.17    -2.49   0.0000000209
#>  7 qsec               1  2.19  2.19     1.19     3.18   0.0000155   
#>  8 vs                 0  0    -0.00198 -1.65     1.64   0.998       
#>  9 am                 1  1.46  1.46     0.0421   2.81   0.0342      
#> 10 gear               0  0    -0.0379  -1.41     1.33   0.958       
#> 11 carb               0  0    -0.328   -1.61     0.950  0.625
```
