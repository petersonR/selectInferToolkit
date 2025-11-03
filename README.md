
<!-- README.md is generated from README.Rmd. Please edit that file -->

# selectInferToolkit

<!-- badges: start -->

<!-- badges: end -->

The `selectInferToolkit` package aims to addresses a critical challenge
in modern statistical modeling: **conducting valid inference after
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
- **Penalized Regression:** Perform lasso, ridge, and elastic net
  regressions using the powerful `ncvreg` or `glmnet` packages.
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
provide straightforward implementations of three distinct post-selection
inference methods, applicable after you’ve performed model selection
using one of the methods above:

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

`selectInferToolkit` is explicity in its handling of non-selections,
which can be set to be:

- **Ignored:** Treating non-selected variables as irrelevant.
- **Confident (point-mass) nulls:** Assuming these variables have
  exactly zero effect.
- **Uncertain nulls:** Aim to make inference on all variables but
  without the assumption that non-selections certainly have a null
  effect

### Bootstrapping considerations

In bootstrapping, the above methods of handling nonselections are
similar but slightly altered.

We allow the user to specify their `inference_target` as either
`"selections"` or `"all"`, `debias` varies whether the coefficients are
re-fit after selection prior to bootstrapped aggregation, and
`estimation_data` as either `"in-sample"` or `"out-of-sample"` which
facilitates the estimation of the coefficients on the ‘left-out’ data
after the model has been fit within a bootstrapped sample (similar to
sample spliting).

More details on the bootstrapping approach are forthcoming.

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
narrow in on what we think the most important factors are. The `step_ic`
function can do forwards and backwards selection via AIC by default.

### Stepwise selection

``` r
fit_aic <- select_stepwise_ic(mpg ~ ., data = mtcars, penalty = "AIC") 
fit_aic
#> Stepwise IC-based selector
#> 3 of 10 variables selected: hp, cyl, wt 
#> Meta information: family=gaussian, select_factors_together=TRUE, penalty=AIC, direction=forward 
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
fit_aic_infer_boot
#> Bootstrap inference applied post Stepwise IC-based Selection 
#> 3 of 10 variables selected. 
#> Nonselections were considered N/A 
#> Meta information: B=100, inference_target=selections, debias=TRUE, estimation_data=in-sample 
#> Median CI length = 7.8; mean = 7.1; max = 9.7
tidy(fit_aic_infer_boot) 
#> # A tibble: 11 × 7
#>    term        selected  coef estimate ci_low ci_high prop_selected
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1    20.1    18.7    21.7           1   
#>  2 cyl                1 -1.68   -0.357  -4.19    3.99          0.51
#>  3 disp               0  0      NA      NA      NA            NA   
#>  4 hp                 1 -1.24   -1.42   -5.70    1.66          0.5 
#>  5 drat               0  0      NA      NA      NA            NA   
#>  6 wt                 1 -3.10   -3.21   -8.05    1.64          0.88
#>  7 qsec               0  0      NA      NA      NA            NA   
#>  8 vs                 0  0      NA      NA      NA            NA   
#>  9 am                 0  0      NA      NA      NA            NA   
#> 10 gear               0  0      NA      NA      NA            NA   
#> 11 carb               0  0      NA      NA      NA            NA
```

These results are less promising. Here we can also use a wrapper for
`selectiveInference` method:

``` r
fit_aic_SI <- infer_selective(fit_aic, data = mtcars)
fit_aic_SI 
#> Selective inference applied post Stepwise IC-based Selection 
#> 3 of 10 variables selected. 
#> Nonselections were considered ignored 
#> Meta information: none 
#> Median CI length = Inf; mean = Inf; max = Inf
tidy(fit_aic_SI) 
#> # A tibble: 11 × 7
#>    term        selected  coef estimate ci_low ci_high p_value
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>   <dbl>
#>  1 (Intercept)        1 20.1     NA      NA     NA     NA    
#>  2 cyl                1 -1.68     1.68 -Inf     38.0    0.235
#>  3 disp               0  0       NA      NA     NA     NA    
#>  4 hp                 1 -1.24     1.24  -32.2  Inf      0.795
#>  5 drat               0  0       NA      NA     NA     NA    
#>  6 wt                 1 -3.10     3.10  -13.2    7.59   0.224
#>  7 qsec               0  0       NA      NA     NA     NA    
#>  8 vs                 0  0       NA      NA     NA     NA    
#>  9 am                 0  0       NA      NA     NA     NA    
#> 10 gear               0  0       NA      NA     NA     NA    
#> 11 carb               0  0       NA      NA     NA     NA
```

Here we see that after adjusting for the selective process, we are
unable to claim significance of any of these effects (and have quite
large CIs).

Lastly, as stated above for each post-selection inference method, we
provide three approaches to handling non-selections: `ignored` (ignoring
them), `confident_nulls` (treating them as confident (point-mass)
degenerate nulls at zero), and `uncertain_nulls` (treating them as
uncertain). For details about each of theses methods, see the package
vignette.

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
#> Meta information: family=gaussian, lambda=best, lambda_used=0.801, cv_info=list(...), ellipses=list(...) 
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

The lasso also selects the same 3 variables. The lasso alone does not
provide any p-value or CI to do inference, but UPSIs aside we can again
perform a (questionable) hybrid inferential process where ordinary least
squares theory is used for inference as though we never used the data
for selection.

``` r
fit_lasso_infer_upsi <-infer_upsi(fit_lso, data = mtcars) 
tidy(fit_lasso_infer_upsi)
#> # A tibble: 11 × 7
#>    term        selected   coef estimate ci_low ci_high   p_value
#>    <chr>          <dbl>  <dbl>    <dbl>  <dbl>   <dbl>     <dbl>
#>  1 (Intercept)        1 20.1      20.1   19.2   21.0    9.94e-28
#>  2 cyl                1 -1.58     -1.68  -3.61   0.247  9.85e- 2
#>  3 disp               0  0        NA     NA     NA     NA       
#>  4 hp                 1 -0.801    -1.24  -2.83   0.359  1.40e- 1
#>  5 drat               0  0        NA     NA     NA     NA       
#>  6 wt                 1 -2.65     -3.10  -4.52  -1.68   1.99e- 4
#>  7 qsec               0  0        NA     NA     NA     NA       
#>  8 vs                 0  0        NA     NA     NA     NA       
#>  9 am                 0  0        NA     NA     NA     NA       
#> 10 gear               0  0        NA     NA     NA     NA       
#> 11 carb               0  0        NA     NA     NA     NA
```

These results should look familiar: since the model selected by lasso is
the same as that selected by stepwise AIC, the UPSI inference method is
identical; `wt` is still seemingly significant.

*Selective inference*

``` r
fit_lasso_infer_SI <- infer_selective(fit_lso, data = mtcars)
tidy(fit_lasso_infer_SI)
#> # A tibble: 11 × 7
#>    term        selected   coef estimate ci_low ci_high    p_value
#>    <chr>          <dbl>  <dbl>    <dbl>  <dbl>   <dbl>      <dbl>
#>  1 (Intercept)        1 20.1      NA     NA      NA    NA        
#>  2 cyl                1 -1.58     -1.68  -5.41    1.77  0.112    
#>  3 disp               0  0        NA     NA      NA    NA        
#>  4 hp                 1 -0.801    -1.24  -3.48    3.06  0.246    
#>  5 drat               0  0        NA     NA      NA    NA        
#>  6 wt                 1 -2.65     -3.10  -4.95   -1.33  0.0000918
#>  7 qsec               0  0        NA     NA      NA    NA        
#>  8 vs                 0  0        NA     NA      NA    NA        
#>  9 am                 0  0        NA     NA      NA    NA        
#> 10 gear               0  0        NA     NA      NA    NA        
#> 11 carb               0  0        NA     NA      NA    NA
```

Here we see that after adjusting for the selective process with
selective inference, we are able to assert a significant effect of `wt`
even after the selective process. The CI for this term is only slightly
wide than in the full model.

*Bootstrapping + debiasing*

If we debias our bootrapped estimates, we no longer are “confident” that
a non-selection is akin to a coefficient of zero, thereby treating
non-selections as “uncertain nulls”.

This may be the case if we’re interesting in making inferences for all
variables, including those not selected by the prime model using full
data. To obtain the bootstrap distribution of each $\hat{\beta}_j$ we
proceed as follows: For each B iterations, we re sample the data with
replacement, apply the same model selection method to the re sampled
data, and save the coefficients of selected variables. For any variable
not selected in a given bootstrap sample, we regress the model residuals
on the each non-selected variables separately to get the beta estimates
for non-selections. By doing so, we’re trying to explain the residual
variance by variables that were not selected by model selection method.
This process will produces a bootstrap distribution for all $p$
variables, allowing us to calculate CIs by using the quantiles of these
distributions.

``` r
set.seed(1)
fit_lasso_infer_boot <- infer_boot(fit_lso, data = mtcars, B = 100, inference_target = "all", debias = TRUE) 
tidy(fit_lasso_infer_boot) 
#> # A tibble: 11 × 7
#>    term        selected   coef estimate ci_low ci_high prop_selected
#>    <chr>          <dbl>  <dbl>    <dbl>  <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1    20.1    19.3    21.1            1   
#>  2 cyl                1 -1.58   -0.516  -3.10    3.60           0.8 
#>  3 disp               0  0       1.41   -0.621   4.79           0.12
#>  4 hp                 1 -0.801  -1.26   -3.22    1.07           0.9 
#>  5 drat               0  0       0.550  -0.538   2.24           0.4 
#>  6 wt                 1 -2.65   -2.71   -5.52   -0.241          1   
#>  7 qsec               0  0       0.733  -1.12    3.23           0.09
#>  8 vs                 0  0       0.0767 -1.48    2.57           0.16
#>  9 am                 0  0       0.770  -0.667   2.39           0.35
#> 10 gear               0  0       0.306  -1.95    2.95           0.05
#> 11 carb               0  0      -0.851  -2.89    0.827          0.38
```

With this approach, we see that the `wt` has a CI which does not
intersect zero, indicating a significant effect.

Compare these to the results if `debias = FALSE`:

``` r
set.seed(1)
fit_lasso_infer_boot <- infer_boot(fit_lso, data = mtcars, B = 100, inference_target = "all", debias = FALSE) 
tidy(fit_lasso_infer_boot) 
#> # A tibble: 11 × 7
#>    term        selected   coef estimate ci_low ci_high prop_selected
#>    <chr>          <dbl>  <dbl>    <dbl>  <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1    20.1    19.2    20.9            1   
#>  2 cyl                1 -1.58   -1.10   -2.96    0              0.8 
#>  3 disp               0  0      -0.0696 -0.732   0              0.12
#>  4 hp                 1 -0.801  -0.964  -2.31    0              0.9 
#>  5 drat               0  0       0.170   0       1.30           0.4 
#>  6 wt                 1 -2.65   -2.55   -4.23   -0.884          1   
#>  7 qsec               0  0       0.0641  0       0.796          0.09
#>  8 vs                 0  0       0.0843  0       0.973          0.16
#>  9 am                 0  0       0.173   0       1.08           0.35
#> 10 gear               0  0       0.0214  0       0.215          0.05
#> 11 carb               0  0      -0.153  -0.927   0              0.38
```

For a more in depth tutorial and examples for different methods, please
consult the package vignette.

#### MCP via ncvreg

``` r
set.seed(12)
fit_mcp <- select_ncvreg(mpg ~ ., data = mtcars) 
fit_mcp 
#> Penalized `ncvreg`-based selector
#> 2 of 10 variables selected: qsec, wt 
#> Meta information: family=gaussian, penalty=MCP, lambda=best, lambda_used=0.73, lambda_seq=list(...), cv_info=list(...), ellipses=list(...) 
#> Default `infer()` method: pipe
tidy(fit_mcp)
#> # A tibble: 11 × 3
#>    term        selected  coef
#>    <chr>          <dbl> <dbl>
#>  1 (Intercept)        1 20.1 
#>  2 cyl                0  0   
#>  3 disp               0  0   
#>  4 hp                 0  0   
#>  5 drat               0  0   
#>  6 wt                 1 -4.99
#>  7 qsec               1  1.37
#>  8 vs                 0  0   
#>  9 am                 0  0   
#> 10 gear               0  0   
#> 11 carb               0  0
```

``` r
fit_mcp_infer_upsi <- infer_upsi(fit_mcp, data = mtcars) 
tidy(fit_mcp_infer_upsi)
#> # A tibble: 11 × 7
#>    term        selected  coef estimate ci_low ci_high   p_value
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>     <dbl>
#>  1 (Intercept)        1 20.1     20.1  19.2     21.0   4.84e-28
#>  2 cyl                0  0       NA    NA       NA    NA       
#>  3 disp               0  0       NA    NA       NA    NA       
#>  4 hp                 0  0       NA    NA       NA    NA       
#>  5 drat               0  0       NA    NA       NA    NA       
#>  6 wt                 1 -4.99    -4.94 -5.87    -4.01  2.52e-11
#>  7 qsec               1  1.37     1.66  0.732    2.59  1.50e- 3
#>  8 vs                 0  0       NA    NA       NA    NA       
#>  9 am                 0  0       NA    NA       NA    NA       
#> 10 gear               0  0       NA    NA       NA    NA       
#> 11 carb               0  0       NA    NA       NA    NA
```

*Bootstrapping*

``` r
set.seed(12)
fit_mcp_infer_boot <- infer_boot(fit_mcp, data = mtcars, B = 100, inference_target = "all", debias = FALSE) 
tidy(fit_mcp_infer_boot) 
#> # A tibble: 11 × 7
#>    term        selected  coef  estimate ci_low ci_high prop_selected
#>    <chr>          <dbl> <dbl>     <dbl>  <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1  20.1       19.1    21.0           1   
#>  2 cyl                0  0    -1.33      -5.54    0             0.37
#>  3 disp               0  0    -0.397     -4.85    0             0.09
#>  4 hp                 0  0    -0.283     -4.74    0             0.07
#>  5 drat               0  0     0.0157     0       0             0.02
#>  6 wt                 1 -4.99 -3.24      -6.32    0             0.76
#>  7 qsec               1  1.37  0.575      0       2.13          0.45
#>  8 vs                 0  0     0          0       0             0   
#>  9 am                 0  0     0.136      0       1.73          0.12
#> 10 gear               0  0     0.000184   0       0             0.01
#> 11 carb               0  0    -0.169     -1.84    0             0.2
```

``` r
set.seed(12)
fit_mcp_infer_boot <- infer_boot(fit_mcp, data = mtcars, B = 100, inference_target = "all", debias = TRUE) 
tidy(fit_mcp_infer_boot) 
#> # A tibble: 11 × 7
#>    term        selected  coef estimate ci_low ci_high prop_selected
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>
#>  1 (Intercept)        1 20.1   20.1    19.2    21.0            1   
#>  2 cyl                0  0     -1.69   -5.56    1.23           0.37
#>  3 disp               0  0     -0.0320 -4.37    2.52           0.09
#>  4 hp                 0  0     -1.16   -4.62    0.778          0.07
#>  5 drat               0  0      0.521  -0.891   1.92           0.02
#>  6 wt                 1 -4.99  -3.56   -6.28   -0.543          0.76
#>  7 qsec               1  1.37   0.912  -1.02    2.24           0.45
#>  8 vs                 0  0     -0.0734 -2.02    1.23           0   
#>  9 am                 0  0      0.915  -0.605   2.46           0.12
#> 10 gear               0  0      0.351  -1.12    2.52           0.01
#> 11 carb               0  0     -0.706  -2.19    0.385          0.2
```

*PIPE*: available for `ncvreg` only

``` r
fit_mcp_infer_pipe <- infer_pipe(fit_mcp, data = mtcars)
tidy(fit_mcp_infer_pipe)
#> # A tibble: 11 × 7
#>    term        selected  coef estimate ci_low ci_high   p_value
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>     <dbl>
#>  1 (Intercept)        1 20.1   NA      NA      NA     NA       
#>  2 cyl                0  0     -0.423  -2.44    1.60   0.701   
#>  3 disp               0  0     -0.0843 -2.46    2.29   0.947   
#>  4 hp                 0  0     -0.423  -2.26    1.43   0.679   
#>  5 drat               0  0      0.425  -0.706   1.39   0.518   
#>  6 wt                 1 -4.99  -4.99   -5.91   -4.07   0       
#>  7 qsec               1  1.37   1.65    0.284   2.45   0.000426
#>  8 vs                 0  0      0.216  -1.37    1.77   0.811   
#>  9 am                 0  0      0.473  -0.816   1.63   0.520   
#> 10 gear               0  0      0.274  -0.731   1.15   0.658   
#> 11 carb               0  0     -0.290  -1.30    0.828  0.667
```
