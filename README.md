
<!-- README.md is generated from README.Rmd. Please edit that file -->

# practicalPSI

<!-- badges: start -->
<!-- badges: end -->

The `practicalPSI` package aims to addresses a critical challenge in
modern statistical modeling: **conducting valid inference after
performing model selection.** In the era of big data and complex
regression problems, researchers often employ model selection procedures
like stepwise regression or penalized methods (e.g., lasso) to identify
important variables. However, classical statistical inference assumes
the model was chosen *a priori*—independent of the data. When this
assumption is violated, standard statistical tests and confidence
intervals can become overly optimistic, leading to unreliable
conclusions.`practicalPSI` provides a user-friendly framework in R to
facilitate post-selection inferential methods.

## Installation

You can install the development version of practicalPSI like so:

``` r
devtools::install_github("petersonR/practicalPSI")
```

## Pacakge overview

The `practicalPSI` provides two broad classes of methods for model
selection, stepwise regression methods, and penalized regression
methods. The package implements stepwise regression
(forward/backward/bidirectional) with the Akaike Information Criterion
(AIC) and Bayesian Information Criterion (BIC) via `stepAIC` function
from `MASS` package. It also performs penalized regression methods
including lasso, ridge, elastic net using `ncvreg` or `glmnet` pacakges
and easily returns models associated with choice of either
$\lambda_{min}$ or $\lambda_{1se}$. $\lambda_{min}$ is the value of the
regularization parameter $\lambda$ that minimizes the cross-validation
error, offering the best predictive performance without accounting for
model complexity. $\lambda_{1se}$ is a more conservative choice,
selected as the largest $\lambda$ where the cross-validation error is
within one standard error of $\lambda_{min}$, resulting in a
sparser/condensed model.

Main contribution of the package is that it provides easy ways to
implement three different post-selection inference methods after
perfoming model selection using one of the methods above: 1) hybrid
ordinary least squares (no adjustment) 2) bootstrap and, 3) selective
inference (only available for lasso and forward stepwise selection via
`selectiveInference` package). Finally, for each post-selection
inference method, we provide three approaches to handling
non-selections: ignoring them, treating them as confident (point-mass)
nulls, and treating them as uncertain nulls.

## Package Overview

`practicalPSI` offers a comprehensive suite of tools, covering both
model selection and the crucial post-selection inference steps.

### Model Selection Methods

The package supports two broad classes of popular model selection
techniques:

- **Stepwise Regression:** Implement forward, backward, and
  bidirectional stepwise regression. User can use either the **Akaike
  Information Criterion (AIC)** or the **Bayesian Information Criterion
  (BIC)** as selection criteria, leveraging the `stepAIC` function from
  the `MASS` package.
- **Penalized Regression:** Easily perform lasso, ridge, and elastic net
  regressions using the powerful `ncvreg` or `glmnet` packages.
  `practicalPSI` simplifies the process of obtaining models associated
  with either $\lambda_{min}$ or $\lambda_{1se}$.
  - $\lambda_{min}$ represents the regularization parameter $\lambda$
    that minimizes the cross-validation error, aiming for the best
    predictive performance.
  - $\lambda_{1se}$ is a more conservative choice, selecting the largest
    $\lambda$ where the cross-validation error is within one standard
    error of $\lambda_{min}$. This often results in a sparser, more
    parsimonious model.

### Post-Selection Inference Methods

The core contribution of `practicalPSI` lies in its ability to provide
straightforward implementations of three distinct post-selection
inference methods, applicable after you’ve performed model selection
using one of the methods above:

1.  **Hybrid Ordinary Least Squares:** A baseline approach that performs
    no adjustment for the selection process.
2.  **Bootstrap:** A non-parametric bootstrap method that resamples data
    to account for selection uncertainty.
3.  **Selective Inference:** A theoretically grounded approach,
    currently available for lasso and forward stepwise selection through
    integration with the `selectiveInference` package.

### Handling Non-Selected Variables

A unique and important feature of `practicalPSI` is its flexibility in
handling non-selected variables (those not included in the final model).
For each post-selection inference method, we offer three distinct
approaches:

- **Ignoring them:** Treating non-selected variables as irrelevant.
- **Treating them as confident (point-mass) nulls:** Assuming these
  variables have exactly zero effect.
- **Treating them as uncertain nulls:** Aim to make inference on all
  variables but without the assumption that non-selections certainly
  have a null effect

## Example

Let’s say you are wanting to predict gas mileage based on all variables
in the `mtcars` data set.

``` r
library(practicalPSI)
#> Loading required package: broom

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
model is probably over-specified. Let’s use `practicalPSI` to narrow in
on what we think the most important factors are. The `step_ic` function
can do forwards and backwards selection via AIC by default.

### `step_ic`

``` r
X <- model.matrix(fit_full)[,-1]
fit_aic <- step_ic(y = mtcars$mpg, x = X, direction = "forward") 
fit_aic
#> Stepwise Model Selection Summary:
#> Direction of Selection:  forward 
#> Penalty used:  AIC 
#> Design Matrix standardized: FALSE
#> 
#> Final Model Coefficients:
#> (Intercept)          wt         cyl          hp 
#>  38.7517874  -3.1669731  -0.9416168  -0.0180381
```

`step_ic()` function creates a `selector_ic` class which we can pass to
`tidy()` function to get results in `tibble`:

``` r
tidy(fit_aic)
#> # A tibble: 4 × 2
#>   term        estimate
#>   <chr>          <dbl>
#> 1 (Intercept)  38.8   
#> 2 wt           -3.17  
#> 3 cyl          -0.942 
#> 4 hp           -0.0180
```

AIC selects `wt`, `cyl`, and `hp`. But where are the p-values?! This is
where post-selection inference comes in; p-values that do not adjust for
the selective process are not valid (they will be too small!). We refer
to this as a “hybrid” method where selection is performed and ordinary
least squares theory is used for inference.

``` r
fit_aic_hybrid <- infer(fit_aic, method = "hybrid")
fit_aic_hybrid
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  hybrid 
#> Method for handling null:  ignored 
#> Average confidence interval length  1.779886 
#> Median confidence interval length  2.257002
tidy(fit_aic_hybrid)
#> # A tibble: 11 × 7
#>    term        estimate std.error   p.value conf.low conf.high   ci_ln
#>    <chr>          <dbl>     <dbl>     <dbl>    <dbl>     <dbl>   <dbl>
#>  1 (Intercept)  38.8       1.79    4.80e-19  35.1     42.4      7.32  
#>  2 cyl          -0.942     0.551   9.85e- 2  -2.07     0.187    2.26  
#>  3 disp         NA        NA      NA         NA       NA       NA     
#>  4 hp           -0.0180    0.0119  1.40e- 1  -0.0424   0.00629  0.0487
#>  5 drat         NA        NA      NA         NA       NA       NA     
#>  6 wt           -3.17      0.741   1.99e- 4  -4.68    -1.65     3.03  
#>  7 qsec         NA        NA      NA         NA       NA       NA     
#>  8 vs           NA        NA      NA         NA       NA       NA     
#>  9 am           NA        NA      NA         NA       NA       NA     
#> 10 gear         NA        NA      NA         NA       NA       NA     
#> 11 carb         NA        NA      NA         NA       NA       NA
```

Thus, `cyl` and `wt` are highly significant while `hp` has a p-value of
0.14. We know these are not valid, as we’ve used the data both to select
the model and now are using the same data to perform inference. What if
we wanted to adjust our uncertainty quantification for the selective
process?

Here we can just change `method` option in `infer()` function to using
`selectiveinf` to use `selectiveInference` method.

``` r
fit_aic_SI <- infer(fit_aic, method = "selectiveinf")
fit_aic_SI 
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  selectiveinf 
#> Method for handling null:  ignored 
#> Average confidence interval length  Inf 
#> Median confidence interval length  Inf
tidy(fit_aic_SI) 
#> # A tibble: 11 × 6
#>    term        estimate conf.low conf.high p.value ci_ln
#>    <chr>          <dbl>    <dbl>     <dbl>   <dbl> <dbl>
#>  1 (Intercept)  NA        NA         NA     NA      NA  
#>  2 cyl          -0.942  -Inf         17.0    0.235 Inf  
#>  3 disp         NA        NA         NA     NA      NA  
#>  4 hp           -0.0180   -0.372    Inf      0.795 Inf  
#>  5 drat         NA        NA         NA     NA      NA  
#>  6 wt           -3.17    -11.8        6.00   0.224  17.8
#>  7 qsec         NA        NA         NA     NA      NA  
#>  8 vs           NA        NA         NA     NA      NA  
#>  9 am           NA        NA         NA     NA      NA  
#> 10 gear         NA        NA         NA     NA      NA  
#> 11 carb         NA        NA         NA     NA      NA
```

Here we see that after adjusting for the selective process, we are
unable to claim significance of any of these effects (and have quite
large CIs).

With the step wise methods, another option available in package when
making inference post-selections is bootstrap
`r{eval =F, echo=T} infer (fit_aic, method="boot", B=100)`. Lastly, as
stated above for each post-selection inference method, we provide three
approaches to handling non-selections: `ignored`(ignoring them),
`confident_nulls`- treating them as confident (point-mass) nulls, and
`uncertain_nulls` treating them as uncertain nulls. For details about
each of theses methods see package vignette.

### `step_ic` vs `pen_cv`

We can also try selecting the most important factors are by penalized
models. We can fit the lasso with cross validation and selects
coefficients associated with `lambda_min` (default) or `lambda.1se`.

``` r
set.seed(12)
fit_lso <- pen_cv(y = mtcars$mpg, x = X,penalty= "lasso",lambda="lambda.min") 
fit_lso 
#> Penalized regression  Model Summary:
#> Design Matrix standardized: TRUE
#> Penalty used:  lasso  and alpha: 1 
#> Coefficient associated with :  lambda.min 
#> 
#> Final Model Non-Zero Coefficients:
#> (Intercept)         cyl          hp          wt 
#>  20.0906250  -1.5824791  -0.8011145  -2.6498066
tidy(fit_lso)
#> # A tibble: 11 × 2
#>    term        estimate
#>    <chr>          <dbl>
#>  1 (Intercept)   20.1  
#>  2 cyl           -1.58 
#>  3 disp           0    
#>  4 hp            -0.801
#>  5 drat           0    
#>  6 wt            -2.65 
#>  7 qsec           0    
#>  8 vs             0    
#>  9 am             0    
#> 10 gear           0    
#> 11 carb           0
```

The lasso also selects the same 3 variables. The lasso alone does not
provide any p-value or CI to do inference, but we can again perform a
(questionable) hybrid inferential process where ordinary least squares
theory is used for inference as though we never used the data for
selection.

``` r
fit_lasso_hybrid <-infer(fit_lso, method = "hybrid") 
tidy(fit_lasso_hybrid)
#> # A tibble: 11 × 7
#>    term        estimate std.error   p.value conf.low conf.high ci_ln
#>    <chr>          <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <dbl>
#>  1 (Intercept)    20.1      0.444  9.94e-28    19.2     21.0    1.82
#>  2 cyl            -1.68     0.984  9.85e- 2    -3.70     0.334  4.03
#>  3 disp           NA       NA     NA           NA       NA     NA   
#>  4 hp             -1.24     0.814  1.40e- 1    -2.90     0.431  3.34
#>  5 drat           NA       NA     NA           NA       NA     NA   
#>  6 wt             -3.10     0.725  1.99e- 4    -4.58    -1.61   2.97
#>  7 qsec           NA       NA     NA           NA       NA     NA   
#>  8 vs             NA       NA     NA           NA       NA     NA   
#>  9 am             NA       NA     NA           NA       NA     NA   
#> 10 gear           NA       NA     NA           NA       NA     NA   
#> 11 carb           NA       NA     NA           NA       NA     NA
```

These results indicate that for the model selected by lasso, while
`cycl` and `hp` are significant, `wt` is still not significant. Let’s
see if we can confirm this sentiment after adjusting for the selective
process.

*Selective inference*

``` r
fit_lasso_SI <- infer(fit_lso, method = "selectiveinf")
tidy(fit_lasso_SI)
#> # A tibble: 11 × 6
#>    term        estimate conf.low conf.high   p.value ci_ln
#>    <chr>          <dbl>    <dbl>     <dbl>     <dbl> <dbl>
#>  1 (Intercept)    NA       NA        NA    NA        NA   
#>  2 cyl            -1.68    -5.24      1.63  0.137     6.87
#>  3 disp           NA       NA        NA    NA        NA   
#>  4 hp             -1.24    -3.34      2.87  0.281     6.21
#>  5 drat           NA       NA        NA    NA        NA   
#>  6 wt             -3.10    -4.87     -1.45  0.000273  3.42
#>  7 qsec           NA       NA        NA    NA        NA   
#>  8 vs             NA       NA        NA    NA        NA   
#>  9 am             NA       NA        NA    NA        NA   
#> 10 gear           NA       NA        NA    NA        NA   
#> 11 carb           NA       NA        NA    NA        NA
```

Here again we see that after adjusting for the selective process, we are
only able to claim significance of `wt` but we have larger CIs compared
to hybrid approach.

*Bootstrapping, uncertain null non-selections*

With treating non-selection as uncertain nulls, we’re interesting in
making inferences for all variables, including those not selected by the
prime model using full data (i.e., using `step_ic()` or `pen_cv()` on
full data set). To obtain the bootstrap distribution of each
$\hat{\beta}_j$ we proceed as follows: For each B iterations, we re
sample the data with replacement, apply the same model selection method
to the re sampled data, and save the coefficients of selected variables.
For any variable not selected in a given bootstrap sample, we regress
the model residuals on the each non-selected variables separately to get
the beta estimates for non-selections. By doing so, we’re trying to
explain the residual variance by variables that were not selected by
model selection method. This process will produces a bootstrap
distribution for all $p$ variables, allowing us to calculate CIs by
using the quantiles of these distributions.

``` r
set.seed(12)
fit_lasso_boot <- infer(fit_lso, method = "boot", B = 100, 
                        nonselection = "uncertain_nulls") 
tidy(fit_lasso_boot) 
#> # A tibble: 11 × 6
#>    term        mean_estimate conf.low conf.high ci_ln prop.select
#>    <chr>               <dbl>    <dbl>     <dbl> <dbl>       <dbl>
#>  1 (Intercept)        20.1   19.3       21.1    1.83       1     
#>  2 cyl                -1.36  -2.77      -0.192  2.58       0.89  
#>  3 disp               -0.734 -1.12      -0.231  0.888      0.1   
#>  4 hp                 -0.989 -2.13      -0.141  1.99       0.75  
#>  5 drat                0.660  0.0921     1.26   1.16       0.32  
#>  6 wt                 -2.41  -3.47      -0.994  2.48       0.98  
#>  7 qsec                0.610  0.196      1.24   1.05       0.04  
#>  8 vs                  0.591  0.0451     0.865  0.820      0.19  
#>  9 am                  0.563  0.0534     0.949  0.896      0.3   
#> 10 gear                0.448 -0.00420    0.938  0.942      0.0200
#> 11 carb               -0.673 -1.37      -0.0343 1.34       0.36
```

With the `unccertain_nulls` approach, we see that even though `cycl` and
`wt` are selected 89% and 98% of the times, non of the variables are
significant.

For a more in depth tutorial and examples for different methods, please
consult the package vignette.
