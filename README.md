
<!-- README.md is generated from README.Rmd. Please edit that file -->

# practicalPSI

<!-- badges: start -->
<!-- badges: end -->

The goal of practicalPSI is to facilitate post-selection inferential
methods in R in user-friendly ways.

## Installation

You can install the development version of practicalPSI like so:

``` r
devtools::install_github("petersonR/practicalPSI")
```

## Example

Let’s say you are wanting to predict gas mileage based on all variables
in the `mtcars` data set.

``` r
library(practicalPSI)

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
head(X)
#>                   cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             6  225 105 2.76 3.460 20.22  1  0    3    1

fit_aic <- step_ic(y = mtcars$mpg, x = X, std = TRUE ) # does not work
#> Error in UseMethod("tbl_vars"): no applicable method for 'tbl_vars' applied to an object of class "c('matrix', 'array', 'double', 'numeric')"
fit_aic <- step_ic(y = mtcars$mpg, x = X) 
fit_aic
#> Stepwise Model Selection Summary:
#> Direction of Selection:  both 
#> Penalty used:  AIC 
#> Desing Matrix standadrized: FALSE
#> 
#> Final Model Coefficients:
#> (Intercept)          wt        qsec          am 
#>    9.617781   -3.916504    1.225886    2.935837
```

AIC selects `wt`, `qsec`, and `am`. But where are the p-values?! This is
where post-selection inference comes in; p-values that do not adjust for
the selective process are not valid (they will be too small!). We refer
to this as a “hybrid” method where selection is performed and ordinary
least squares theory is used for inference.

``` r
fit_aic_hybrid <- infer(fit_aic, method = "hybrid")
tidy(fit_aic_hybrid)
#> Error in tidy(fit_aic_hybrid): could not find function "tidy"
```

What if we wanted to adjust for the selective process? Here using
`selectiveInference`.

``` r
fit_aic_SI <- infer(fit_aic, method = "selectiveinf")
#> Error in arrange(., term): could not find function "arrange"
fit_aic_SI # both is possible for this? 
#> Error: object 'fit_aic_SI' not found
tidy(fit_aic_SI) # why is the selection different? 
#> Error in tidy(fit_aic_SI): could not find function "tidy"
```

What about bootstrapping?

``` r
set.seed(1)
fit_aic_boot <- infer(fit_aic, method = "boot", B = 100) # needs print method
tidy(fit_aic_boot) # needs tidy method
#> Error in tidy(fit_aic_boot): could not find function "tidy"
fit_aic_boot$model # probably don't report median_p.value, something else instead?
#> # A tibble: 4 × 8
#>   term        mean_estimate conf.low conf.high median_p.value  ci_ln prop.select
#>   <fct>               <dbl>    <dbl>     <dbl>          <dbl>  <dbl>       <dbl>
#> 1 (Intercept)          3.14   -83.7      48.8          0.0253 132.          1   
#> 2 wt                  -3.68    -8.84      1.06         0.0005   9.90        0.88
#> 3 qsec                 1.10     0         4.78         0.0699   4.78        0.61
#> 4 am                   1.66    -2.82      6.52         0.140    9.35        0.57
#> # ℹ 1 more variable: prop.rej <dbl>
```

In either case, we find the selections no longer significant after
adjusting for the selective inference process. What gives?

``` r
fit_bic <- step_ic(y = mtcars$mpg, x=X, penalty = "BIC")
fit_bic 
#> Stepwise Model Selection Summary:
#> Direction of Selection:  both 
#> Penalty used:  BIC 
#> Desing Matrix standadrized: FALSE
#> 
#> Final Model Coefficients:
#> (Intercept)          wt        qsec          am 
#>    9.617781   -3.916504    1.225886    2.935837
tidy(infer(fit_bic)) # should be same as fit_aic_hybrid, same model
#> Error in tidy(infer(fit_bic)): could not find function "tidy"
```

Selective inference:

``` r
fit_bic_SI <- infer(fit_bic, method = "selectiveinf")
#> Error in arrange(., term): could not find function "arrange"
fit_bic_SI # Much smaller intervals
#> Error: object 'fit_bic_SI' not found

tidy(fit_bic_SI)
#> Error in tidy(fit_bic_SI): could not find function "tidy"
```

Whoa! Now we have gear pop out as significant! But wait, “gear” is not
in `fit_bic` as selection. what gives?

What about bootstrapping?

``` r
set.seed(1)
fit_bic_boot <- infer(fit_bic, method = "boot", B = 100) 
fit_bic_boot$model 
#> # A tibble: 4 × 8
#>   term        mean_estimate conf.low conf.high median_p.value  ci_ln prop.select
#>   <fct>               <dbl>    <dbl>     <dbl>          <dbl>  <dbl>       <dbl>
#> 1 (Intercept)         6.77   -75.6       48.5          0.0037 124.          1   
#> 2 wt                 -3.57    -7.72       0            0.0001   7.72        0.84
#> 3 qsec                0.974    0          4.42         0.0746   4.42        0.52
#> 4 am                  1.55    -0.980      6.52         1        7.50        0.42
#> # ℹ 1 more variable: prop.rej <dbl>
```

### `pen_cv`

``` r
set.seed(12)
fit_lso <- pen_cv(y = mtcars$mpg, x = X) # does not work
#> Error in UseMethod("tbl_vars"): no applicable method for 'tbl_vars' applied to an object of class "c('matrix', 'array', 'double', 'numeric')"
fit_lso <- pen_cv(y = mtcars$mpg, x = data.frame(X), std=FALSE) 
fit_lso # nothing selected? 
#> Penalized regression  Model Summary:
#> Desing Matrix standadrized: FALSE
#> Penalty used:  MCP  and alpha: 1 
#> Coefficeint associated with :  lambda.min 
#> 
#> Final Model Non-Zero Coefficients:
#> NULL

infer(fit_lso) # does not work
#> Error in eval(predvars, data, env): object 'y' not found

set.seed(12)
fit_cv <- cv.ncvreg(X, y = mtcars$mpg)
#> Error in cv.ncvreg(X, y = mtcars$mpg): could not find function "cv.ncvreg"
summary(fit_cv) # something not right here
#> Error: object 'fit_cv' not found
```
