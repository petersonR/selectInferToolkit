
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
#> Loading required package: broom
#> Warning: package 'broom' was built under R version 4.4.1

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

fit_aic <- step_ic(y = mtcars$mpg, x = X, std = TRUE ) # does not work if x is matrix
#> Error in UseMethod("tbl_vars"): no applicable method for 'tbl_vars' applied to an object of class "c('matrix', 'array', 'double', 'numeric')"

x <- mtcars[, !names(mtcars) %in% "mpg"]
fit_aic_std <- step_ic(y = mtcars$mpg, x = x, std = TRUE) # std works if x is dataframe 

fit_aic_std 
#> Stepwise Model Selection Summary:
#> Direction of Selection:  forward 
#> Penalty used:  AIC 
#> Design Matrix standardized: TRUE
#> 
#> Final Model Coefficients:
#> (Intercept)          wt         cyl          hp 
#>   20.090625   -3.098748   -1.681654   -1.236744
```

``` r
fit_aic <- step_ic(y = mtcars$mpg, x = X) 
tidy(fit_aic)
#> # A tibble: 4 × 2
#>   term        estimate
#>   <chr>          <dbl>
#> 1 (Intercept)  38.8   
#> 2 wt           -3.17  
#> 3 cyl          -0.942 
#> 4 hp           -0.0180
```

AIC selects `wt`, `qsec`, and `am`. But where are the p-values?! This is
where post-selection inference comes in; p-values that do not adjust for
the selective process are not valid (they will be too small!). We refer
to this as a “hybrid” method where selection is performed and ordinary
least squares theory is used for inference.

``` r
fit_aic_hybrid <- infer(fit_aic, method = "hybrid")
tidy(fit_aic_hybrid)
#> # A tibble: 11 × 8
#>    term        estimate std.error statistic   p.value conf.low conf.high   ci_ln
#>    <chr>          <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>   <dbl>
#>  1 (Intercept)  38.8       1.79       21.7   4.80e-19  35.1     42.4      7.32  
#>  2 cyl          -0.942     0.551      -1.71  9.85e- 2  -2.07     0.187    2.26  
#>  3 disp         NA        NA          NA    NA         NA       NA       NA     
#>  4 hp           -0.0180    0.0119     -1.52  1.40e- 1  -0.0424   0.00629  0.0487
#>  5 drat         NA        NA          NA    NA         NA       NA       NA     
#>  6 wt           -3.17      0.741      -4.28  1.99e- 4  -4.68    -1.65     3.03  
#>  7 qsec         NA        NA          NA    NA         NA       NA       NA     
#>  8 vs           NA        NA          NA    NA         NA       NA       NA     
#>  9 am           NA        NA          NA    NA         NA       NA       NA     
#> 10 gear         NA        NA          NA    NA         NA       NA       NA     
#> 11 carb         NA        NA          NA    NA         NA       NA       NA
```

What if we wanted to adjust for the selective process? Here using
`selectiveInference`.

``` r
fit_aic_SI <- infer(fit_aic, method = "selectiveinf")
fit_aic_SI 
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  selectiveinf 
#> Method for handling null:  ignored 
#> Averege confidence interval length  Inf 
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

#### Bootstrapping

What about bootstrapping?

##### Ignored null

\[explain what this is\]

``` r
set.seed(1)
fit_aic_boot <- infer(fit_aic, method = "boot", B = 100) 
fit_aic_boot
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  bootstrap with  100 boostrap samples 
#> Method for handling null:  ignored 
#> Averege confidence interval length  36.95472 
#> Median confidence interval length  7.2475
tidy(fit_aic_boot) 
#> # A tibble: 4 × 7
#>   term        mean_estimate conf.low conf.high   ci_ln prop.select prop.rej
#>   <fct>               <dbl>    <dbl>     <dbl>   <dbl>       <dbl>    <dbl>
#> 1 (Intercept)       15.3    -84.2      49.0    133.           1        0.7 
#> 2 cyl               -0.238   -2.35      2.24     4.58         0.51     0.31
#> 3 hp                -0.0178  -0.0832    0.0235   0.107        0.5      0.34
#> 4 wt                -3.19    -8.23      1.68     9.91         0.88     0.76
```

##### Confident null non-selections

\[Explain what this is\]

``` r
set.seed(1)
fit_aic_boot <- infer(fit_aic, method = "boot", B = 100, nonselection = "confident_nulls") 
fit_aic_boot
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  bootstrap with  100 boostrap samples 
#> Method for handling null:  confident_nulls 
#> Averege confidence interval length  17.37981 
#> Median confidence interval length  7.0565
tidy(fit_aic_boot) 
#> # A tibble: 11 × 7
#>    term        mean_estimate conf.low conf.high    ci_ln prop.select prop.rej
#>    <fct>               <dbl>    <dbl>     <dbl>    <dbl>       <dbl>    <dbl>
#>  1 (Intercept)       15.3    -84.2      49.0    133.            1        0.7 
#>  2 cyl               -0.238   -2.35      2.24     4.58          0.51     0.31
#>  3 disp               0.0059  -0.0321    0.0634   0.0955        0.5      0.3 
#>  4 hp                -0.0178  -0.0832    0.0235   0.107         0.5      0.34
#>  5 drat               1.26     0         7.06     7.06          0.32     0.23
#>  6 wt                -3.19    -8.23      1.68     9.91          0.88     0.76
#>  7 qsec               0.628    0         4.21     4.21          0.41     0.36
#>  8 vs                -0.329   -6.08      3.63     9.70          0.2      0.12
#>  9 am                 1.41    -1.63      6.55     8.18          0.42     0.25
#> 10 gear               0.791   -1.98      7.24     9.22          0.37     0.25
#> 11 carb              -0.459   -3.18      1.71     4.89          0.44     0.33
```

##### uncertain null non-selections

\[Explain what this is\]

``` r
set.seed(1)
fit_aic_boot <- infer(fit_aic, method = "boot", B = 100, nonselection = "uncertain_nulls") 
fit_aic_boot
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  bootstrap with  100 boostrap samples 
#> Method for handling null:  uncertain_nulls 
#> Averege confidence interval length  17.42436 
#> Median confidence interval length  7.4793
tidy(fit_aic_boot) 
#> # A tibble: 11 × 7
#>    term        mean_estimate conf.low conf.high    ci_ln prop.select prop.rej
#>    <fct>               <dbl>    <dbl>     <dbl>    <dbl>       <dbl>    <dbl>
#>  1 (Intercept)       15.3    -84.2      49.0    133.            1        0.7 
#>  2 cyl               -0.237   -2.35      2.24     4.58          0.51     0.31
#>  3 disp               0.0062  -0.0321    0.0634   0.0955        0.5      0.3 
#>  4 hp                -0.0184  -0.0832    0.0235   0.107         0.5      0.34
#>  5 drat               1.36    -0.423     7.06     7.48          0.32     0.23
#>  6 wt                -3.20    -8.23      1.68     9.91          0.88     0.76
#>  7 qsec               0.657   -0.0673    4.21     4.28          0.41     0.36
#>  8 vs                -0.345   -6.08      3.63     9.70          0.2      0.12
#>  9 am                 1.52    -1.63      6.55     8.18          0.42     0.25
#> 10 gear               0.834   -1.98      7.24     9.22          0.37     0.25
#> 11 carb              -0.486   -3.18      1.71     4.89          0.44     0.33
```

In either case, we find the selections no longer significant after
adjusting for the selective inference process. What gives?

``` r
fit_bic <- step_ic(y = mtcars$mpg, x=X, penalty = "BIC")
fit_bic 
#> Stepwise Model Selection Summary:
#> Direction of Selection:  forward 
#> Penalty used:  BIC 
#> Design Matrix standardized: FALSE
#> 
#> Final Model Coefficients:
#> (Intercept)          wt         cyl 
#>   39.686261   -3.190972   -1.507795
tidy(infer(fit_bic)) # should be same as fit_aic_hybrid, same model (I think it shouls be same as fit_bic and it is??)
#> # A tibble: 11 × 8
#>    term        estimate std.error statistic   p.value conf.low conf.high ci_ln
#>    <chr>          <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <dbl>
#>  1 (Intercept)    39.7      1.71      23.1   3.04e-20    36.2     43.2    7.02
#>  2 cyl            -1.51     0.415     -3.64  1.06e- 3    -2.36    -0.660  1.70
#>  3 disp           NA       NA         NA    NA           NA       NA     NA   
#>  4 hp             NA       NA         NA    NA           NA       NA     NA   
#>  5 drat           NA       NA         NA    NA           NA       NA     NA   
#>  6 wt             -3.19     0.757     -4.22  2.22e- 4    -4.74    -1.64   3.10
#>  7 qsec           NA       NA         NA    NA           NA       NA     NA   
#>  8 vs             NA       NA         NA    NA           NA       NA     NA   
#>  9 am             NA       NA         NA    NA           NA       NA     NA   
#> 10 gear           NA       NA         NA    NA           NA       NA     NA   
#> 11 carb           NA       NA         NA    NA           NA       NA     NA
```

Selective inference:

``` r
fit_bic_SI <- infer(fit_bic, method = "selectiveinf")
fit_bic_SI # Much smaller intervals
#> Selection method:  Stepwise    BIC .  Direction:  forward 
#> Inference methohd:  selectiveinf 
#> Method for handling null:  ignored 
#> Averege confidence interval length  14.89557 
#> Median confidence interval length  14.89557

tidy(fit_bic_SI)
#> # A tibble: 11 × 6
#>    term        estimate conf.low conf.high p.value ci_ln
#>    <chr>          <dbl>    <dbl>     <dbl>   <dbl> <dbl>
#>  1 (Intercept)    NA       NA        NA     NA      NA  
#>  2 cyl            -1.51    -6.48      4.95   0.329  11.4
#>  3 disp           NA       NA        NA     NA      NA  
#>  4 hp             NA       NA        NA     NA      NA  
#>  5 drat           NA       NA        NA     NA      NA  
#>  6 wt             -3.19   -12.4       5.95   0.218  18.4
#>  7 qsec           NA       NA        NA     NA      NA  
#>  8 vs             NA       NA        NA     NA      NA  
#>  9 am             NA       NA        NA     NA      NA  
#> 10 gear           NA       NA        NA     NA      NA  
#> 11 carb           NA       NA        NA     NA      NA
```

What about bootstrapping?

``` r
set.seed(1)
fit_bic_boot <- infer(fit_bic, method = "boot", B = 100) 

tidy(fit_bic_boot )
#> # A tibble: 3 × 7
#>   term        mean_estimate conf.low conf.high ci_ln prop.select prop.rej
#>   <fct>               <dbl>    <dbl>     <dbl> <dbl>       <dbl>    <dbl>
#> 1 (Intercept)        24.7     -47.1     48.8   95.9         1        0.85
#> 2 cyl                -0.501    -2.25     0.216  2.47        0.42     0.33
#> 3 wt                 -3.16     -6.83     0.386  7.22        0.87     0.83
```

### `pen_cv`

We can also try selecting the most important factors are by penalized
models. We can fit the lasso with cross validation and selects
coefficets assocaited with `lambda_min` (default) or `lambda.1se`

``` r
set.seed(12)
fit_lso <- pen_cv(y = mtcars$mpg, x = X) # does not work
#> Error in UseMethod("tbl_vars"): no applicable method for 'tbl_vars' applied to an object of class "c('matrix', 'array', 'double', 'numeric')"

fit_lso <- pen_cv(y = mtcars$mpg, x = x) # it works with x as data-frame
fit_lso # nothing selected? 
#> Penalized regression  Model Summary:
#> Design Matrix standardized: TRUE
#> Penalty used:  MCP  and alpha: 1 
#> Coefficient associated with :  lambda.min 
#> 
#> Final Model Non-Zero Coefficients:
#> (Intercept)          wt        qsec          am 
#>   20.090625   -3.832306    2.190508    1.464817
tidy(fit_lso)
#> # A tibble: 11 × 2
#>    term        estimate
#>    <chr>          <dbl>
#>  1 (Intercept)    20.1 
#>  2 cyl             0   
#>  3 disp            0   
#>  4 hp              0   
#>  5 drat            0   
#>  6 wt             -3.83
#>  7 qsec            2.19
#>  8 vs              0   
#>  9 am              1.46
#> 10 gear            0   
#> 11 carb            0
```

As we can see with lasso, we get same three variables at stepwise AIC
(`wt`,`qaec`,`am`), but their coefficients are different. Even without
adjusting for post-selective inference, lasso by itself does not provide
any p-value or CI to do inference. We can again perfom do hybrid method
where selection is performed with LASSO and ordinary least squares
theory is used for inference.

``` r
fit_lasso_hybrid <-infer(fit_lso,method = "hybrid") # does not work
tidy(fit_lasso_hybrid)
#> # A tibble: 11 × 8
#>    term        estimate std.error statistic   p.value conf.low conf.high ci_ln
#>    <chr>          <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <dbl>
#>  1 (Intercept)    20.1      0.435     46.2   5.53e-28  19.2        21.0   1.78
#>  2 cyl            NA       NA         NA    NA         NA          NA    NA   
#>  3 disp           NA       NA         NA    NA         NA          NA    NA   
#>  4 hp             NA       NA         NA    NA         NA          NA    NA   
#>  5 drat           NA       NA         NA    NA         NA          NA    NA   
#>  6 wt             -3.83     0.696     -5.51  6.95e- 6  -5.26       -2.41  2.85
#>  7 qsec            2.19     0.516      4.25  2.16e- 4   1.13        3.25  2.11
#>  8 vs             NA       NA         NA    NA         NA          NA    NA   
#>  9 am              1.46     0.704      2.08  4.67e- 2   0.0228      2.91  2.88
#> 10 gear           NA       NA         NA    NA         NA          NA    NA   
#> 11 carb           NA       NA         NA    NA         NA          NA    NA
```

What if we wanted to adjust for the selective process? Here using
`selectiveInference`.

``` r
fit_lasso_SI <- infer(fit_lso, method = "selectiveinf")
tidy(fit_lasso_SI)
#> # A tibble: 11 × 6
#>    term        estimate p.value conf.low conf.high ci_ln
#>    <chr>          <dbl>   <dbl>    <dbl>     <dbl> <dbl>
#>  1 (Intercept)   NA     NA         NA       NA      NA  
#>  2 cyl           -1.46   0.225     -7.63     3.27   10.9
#>  3 disp          NA     NA         NA       NA      NA  
#>  4 hp            -1.08   0.163    -23.3      4.41   27.7
#>  5 drat          NA     NA         NA       NA      NA  
#>  6 wt            -2.31   0.0122   -14.9     -0.437  14.4
#>  7 qsec          NA     NA         NA       NA      NA  
#>  8 vs            NA     NA         NA       NA      NA  
#>  9 am             1.04   0.734    -15.6      2.30   17.9
#> 10 gear          NA     NA         NA       NA      NA  
#> 11 carb          -0.815  0.846     -2.33    27.8    30.1
```

What about bootstrapping?

``` r
set.seed(12)
fit_lasso_boot <- infer(fit_lso, method = "boot", B = 100) # needs print method
tidy(fit_lasso_boot) # needs tidy method
#> # A tibble: 4 × 6
#>   term        mean_estimate conf.low conf.high ci_ln prop.select
#>   <fct>               <dbl>    <dbl>     <dbl> <dbl>       <dbl>
#> 1 (Intercept)         20.6   20.4        20.7  0.304         1  
#> 2 wt                  -4.06  -5.14       -2.97 2.18          1  
#> 3 qsec                 1.59   0.0794      3.09 3.02          0.5
#> 4 am                   0      0           0    0             0
```
