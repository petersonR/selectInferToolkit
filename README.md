
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

#### Bootstrapping

Another option often considered in for quantifying uncertainty of
post-selection estimators is bootstrapping. When bootstrapping the data
and performing model selection, we have to decide how to handle the case
when, for a particular bootstrap sample, some of the variables are not
selected. We consider three possible decisions: ignoring non-selections,
treating them as confident nulls, or treating them as uncertain nulls.
We describe each approach in the following subsections.

##### Ignored non-selections

In this case, we first perform the variable selection using whole data
set and prefer model selection method using either `step_ic()` or
`pen_cv()` functions. Then for inference, we only focus on variables
that are selected on whole data when bootstrapping.

To obtain the bootstrap distribution of each $\hat{\beta}_j$ selected by
our initial model selection procedure, (which we dub our “prime” model),
we proceed as follows: For each B iterations, we resample the data with
replacement, apply the same model selection method to the resampled
data, and save the coefficients *for variables that were selected in
first step*. If a variable in the prime model is not selected in the
current bootstrap model, we set its coefficient to zero. This process
provides a bootstrap distribution for each variable that was selected in
full dataset, allowing us to calculate confidence intervals based on the
quantiles of these distributions.

``` r
set.seed(1)
fit_aic_boot <- infer(fit_aic, method = "boot", B = 100) 
fit_aic_boot
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  bootstrap with  100 bootstrap samples 
#> Method for handling null:  ignored 
#> Average confidence interval length:  4.867233 
#> Median confidence interval length:  4.5834
tidy(fit_aic_boot) 
#> # A tibble: 11 × 6
#>    term        mean_estimate conf.low conf.high   ci_ln prop.select
#>    <chr>               <dbl>    <dbl>     <dbl>   <dbl>       <dbl>
#>  1 (Intercept)       15.2    -84.2      49.0    133.           1   
#>  2 cyl               -0.222   -2.35      2.24     4.58         0.51
#>  3 disp              NA       NA        NA       NA           NA   
#>  4 hp                -0.0178  -0.0832    0.0235   0.107        0.5 
#>  5 drat              NA       NA        NA       NA           NA   
#>  6 wt                -3.19    -8.23      1.68     9.91         0.88
#>  7 qsec              NA       NA        NA       NA           NA   
#>  8 vs                NA       NA        NA       NA           NA   
#>  9 am                NA       NA        NA       NA           NA   
#> 10 gear              NA       NA        NA       NA           NA   
#> 11 carb              NA       NA        NA       NA           NA
```

Similar to the selective inference result, none of our 3 effects remain
significant after adjusting for uncertainty in the model selection
process. However, our intervals are finite, which is an improvement.

##### Confident null non-selections

In this setting, we are interested in making inferences for all
variables, including those not selected by the prime model using full
data (i.e., using `step_ic()` or `pen_cv()` on full dataset). Compared
to the previous approach, the primary difference here is that, for each
bootstrap iteration, after applying the same model selection method to a
bootstrap sample, we retain the coefficients for *all variables,
regardless of whether they were included in the prime model or not*. For
any variable not selected in a given bootstrap sample, its coefficient
is set to zero. This process produces a bootstrap distribution for all
$p$ variables, allowing us to calculate CIs by using the quantiles of
these distributions.

``` r
set.seed(1)
fit_aic_boot <- infer(fit_aic, method = "boot", B = 100, nonselection = "confident_nulls") 
fit_aic_boot
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  bootstrap with  100 bootstrap samples 
#> Method for handling null:  confident_nulls 
#> Average confidence interval length:  5.79607 
#> Median confidence interval length:  5.9727
tidy(fit_aic_boot) 
#> # A tibble: 11 × 6
#>    term        mean_estimate conf.low conf.high    ci_ln prop.select
#>    <fct>               <dbl>    <dbl>     <dbl>    <dbl>       <dbl>
#>  1 (Intercept)       15.2    -84.2      49.0    133.            1   
#>  2 cyl               -0.222   -2.35      2.24     4.58          0.51
#>  3 disp               0.0059  -0.0321    0.0634   0.0955        0.5 
#>  4 hp                -0.0178  -0.0832    0.0235   0.107         0.5 
#>  5 drat               1.26     0         7.06     7.06          0.32
#>  6 wt                -3.19    -8.23      1.68     9.91          0.88
#>  7 qsec               0.628    0         4.21     4.21          0.41
#>  8 vs                -0.299   -6.08      3.63     9.70          0.19
#>  9 am                 1.44    -1.63      6.55     8.18          0.43
#> 10 gear               0.791   -1.98      7.24     9.22          0.37
#> 11 carb              -0.459   -3.18      1.71     4.89          0.44
```

Here again we notice that among all the variables, only `wt` and `cycl`
variable is selected in more than 50% of the bootstrap samples. However,
here again none of our effects remain significant after adjusting for
uncertainty in the model selection process.

##### Uncertain null non-selections

In this setting, we are also interested in making inferences for all
variables, including those not selected by the prime model using full
data (i.e., using step_ic() or pen_cv() on full data set). Compared to
the previous approach of treating non-selections as `confident null`,
the primary difference here is that, for each bootstrap iteration, in
order to get coefficient for all variables, we also get the coefficient
for non-selections instead of setting them to zero. That is after
applying the same selection method to a bootstrap sample, we retain the
coefficients for all variables, regardless of whether they were included
in the prime model or not. But for any variable not selected in a given
bootstrap sample, we regress the model residuals on the each
non-selected variables separately to get the beta estimates for
non-selections. By doing so, we’re trying to explain the residual
variance by variables that were not selected by model selection method.
This process will again produces a bootstrap distribution for all $p$
variables, allowing us to calculate CIs by using the quantiles of these
distributions.

``` r
set.seed(1)
fit_aic_boot <- infer(fit_aic, method = "boot", B = 100, nonselection = "uncertain_nulls") 
fit_aic_boot
#> Selection method:  Stepwise    AIC .  Direction:  forward 
#> Inference methohd:  bootstrap with  100 bootstrap samples 
#> Method for handling null:  uncertain_nulls 
#> Average confidence interval length:  5.84508 
#> Median confidence interval length:  6.1841
tidy(fit_aic_boot) 
#> # A tibble: 11 × 6
#>    term        mean_estimate conf.low conf.high    ci_ln prop.select
#>    <fct>               <dbl>    <dbl>     <dbl>    <dbl>       <dbl>
#>  1 (Intercept)       15.2    -84.2      49.0    133.            1   
#>  2 cyl               -0.222   -2.35      2.24     4.58          0.51
#>  3 disp               0.0062  -0.0321    0.0634   0.0955        0.5 
#>  4 hp                -0.0184  -0.0832    0.0235   0.107         0.5 
#>  5 drat               1.36    -0.423     7.06     7.48          0.32
#>  6 wt                -3.20    -8.23      1.68     9.91          0.88
#>  7 qsec               0.657   -0.0673    4.21     4.28          0.41
#>  8 vs                -0.314   -6.08      3.63     9.70          0.19
#>  9 am                 1.55    -1.63      6.55     8.18          0.43
#> 10 gear               0.834   -1.98      7.24     9.22          0.37
#> 11 carb              -0.486   -3.18      1.71     4.89          0.44
```

Here again in this data set we see that even though our estimates have
slightly change, out overall conclusion are still the same as none of
our our effects remain significant when handling non-selections this way
either.

### BIC vs AIC

No matter how we look, we find our selections are no longer significant
after adjusting for the selective inference process. What gives? It may
be that AIC is not as conducive to post-selection inference as a more
conservative criterion such as BIC. Let’s investigate.

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
tidy(infer(fit_bic, method = "hybrid"))
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

As expected with BIC, we get fewer variables, specifically BIC does not
select `hp`. And as before, a hybrid method ignoring non-selections
finds that both `cyl` and `wt` are significant as shown above.

*Selective inference*

``` r
fit_bic_SI <- infer(fit_bic, method = "selectiveinf")
fit_bic_SI
#> Selection method:  Stepwise    BIC .  Direction:  forward 
#> Inference methohd:  selectiveinf 
#> Method for handling null:  ignored 
#> Average confidence interval length  14.89557 
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

Selective inference gives wider confidence interval and once selection
procedure is taken into account, both selected variables are no longer
significant.

*Bootstrapping*

``` r
set.seed(1)
fit_bic_boot <- infer(fit_bic, method = "boot", B = 100) 

tidy(fit_bic_boot)
#> # A tibble: 3 × 6
#>   term        mean_estimate conf.low conf.high ci_ln prop.select
#>   <fct>               <dbl>    <dbl>     <dbl> <dbl>       <dbl>
#> 1 (Intercept)        24.6     -47.1     48.8   95.9         1   
#> 2 cyl                -0.486    -2.25     0.216  2.47        0.42
#> 3 wt                 -3.16     -6.83     0.386  7.22        0.87
```

Again, with bootstrap with see that while `cyl` is only selected 42% of
the times compared to `wt` which is selected 87% of the time. However
based on bootstrapped CI, neither of them are significant.

We find that AIC nor BIC are able to recover any significant effects on
their own, regardless of the method used for post-selection inference.

### ICs vs `pen_cv`

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
#> # A tibble: 11 × 8
#>    term        estimate std.error statistic   p.value conf.low conf.high ci_ln
#>    <chr>          <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <dbl>
#>  1 (Intercept)    20.1      0.444     45.3   9.94e-28    19.2     21.0    1.82
#>  2 cyl            -1.68     0.984     -1.71  9.85e- 2    -3.70     0.334  4.03
#>  3 disp           NA       NA         NA    NA           NA       NA     NA   
#>  4 hp             -1.24     0.814     -1.52  1.40e- 1    -2.90     0.431  3.34
#>  5 drat           NA       NA         NA    NA           NA       NA     NA   
#>  6 wt             -3.10     0.725     -4.28  1.99e- 4    -4.58    -1.61   2.97
#>  7 qsec           NA       NA         NA    NA           NA       NA     NA   
#>  8 vs             NA       NA         NA    NA           NA       NA     NA   
#>  9 am             NA       NA         NA    NA           NA       NA     NA   
#> 10 gear           NA       NA         NA    NA           NA       NA     NA   
#> 11 carb           NA       NA         NA    NA           NA       NA     NA
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
#>    term        estimate   p.value conf.low conf.high ci_ln
#>    <chr>          <dbl>     <dbl>    <dbl>     <dbl> <dbl>
#>  1 (Intercept)    NA    NA           NA        NA    NA   
#>  2 cyl            -1.68  0.137       -5.24      1.63  6.87
#>  3 disp           NA    NA           NA        NA    NA   
#>  4 hp             -1.24  0.281       -3.34      2.87  6.21
#>  5 drat           NA    NA           NA        NA    NA   
#>  6 wt             -3.10  0.000273    -4.87     -1.45  3.42
#>  7 qsec           NA    NA           NA        NA    NA   
#>  8 vs             NA    NA           NA        NA    NA   
#>  9 am             NA    NA           NA        NA    NA   
#> 10 gear           NA    NA           NA        NA    NA   
#> 11 carb           NA    NA           NA        NA    NA
```

Here again we see that after adjusting for the selective process, we are
only able to claim significance of `wt` but we have larger CIs compared
to hybrid approach.

*Bootstrapping, ignoring non-selections*

``` r
set.seed(12)
fit_lasso_boot <- infer(fit_lso, method = "boot", B = 100)
tidy(fit_lasso_boot)
#> # A tibble: 11 × 6
#>    term        mean_estimate conf.low conf.high   ci_ln prop.select
#>    <chr>               <dbl>    <dbl>     <dbl>   <dbl>       <dbl>
#>  1 (Intercept)         20.4     20.4     20.4    0.0198           1
#>  2 cyl                 -1.36    -2.10    -0.619  1.48             1
#>  3 disp                NA       NA       NA     NA               NA
#>  4 hp                  -1.22    -1.86    -0.576  1.28             1
#>  5 drat                NA       NA       NA     NA               NA
#>  6 wt                  -2.89    -3.34    -2.45   0.890            1
#>  7 qsec                NA       NA       NA     NA               NA
#>  8 vs                  NA       NA       NA     NA               NA
#>  9 am                  NA       NA       NA     NA               NA
#> 10 gear                NA       NA       NA     NA               NA
#> 11 carb                NA       NA       NA     NA               NA
```

With bootstrapping and only focusing on selections from full model, we
see that all three variables are significant. What is we wanted to get
inference on all variables assuming non-selection within each boostrap
might not have true null effect aka `unccertain_nulls` approach
explained above.

*Bootstrapping, uncertain null non-selections*

``` r
set.seed(12)
fit_lasso_boot <- infer(fit_lso, method = "boot", B = 100, 
                        nonselection = "uncertain_nulls") # needs print method
tidy(fit_lasso_boot) # needs tidy method
#> # A tibble: 11 × 6
#>    term        mean_estimate conf.low conf.high ci_ln prop.select
#>    <fct>               <dbl>    <dbl>     <dbl> <dbl>       <dbl>
#>  1 (Intercept)        20.1      19.2     20.8    1.66        1   
#>  2 cyl                 0.383    -5.42     6.17  11.6         0.89
#>  3 disp                3.62     -4.23     7.03  11.3         0.1 
#>  4 hp                  1.03     -5.09     6.91  12.0         0.75
#>  5 drat               -2.70     -6.09     1.45   7.54        0.31
#>  6 wt                  0.206    -3.46     5.91   9.37        0.98
#>  7 qsec               -1.69     -4.42     1.46   5.88        0.04
#>  8 vs                 -2.35     -5.60     2.13   7.73        0.17
#>  9 am                 -2.23     -5.67     2.95   8.62        0.3 
#> 10 gear               -2.49     -5.60     0.435  6.04        0.02
#> 11 carb                1.90     -2.01     7.24   9.25        0.37
```

With the `unccertain_nulls` approach, we see that even though `cycl` and
`wt` are selected 89% and 98% of the times, non of the variables are
significant.

### Choice of Tuining paramter `lambda`

In the above scenario we used the values of parameter `lambda` that
gives minimum mean cross validated error (`lambda.min` which is the
default in `pen_cv` function) to get the beta/coefficient estimates. But
we could also get coefficients associated with `lambda.1se` which gives
coefficients associated with largest value of lambda such that error is
within 1 standard error of the minimum. That is `lambda.1se` provides us
with more sparser model.

``` r
set.seed(13)
fit_lso_1se <- pen_cv(y = mtcars$mpg, x = X, lambda="lambda.1se" ) 
fit_lasso1se_hybrid <-infer(fit_lso_1se, method = "hybrid") 
tidy(fit_lasso1se_hybrid)
#> # A tibble: 11 × 8
#>    term        estimate std.error statistic   p.value conf.low conf.high ci_ln
#>    <chr>          <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <dbl>
#>  1 (Intercept)    20.1      0.444     45.3   9.94e-28    19.2     21.0    1.82
#>  2 cyl            -1.68     0.984     -1.71  9.85e- 2    -3.70     0.334  4.03
#>  3 disp           NA       NA         NA    NA           NA       NA     NA   
#>  4 hp             -1.24     0.814     -1.52  1.40e- 1    -2.90     0.431  3.34
#>  5 drat           NA       NA         NA    NA           NA       NA     NA   
#>  6 wt             -3.10     0.725     -4.28  1.99e- 4    -4.58    -1.61   2.97
#>  7 qsec           NA       NA         NA    NA           NA       NA     NA   
#>  8 vs             NA       NA         NA    NA           NA       NA     NA   
#>  9 am             NA       NA         NA    NA           NA       NA     NA   
#> 10 gear           NA       NA         NA    NA           NA       NA     NA   
#> 11 carb           NA       NA         NA    NA           NA       NA     NA
```

In this example, using `lambda.1se` we only get the same three variables
and only `wt` is significant just as with `lambda.min` and hybrid
apprach .

*Selective inference*

``` r
fit_lasso1se_SI <- infer(fit_lso_1se, method = "selectiveinf")
tidy(fit_lasso1se_SI)
#> # A tibble: 11 × 6
#>    term        estimate   p.value conf.low conf.high ci_ln
#>    <chr>          <dbl>     <dbl>    <dbl>     <dbl> <dbl>
#>  1 (Intercept)    NA    NA           NA        NA    NA   
#>  2 cyl            -1.68  0.130       -7.32      1.75  9.07
#>  3 disp           NA    NA           NA        NA    NA   
#>  4 hp             -1.24  0.440       -3.31      6.47  9.78
#>  5 drat           NA    NA           NA        NA    NA   
#>  6 wt             -3.10  0.000515    -4.92     -1.37  3.54
#>  7 qsec           NA    NA           NA        NA    NA   
#>  8 vs             NA    NA           NA        NA    NA   
#>  9 am             NA    NA           NA        NA    NA   
#> 10 gear           NA    NA           NA        NA    NA   
#> 11 carb           NA    NA           NA        NA    NA
```

Even though our estimates remain same, we see that our confidence
interval length with selective inference are higher.

### collating methods

Lastly, one way to compare the precision of our model
selection/inference method is by looking at the length of confidence
intervals. That can be done with `ciratio` function which returns a list
with three cross tables. One for average CI length across all variables,
second one for median CI length across all variables and lastly number
of significant discoveries across all models.

``` r
set.seed(1234)
precision= ciratio(X, y = mtcars$mpg,B=50, nonselection="ignored")
precision[["avg_ci_ln"]]
#>                Hybrid OLS Bootstrap Selective Inference
#> Full Model       5.878383 8.6984389                  NA
#> Stepwise AIC     3.445128 7.6559333                 Inf
#> Stepwise BIC     3.029397 4.8941000           14.895575
#> Lasso CV (min)   3.445128 0.7346000            5.418803
#> Lasso CV (se)    3.445128 0.3296333            6.377646
#> MCP CV (min)     2.901805 0.9285750                  NA
#> MCP CV (se)      1.937122 1.4597500                  NA
```

We see that in general, the selective inference method gives wider CI
compared to hybrid or bootstrap for given model selection procedure.
Boostrap gives wider CI compared to hybrid method for full model and
stepwise methods but it is more precise on average compared to hybrid
for penalized methods. We can also look at average of medianc CI for
each variable as follow:

``` r
precision[["med_ci_ln"]]
#>                Hybrid OLS Bootstrap Selective Inference
#> Full Model       5.499703  8.696219                  NA
#> Stepwise AIC     3.335912  7.804600                 Inf
#> Stepwise BIC     3.029397  4.894100           14.895575
#> Lasso CV (min)   3.335912  0.622600            6.114553
#> Lasso CV (se)    3.335912  0.296300            7.688638
#> MCP CV (min)     2.918904  0.873350                  NA
#> MCP CV (se)      1.937122  1.459750                  NA
```

While, we see that medican CI lenght for given model is slightly lower
comapred to mean CI length, we see similar trend when comparing
different methods as above.

``` r
precision[["no_sign_disc"]]
#>                Hybrid OLS Bootstrap Selective Inference
#> Full Model              0         0                  NA
#> Stepwise AIC            1         1                   0
#> Stepwise BIC            2         0                   0
#> Lasso CV (min)          1         3                   1
#> Lasso CV (se)           1         3                   1
#> MCP CV (min)            3         4                  NA
#> MCP CV (se)             2         2                  NA
```

We see that in general with bootstrap approach, we have more discovires
compared to hybrid and selective inference.
