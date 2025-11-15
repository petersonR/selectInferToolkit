# HERS data example

This dataset contains data from the HERS study used for demonstration
purposes. Missing data (7-8%) has been omitted.

## Usage

``` r
hers
```

## Format

A data frame with `n` rows and `p` variables:

- age:

  age in years

- bmi:

  BMI (kg/m^2)

- dbp:

  diastolic blood pressure.

- diabetes:

  diabetes

- dmpills:

  oral DM medication by self-report

- drinkany:

  any current alcohol consumption

- exercise:

  exercise at least 3 times per week.

- globrat:

  self-reported health

- glucose:

  fasting glucose (mg/dl)

- hdl:

  HDL cholesterol (mg/dl)

- hdl1:

  year 1 HDL cholesterol (mg/dl).

- ht:

  random assignment to hormone therapy

- htnmeds:

  anti-hypertensive use

- insulin:

  insulin use by self-report

- ldl:

  LDL cholesterol (mg/dl)

- medcond:

  other serious conditions by self-report.

- physact:

  comparative physical activity.

- raceth:

  race/ethnicity.

- sbp:

  systolic blood pressure

- smoking:

  current smoker

- statins:

  statin use

- tg:

  triglycerides (mg/dl)

- waist:

  waist (cm).

- weight:

  weight (kg)

- whr:

  waist/hip ratio.

## Source

- hers:

  [insert-url-here](insert-url-here)

## References

## Examples

``` r
data(hers)
head(hers)
#>   hdl1              ht age           raceth smoking drinkany exercise
#> 1   48         placebo  70 African American      no       no       no
#> 2   48         placebo  62 African American      no       no       no
#> 3   66 hormone therapy  69            White      no       no       no
#> 4   57         placebo  64            White     yes      yes       no
#> 5   35         placebo  65            White      no       no       no
#> 6   53 hormone therapy  68 African American      no      yes       no
#>                physact globrat medcond htnmeds statins diabetes dmpills insulin
#> 1     much more active    good      no     yes     yes       no      no      no
#> 2     much less active    good     yes     yes      no       no      no      no
#> 3      about as active    good      no     yes      no      yes      no      no
#> 4     much less active    good     yes     yes      no       no      no      no
#> 5 somewhat less active    good      no      no      no       no      no      no
#> 6      about as active    good      no      no      no       no      no      no
#>   weight   bmi waist   whr glucose   ldl hdl  tg sbp dbp
#> 1   73.8 23.69  96.0 0.932      84 122.4  52  73 138  78
#> 2   70.9 28.62  93.0 0.964     111 241.6  44 107 118  70
#> 3  102.0 42.51 110.2 0.782     114 166.2  57 154 134  78
#> 4   64.4 24.39  87.0 0.877      94 116.2  56 159 152  72
#> 5   57.9 21.90  77.0 0.794     101 150.6  42 107 175  95
#> 6   60.9 29.05  96.0 1.000     116 137.8  52 111 174  98
```
