# golden: full and null models

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  1.0753900
                x1        1  1.5034800
                x2        1 -1.2208500
                x3        1 -0.1096710
                x4        1 -0.0638407
             g3mid        1 -0.1067120
              g3hi        1  0.4831120
            spmore        1  0.4345930
       spmuch.less        1 -0.2025840
            binyes        1 -0.1915700

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  1.0753900
                x1        1  1.4031800
                x2        1 -1.2254600
                x3        1 -0.1077000
                x4        1 -0.0667674
             g3mid        1 -0.2242610
              g3hi        1  1.0152800
            spmore        1  0.9133160
       spmuch.less        1 -0.4158320
            binyes        1 -0.3837480

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
           pred
       0.281824
       3.523210
       1.518020
       4.093430
       0.319260
       2.919350
       4.614980
       1.053570
       2.431290
       3.520600

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  1.0753900  1.0753900  0.868157  1.2826200 9.38999e-16
                x1        1  1.5034800  1.5034800  1.282470  1.7245000 2.82125e-21
                x2        1 -1.2208500 -1.2208500 -1.455170 -0.9865370 7.89978e-16
                x3        1 -0.1096710 -0.1096710 -0.324653  0.1053100 3.12444e-01
                x4        1 -0.0638407 -0.0638407 -0.282522  0.1548410 5.62272e-01
             g3mid        1 -0.1067120 -0.1067120 -0.363011  0.1495860 4.09134e-01
              g3hi        1  0.4831120  0.4831120  0.221034  0.7451900 4.59788e-04
            spmore        1  0.4345930  0.4345930  0.176079  0.6931070 1.29323e-03
       spmuch.less        1 -0.2025840 -0.2025840 -0.464278  0.0591096 1.27110e-01
            binyes        1 -0.1915700 -0.1915700 -0.407677  0.0245368 8.14203e-02

---

    Code
      cat(label, "\n")
    Output
      infer_upsi(uncertain_nulls), unscaled 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  1.0753900  1.0753900  0.868157  1.2826200 9.38999e-16
                x1        1  1.4031800  1.4031800  1.196910  1.6094500 2.82125e-21
                x2        1 -1.2254600 -1.2254600 -1.460660 -0.9902550 7.89978e-16
                x3        1 -0.1077000 -0.1077000 -0.318817  0.1034170 3.12444e-01
                x4        1 -0.0667674 -0.0667674 -0.295474  0.1619390 5.62272e-01
             g3mid        1 -0.2242610 -0.2242610 -0.762883  0.3143620 4.09134e-01
              g3hi        1  1.0152800  1.0152800  0.464513  1.5660500 4.59788e-04
            spmore        1  0.9133160  0.9133160  0.370037  1.4566000 1.29323e-03
       spmuch.less        1 -0.4158320 -0.4158320 -0.952995  0.1213310 1.27110e-01
            binyes        1 -0.3837480 -0.3837480 -0.816647  0.0491514 8.14203e-02

---

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected    coef
       (Intercept)        1 1.07539
                x1        0 0.00000
                x2        0 0.00000
                x3        0 0.00000
                x4        0 0.00000
             g3mid        0 0.00000
              g3hi        0 0.00000
            spmore        0 0.00000
       spmuch.less        0 0.00000
            binyes        0 0.00000

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected    coef
       (Intercept)        1 1.07539
                x1        0 0.00000
                x2        0 0.00000
                x3        0 0.00000
                x4        0 0.00000
             g3mid        0 0.00000
              g3hi        0 0.00000
            spmore        0 0.00000
       spmuch.less        0 0.00000
            binyes        0 0.00000

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
          pred
       1.07539
       1.07539
       1.07539
       1.07539
       1.07539
       1.07539
       1.07539
       1.07539
       1.07539
       1.07539

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected    coef estimate ci_low ci_high     p_value
       (Intercept)        1 1.07539  1.07539 0.6275 1.52328 7.99808e-06
                x1        0 0.00000       NA     NA      NA          NA
                x2        0 0.00000       NA     NA      NA          NA
                x3        0 0.00000       NA     NA      NA          NA
                x4        0 0.00000       NA     NA      NA          NA
             g3mid        0 0.00000       NA     NA      NA          NA
              g3hi        0 0.00000       NA     NA      NA          NA
            spmore        0 0.00000       NA     NA      NA          NA
       spmuch.less        0 0.00000       NA     NA      NA          NA
            binyes        0 0.00000       NA     NA      NA          NA

---

    Code
      cat(label, "\n")
    Output
      infer_upsi(uncertain_nulls), unscaled 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected    coef   estimate    ci_low   ci_high     p_value
       (Intercept)        1 1.07539  1.0753900  0.627500  1.523280 7.99808e-06
                x1        0 0.00000  1.1886100  0.860742  1.516470 3.03026e-10
                x2        0 0.00000 -0.7458360 -1.169060 -0.322609 7.51668e-04
                x3        0 0.00000 -0.1801670 -0.623844  0.263509 4.21296e-01
                x4        0 0.00000 -0.2890340 -0.759026  0.180957 2.24514e-01
             g3mid        0 0.00000 -0.0292442 -0.982666  0.924178 9.51464e-01
              g3hi        0 0.00000  0.2078460 -0.744447  1.160140 6.65111e-01
            spmore        0 0.00000  1.3046400  0.397685  2.211600 5.37480e-03
       spmuch.less        0 0.00000 -0.6341200 -1.554340  0.286102 1.74033e-01
            binyes        0 0.00000 -0.4606850 -1.363550  0.442176 3.12851e-01

# golden: stepwise, per dummy

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef
       (Intercept)        1  0.772378
                x1        1  1.516620
                x2        1 -1.227170
                x3        0  0.000000
                x4        0  0.000000
             g3mid        0  0.000000
              g3hi        1  0.528920
            spmore        1  0.419497
       spmuch.less        1 -0.218718
            binyes        1 -0.178665

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef
       (Intercept)        1  0.772378
                x1        1  1.415430
                x2        1 -1.231800
                x3        0  0.000000
                x4        0  0.000000
             g3mid        0  0.000000
              g3hi        1  1.111550
            spmore        1  0.881592
       spmuch.less        1 -0.448950
            binyes        1 -0.357896

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
            pred
       0.0845048
       3.6123800
       1.4460100
       3.9959600
       0.2301990
       2.9408100
       4.3672700
       1.0082900
       2.5042000
       3.5114000

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef  estimate    ci_low    ci_high     p_value
       (Intercept)        1  0.772378  0.772378  0.279374  1.2653800 2.57129e-03
                x1        1  1.516620  1.516620  1.304390  1.7288500 9.13547e-23
                x2        1 -1.227170 -1.227170 -1.456960 -0.9973890 1.67012e-16
                x3        0  0.000000        NA        NA         NA          NA
                x4        0  0.000000        NA        NA         NA          NA
             g3mid        0  0.000000        NA        NA         NA          NA
              g3hi        1  0.528920  0.528920  0.307291  0.7505480 9.67363e-06
            spmore        1  0.419497  0.419497  0.164772  0.6742220 1.58204e-03
       spmuch.less        1 -0.218718 -0.218718 -0.476763  0.0393270 9.54373e-02
            binyes        1 -0.178665 -0.178665 -0.386230  0.0289006 9.04954e-02

---

    Code
      cat(label, "\n")
    Output
      infer_upsi(uncertain_nulls), unscaled 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  0.772378  0.7723780  0.279374  1.2653800 2.57129e-03
                x1        1  1.415430  1.4154300  1.217360  1.6135000 9.13547e-23
                x2        1 -1.231800 -1.2318000 -1.462450 -1.0011500 1.67012e-16
                x3        0  0.000000 -0.1029570 -0.311699  0.1057850 3.28789e-01
                x4        0  0.000000 -0.0471834 -0.273275  0.1789080 6.78633e-01
             g3mid        0  0.000000 -0.2085650 -0.741214  0.3240850 4.37619e-01
              g3hi        1  1.111550  1.1115500  0.645786  1.5773100 9.67363e-06
            spmore        1  0.881592  0.8815920  0.346275  1.4169100 1.58204e-03
       spmuch.less        1 -0.448950 -0.4489500 -0.978623  0.0807242 9.54373e-02
            binyes        1 -0.357896 -0.3578960 -0.773685  0.0578929 9.04954e-02

---

    Code
      cat(label, "\n")
    Output
      infer_boot(B = 10) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef  estimate    ci_low   ci_high prop_selected null_radius
       (Intercept)        1  0.772378  1.013510  0.653089  1.622290           1.0          NA
                x1        1  1.516620  1.509260  1.280900  1.710890           1.0    0.156718
                x2        1 -1.227170 -1.245600 -1.351550 -1.034640           1.0    0.166149
                x3        0  0.000000        NA        NA        NA            NA    0.152439
                x4        0  0.000000        NA        NA        NA            NA    0.155063
             g3mid        0  0.000000        NA        NA        NA            NA    0.181736
              g3hi        1  0.528920  0.475368  0.286819  0.657126           1.0    0.185834
            spmore        1  0.419497  0.393643  0.212528  0.546859           1.0    0.183307
       spmuch.less        1 -0.218718 -0.217522 -0.429975  0.185561           0.6    0.185561
            binyes        1 -0.178665 -0.241183 -0.342446 -0.174613           1.0    0.153237

# golden: stepwise, factors together

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  0.6237050
                x1        1  1.5256700
                x2        1 -1.2219200
                x3        0  0.0000000
                x4        0  0.0000000
             g3mid        1 -0.0905419
              g3hi        1  1.0451700
            spmore        1  0.8949250
       spmuch.less        1 -0.4601030
            binyes        0  0.0000000

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  0.6237050
                x1        1  1.4238800
                x2        1 -1.2265300
                x3        0  0.0000000
                x4        0  0.0000000
             g3mid        1 -0.0905419
              g3hi        1  1.0451700
            spmore        1  0.8949250
       spmuch.less        1 -0.4601030
            binyes        0  0.0000000

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
            pred
       -0.165871
        3.368740
        1.288280
        4.232080
        0.441452
        3.170100
        4.586040
        1.140300
        2.725910
        3.300270

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  0.6237050  0.6237050  0.105109  1.1423000 1.90937e-02
                x1        1  1.5256700  1.5256700  1.307170  1.7441600 3.15944e-22
                x2        1 -1.2219200 -1.2219200 -1.457290 -0.9865530 5.84855e-16
                x3        0  0.0000000         NA        NA         NA          NA
                x4        0  0.0000000         NA        NA         NA          NA
             g3mid        1 -0.0905419 -0.0905419 -0.616112  0.4350280 7.32329e-01
              g3hi        1  1.0451700  1.0451700  0.494106  1.5962300 3.17949e-04
            spmore        1  0.8949250  0.8949250  0.349602  1.4402500 1.63909e-03
       spmuch.less        1 -0.4601030 -0.4601030 -1.000240  0.0800359 9.38274e-02
            binyes        0  0.0000000         NA        NA         NA          NA

---

    Code
      cat(label, "\n")
    Output
      infer_upsi(uncertain_nulls), unscaled 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  0.6237050  0.6237050  0.105109  1.1423000 1.90937e-02
                x1        1  1.4238800  1.4238800  1.219960  1.6278000 3.15944e-22
                x2        1 -1.2265300 -1.2265300 -1.462790 -0.9902710 5.84855e-16
                x3        0  0.0000000 -0.1158170 -0.327702  0.0960685 2.79510e-01
                x4        0  0.0000000 -0.0549812 -0.286614  0.1766510 6.37519e-01
             g3mid        1 -0.0905419 -0.0905419 -0.616112  0.4350280 7.32329e-01
              g3hi        1  1.0451700  1.0451700  0.494106  1.5962300 3.17949e-04
            spmore        1  0.8949250  0.8949250  0.349602  1.4402500 1.63909e-03
       spmuch.less        1 -0.4601030 -0.4601030 -1.000240  0.0800359 9.38274e-02
            binyes        0  0.0000000 -0.3980910 -0.827544  0.0313618 6.87262e-02

---

    Code
      cat(label, "\n")
    Output
      infer_boot(B = 10) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef  estimate    ci_low    ci_high prop_selected null_radius
       (Intercept)        1  0.6237050  0.846899  0.260434  1.6511000             1          NA
                x1        1  1.5256700  1.545950  1.339020  1.8543900             1          NA
                x2        1 -1.2219200 -1.252570 -1.387020 -1.0878300             1          NA
                x3        0  0.0000000        NA        NA         NA            NA          NA
                x4        0  0.0000000        NA        NA         NA            NA          NA
             g3mid        1 -0.0905419 -0.211593 -0.579519  0.0589871             1          NA
              g3hi        1  1.0451700  0.967569  0.580151  1.4534100             1          NA
            spmore        1  0.8949250  0.926127  0.516896  1.3224900             1          NA
       spmuch.less        1 -0.4601030        NA        NA         NA            NA          NA
            binyes        0  0.0000000        NA        NA         NA            NA          NA

# golden: stepwise binomial

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef
       (Intercept)        1 -0.310460
                x1        1  1.748120
                x2        1 -1.064820
                x3        0  0.000000
                x4        1  0.637427
             g3mid        0  0.000000
              g3hi        1  0.700083
            spmore        0  0.000000
       spmuch.less        0  0.000000
            binyes        0  0.000000

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef
       (Intercept)        1 -0.310460
                x1        1  1.631500
                x2        1 -1.068830
                x3        0  0.000000
                x4        1  0.666649
             g3mid        0  0.000000
              g3hi        1  1.471260
            spmore        0  0.000000
       spmuch.less        0  0.000000
            binyes        0  0.000000

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
            pred
       -0.856644
        1.971340
       -1.205720
        2.473200
        1.231530
        1.923030
        2.347090
        0.699439
        1.457310
        4.660220

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef  estimate     ci_low   ci_high     p_value
       (Intercept)        1 -0.310460 -0.310460 -1.0784500  0.426087 4.11852e-01
                x1        1  1.748120  1.748120  1.0363400  2.649610 1.61124e-05
                x2        1 -1.064820 -1.064820 -1.8929400 -0.365462 5.81136e-03
                x3        0  0.000000        NA         NA        NA          NA
                x4        1  0.637427  0.637427  0.0475096  1.305970 4.38868e-02
             g3mid        0  0.000000        NA         NA        NA          NA
              g3hi        1  0.700083  0.700083  0.0747130  1.397650 3.58395e-02
            spmore        0  0.000000        NA         NA        NA          NA
       spmuch.less        0  0.000000        NA         NA        NA          NA
            binyes        0  0.000000        NA         NA        NA          NA

---

    Code
      cat(label, "\n")
    Output
      infer_upsi(uncertain_nulls), unscaled 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef    estimate     ci_low   ci_high     p_value
       (Intercept)        1 -0.310460 -0.31046000 -1.0784500  0.426087 4.11852e-01
                x1        1  1.631500  1.63150000  0.9672010  2.472840 1.61124e-05
                x2        1 -1.068830 -1.06883000 -1.9000700 -0.366839 5.81136e-03
                x3        0  0.000000  0.25866400 -0.2967840  0.853116 3.70365e-01
                x4        1  0.666649  0.66664900  0.0496876  1.365840 4.38868e-02
             g3mid        0  0.000000 -0.65064600 -2.1768000  0.791965 3.83539e-01
              g3hi        1  1.471260  1.47126000  0.1570130  2.937230 3.58395e-02
            spmore        0  0.000000  1.29110000  0.0670423  2.642280 4.64070e-02
       spmuch.less        0  0.000000 -0.43496100 -1.6552100  0.754850 4.73917e-01
            binyes        0  0.000000  0.00171061 -1.1490100  1.138410 9.97634e-01

# golden: stepwise cp with selective inference

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef
       (Intercept)        1 -0.238328
                x1        1  1.793230
                x2        1  0.795563
                x3        0  0.000000
                x4        0  0.000000
                x5        0  0.000000
                x6        0  0.000000

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef
       (Intercept)        1 -0.238328
                x1        1  1.932360
                x2        1  0.818235
                x3        0  0.000000
                x4        0  0.000000
                x5        0  0.000000
                x6        0  0.000000

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
            pred
       -0.938170
       -0.182690
       -1.924580
        0.410664
        0.850735
        2.489470
        0.537370
       -0.191577
        1.629280
       -0.588211

---

    Code
      cat(label, "\n")
    Output
      infer_selective() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected      coef estimate   ci_low ci_high     p_value
       (Intercept)        1 -0.238328       NA       NA      NA          NA
                x1        1  1.793230 1.793230 1.506890 2.08222 0.00000e+00
                x2        1  0.795563 0.795563 0.502295 1.08359 1.19752e-06
                x3        0  0.000000       NA       NA      NA          NA
                x4        0  0.000000       NA       NA      NA          NA
                x5        0  0.000000       NA       NA      NA          NA
                x6        0  0.000000       NA       NA      NA          NA

# golden: glmnet lasso

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  1.0753900
                x1        1  1.4556900
                x2        1 -1.1471800
                x3        1 -0.0663332
                x4        1 -0.0160738
             g3mid        1 -0.0622492
              g3hi        1  0.4319010
            spmore        1  0.4103350
       spmuch.less        1 -0.1765610
            binyes        1 -0.1471680

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  1.0753900
                x1        1  1.3585700
                x2        1 -1.1515000
                x3        1 -0.0651407
                x4        1 -0.0168107
             g3mid        1 -0.1308200
              g3hi        1  0.9076590
            spmore        1  0.8623380
       spmuch.less        1 -0.3624160
            binyes        1 -0.2948030

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
           pred
       0.267593
       3.384700
       1.391290
       3.976310
       0.474393
       2.908740
       4.373340
       1.016650
       2.449330
       3.400120

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  1.0753900  1.0753900  0.868157  1.2826200 9.38999e-16
                x1        1  1.4556900  1.5034800  1.282470  1.7245000 2.82125e-21
                x2        1 -1.1471800 -1.2208500 -1.455170 -0.9865370 7.89978e-16
                x3        1 -0.0663332 -0.1096710 -0.324653  0.1053100 3.12444e-01
                x4        1 -0.0160738 -0.0638407 -0.282522  0.1548410 5.62272e-01
             g3mid        1 -0.0622492 -0.1067120 -0.363011  0.1495860 4.09134e-01
              g3hi        1  0.4319010  0.4831120  0.221034  0.7451900 4.59788e-04
            spmore        1  0.4103350  0.4345930  0.176079  0.6931070 1.29323e-03
       spmuch.less        1 -0.1765610 -0.2025840 -0.464278  0.0591096 1.27110e-01
            binyes        1 -0.1471680 -0.1915700 -0.407677  0.0245368 8.14203e-02

---

    Code
      cat(label, "\n")
    Output
      infer_upsi(uncertain_nulls), unscaled 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  1.0753900  1.0753900  0.868157  1.2826200 9.38999e-16
                x1        1  1.3585700  1.4031800  1.196910  1.6094500 2.82125e-21
                x2        1 -1.1515000 -1.2254600 -1.460660 -0.9902550 7.89978e-16
                x3        1 -0.0651407 -0.1077000 -0.318817  0.1034170 3.12444e-01
                x4        1 -0.0168107 -0.0667674 -0.295474  0.1619390 5.62272e-01
             g3mid        1 -0.1308200 -0.2242610 -0.762883  0.3143620 4.09134e-01
              g3hi        1  0.9076590  1.0152800  0.464513  1.5660500 4.59788e-04
            spmore        1  0.8623380  0.9133160  0.370037  1.4566000 1.29323e-03
       spmuch.less        1 -0.3624160 -0.4158320 -0.952995  0.1213310 1.27110e-01
            binyes        1 -0.2948030 -0.3837480 -0.816647  0.0491514 8.14203e-02

---

    Code
      cat(label, "\n")
    Output
      infer_boot(B = 10) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high prop_selected null_radius
       (Intercept)        1  1.0753900  1.0460700  0.953187  1.2102000           1.0          NA
                x1        1  1.4556900  1.4612400  1.264700  1.7118100           1.0   0.0402357
                x2        1 -1.1471800 -1.1940900 -1.335890 -1.0523200           1.0   0.0402357
                x3        1 -0.0663332 -0.1226960 -0.262213  0.0402357           0.8   0.0402357
                x4        1 -0.0160738 -0.0290578 -0.163985  0.0763329           0.8   0.0402357
             g3mid        1 -0.0622492 -0.1252590 -0.279892  0.0265094           0.9   0.0402357
              g3hi        1  0.4319010  0.3839470  0.220675  0.5693780           1.0   0.0402357
            spmore        1  0.4103350  0.3754170  0.140153  0.5726120           1.0   0.0402357
       spmuch.less        1 -0.1765610 -0.1974450 -0.441596  0.0712456           0.9   0.0402357
            binyes        1 -0.1471680 -0.2173690 -0.362249 -0.0458072           1.0   0.0402357

---

    Code
      cat(label, "\n")
    Output
      infer_selective() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate     ci_low   ci_high     p_value
       (Intercept)        1  1.0753900         NA         NA        NA          NA
                x1        1  1.4556900  1.5034800  1.2878300  1.948130 9.86613e-21
                x2        1 -1.1471800 -1.2208500 -1.5139700 -0.988935 0.00000e+00
                x3        1 -0.0663332 -0.1096710 -0.3112410  0.553196 4.49339e-01
                x4        1 -0.0160738 -0.0638407 -0.2069630  2.706190 8.45464e-01
             g3mid        1 -0.0622492 -0.1067120 -0.3395900  0.885032 5.56037e-01
              g3hi        1  0.4319010  0.4831120  0.0710584  1.009570 1.28158e-02
            spmore        1  0.4103350  0.4345930  0.1679570  0.720759 1.21444e-03
       spmuch.less        1 -0.1765610 -0.2025840 -0.5368420  0.202153 1.44526e-01
            binyes        1 -0.1471680 -0.1915700 -0.4035250  0.140405 1.12945e-01

# golden: glmnet binomial

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  0.1538210
                x1        1  1.4189800
                x2        1 -0.6483760
                x3        1  0.0522263
                x4        1  0.4400170
             g3mid        1 -0.1940720
              g3hi        1  0.3446310
            spmore        1  0.3989300
       spmuch.less        0  0.0000000
            binyes        0  0.0000000

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  0.1538210
                x1        1  1.3243100
                x2        1 -0.6508190
                x3        1  0.0512875
                x4        1  0.4601890
             g3mid        1 -0.4078500
              g3hi        1  0.7242580
            spmore        1  0.8383700
       spmuch.less        0  0.0000000
            binyes        0  0.0000000

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
            pred
       -1.147090
        0.883539
       -0.443368
        2.854870
        1.065880
        2.344920
        2.348150
        0.178214
        1.878960
        3.204470

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef  estimate      ci_low   ci_high     p_value
       (Intercept)        1  0.1538210  0.154604 -0.44129200  0.766067 6.10946e-01
                x1        1  1.4189800  1.957810  1.16025000  3.007830 2.32559e-05
                x2        1 -0.6483760 -1.078150 -1.94048000 -0.356807 6.93370e-03
                x3        1  0.0522263  0.217132 -0.37076400  0.844411 4.76574e-01
                x4        1  0.4400170  0.738113  0.09521690  1.478320 3.32838e-02
             g3mid        1 -0.1940720 -0.328043 -1.08985000  0.389572 3.77623e-01
              g3hi        1  0.3446310  0.576991 -0.16992600  1.384850 1.40580e-01
            spmore        1  0.3989300  0.589143  0.00199117  1.235370 5.76887e-02
       spmuch.less        0  0.0000000        NA          NA        NA          NA
            binyes        0  0.0000000        NA          NA        NA          NA

---

    Code
      cat(label, "\n")
    Output
      infer_upsi(uncertain_nulls), unscaled 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate      ci_low   ci_high     p_value
       (Intercept)        1  0.1538210  0.1546040 -0.44129200  0.766067 6.10946e-01
                x1        1  1.3243100  1.8271900  1.08284000  2.807160 2.32559e-05
                x2        1 -0.6508190 -1.0822100 -1.94780000 -0.358152 6.93370e-03
                x3        1  0.0512875  0.2132280 -0.36409900  0.829231 4.76574e-01
                x4        1  0.4601890  0.7719510  0.09958200  1.546090 3.32838e-02
             g3mid        1 -0.4078500 -0.6893980 -2.29037000  0.818703 3.77623e-01
              g3hi        1  0.7242580  1.2125700 -0.35710700  2.910330 1.40580e-01
            spmore        1  0.8383700  1.2381100  0.00418452  2.596180 5.76887e-02
       spmuch.less        0  0.0000000  0.4710150 -1.09722000  2.107990 5.58499e-01
            binyes        0  0.0000000  0.0307228 -1.17697000  1.237280 9.59750e-01

---

    Code
      cat(label, "\n")
    Output
      infer_selective() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef  estimate    ci_low   ci_high    p_value
       (Intercept)        1  0.1538210        NA        NA        NA         NA
                x1        1  1.4189800  1.800850  0.722860  2.502770 0.00182785
                x2        1 -0.6483760 -0.961403 -1.604280 -0.182681 0.00933507
                x3        1  0.0522263  0.181901 -5.018500  0.572358 0.79392800
                x4        1  0.4400170  0.655489 -0.176342  1.228420 0.05461700
             g3mid        1 -0.1940720 -0.289609 -1.019620  1.827230 0.49019100
              g3hi        1  0.3446310  0.520266 -0.831219  1.658930 0.20539600
            spmore        1  0.3989300  0.536806 -0.260314  1.273580 0.08138680
       spmuch.less        0  0.0000000        NA        NA        NA         NA
            binyes        0  0.0000000        NA        NA        NA         NA

# golden: ncvreg MCP

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  1.0753900
                x1        1  1.5034800
                x2        1 -1.2208500
                x3        1 -0.1096710
                x4        1 -0.0638407
             g3mid        1 -0.1067120
              g3hi        1  0.4831120
            spmore        1  0.4345930
       spmuch.less        1 -0.2025840
            binyes        1 -0.1915700

---

    Code
      cat(label, "\n")
    Output
      tidy(selector, scale_coef = FALSE) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  1.0753900
                x1        1  1.4031800
                x2        1 -1.2254600
                x3        1 -0.1077000
                x4        1 -0.0667674
             g3mid        1 -0.2242610
              g3hi        1  1.0152800
            spmore        1  0.9133160
       spmuch.less        1 -0.4158320
            binyes        1 -0.3837480

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
           pred
       0.281824
       3.523210
       1.518020
       4.093430
       0.319260
       2.919350
       4.614980
       1.053570
       2.431290
       3.520600

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  1.0753900  1.0753900  0.868157  1.2826200 9.38999e-16
                x1        1  1.5034800  1.5034800  1.282470  1.7245000 2.82125e-21
                x2        1 -1.2208500 -1.2208500 -1.455170 -0.9865370 7.89978e-16
                x3        1 -0.1096710 -0.1096710 -0.324653  0.1053100 3.12444e-01
                x4        1 -0.0638407 -0.0638407 -0.282522  0.1548410 5.62272e-01
             g3mid        1 -0.1067120 -0.1067120 -0.363011  0.1495860 4.09134e-01
              g3hi        1  0.4831120  0.4831120  0.221034  0.7451900 4.59788e-04
            spmore        1  0.4345930  0.4345930  0.176079  0.6931070 1.29323e-03
       spmuch.less        1 -0.2025840 -0.2025840 -0.464278  0.0591096 1.27110e-01
            binyes        1 -0.1915700 -0.1915700 -0.407677  0.0245368 8.14203e-02

---

    Code
      cat(label, "\n")
    Output
      infer_upsi(uncertain_nulls), unscaled 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high     p_value
       (Intercept)        1  1.0753900  1.0753900  0.868157  1.2826200 9.38999e-16
                x1        1  1.4031800  1.4031800  1.196910  1.6094500 2.82125e-21
                x2        1 -1.2254600 -1.2254600 -1.460660 -0.9902550 7.89978e-16
                x3        1 -0.1077000 -0.1077000 -0.318817  0.1034170 3.12444e-01
                x4        1 -0.0667674 -0.0667674 -0.295474  0.1619390 5.62272e-01
             g3mid        1 -0.2242610 -0.2242610 -0.762883  0.3143620 4.09134e-01
              g3hi        1  1.0152800  1.0152800  0.464513  1.5660500 4.59788e-04
            spmore        1  0.9133160  0.9133160  0.370037  1.4566000 1.29323e-03
       spmuch.less        1 -0.4158320 -0.4158320 -0.952995  0.1213310 1.27110e-01
            binyes        1 -0.3837480 -0.3837480 -0.816647  0.0491514 8.14203e-02

---

    Code
      cat(label, "\n")
    Output
      infer_boot(B = 10) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef  estimate    ci_low    ci_high prop_selected null_radius
       (Intercept)        1  1.0753900  1.050120  0.930593  1.2217500             1          NA
                x1        1  1.5034800  1.518140  1.354550  1.7469800             1  0.00577075
                x2        1 -1.2208500 -1.287480 -1.377380 -1.1535000             1  0.00577075
                x3        1 -0.1096710 -0.165957 -0.271147 -0.0281136             1  0.00577075
                x4        1 -0.0638407 -0.060326 -0.176407  0.0691116             1  0.00577075
             g3mid        1 -0.1067120 -0.146558 -0.274958  0.0519343             1  0.00577075
              g3hi        1  0.4831120  0.449974  0.321488  0.6152530             1  0.00577075
            spmore        1  0.4345930  0.400942  0.194635  0.5948010             1  0.00577075
       spmuch.less        1 -0.2025840 -0.223596 -0.440048  0.1130000             1  0.00577075
            binyes        1 -0.1915700 -0.264032 -0.376882 -0.1235770             1  0.00577075

