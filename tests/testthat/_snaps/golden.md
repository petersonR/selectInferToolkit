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
       (Intercept)        1  0.772378  0.740147  0.366193  1.089920           1.0          NA
                x1        1  1.516620  1.503590  1.368670  1.718010           1.0    0.156718
                x2        1 -1.227170 -1.264730 -1.492910 -0.893631           1.0    0.166149
                x3        0  0.000000        NA        NA        NA            NA    0.152439
                x4        0  0.000000        NA        NA        NA            NA    0.155063
             g3mid        0  0.000000        NA        NA        NA            NA    0.181736
              g3hi        1  0.528920  0.554417  0.263027  0.703252           1.0    0.185834
            spmore        1  0.419497  0.446699  0.323659  0.784834           1.0    0.183307
       spmuch.less        1 -0.218718 -0.182621 -0.400741  0.185561           0.6    0.185561
            binyes        1 -0.178665 -0.144707 -0.252015  0.153237           0.7    0.153237

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
             g3mid        1 -0.0430835
              g3hi        1  0.4973340
            spmore        1  0.4258410
       spmuch.less        1 -0.2241520
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
             g3mid        1 -0.0430835 -0.0430835 -0.293171  0.2070040 7.32329e-01
              g3hi        1  0.4973340  0.4973340  0.235115  0.7595530 3.17949e-04
            spmore        1  0.4258410  0.4258410  0.166355  0.6853280 1.63909e-03
       spmuch.less        1 -0.2241520 -0.2241520 -0.487295  0.0389917 9.38274e-02
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
              term selected       coef   estimate     ci_low    ci_high prop_selected
       (Intercept)        1  0.6237050  0.7773220  0.4793380  1.1033600           1.0
                x1        1  1.5256700  1.5061000  1.3605500  1.6761000           1.0
                x2        1 -1.2219200 -1.2625100 -1.5129100 -0.8671580           1.0
                x3        0  0.0000000         NA         NA         NA            NA
                x4        0  0.0000000         NA         NA         NA            NA
             g3mid        1 -0.0430835 -0.0753027 -0.2070520  0.0558765           0.9
              g3hi        1  0.4973340  0.5069570  0.0596566  0.7230370           0.9
            spmore        1  0.4258410  0.4071930  0.2705030  0.6994970           1.0
       spmuch.less        1 -0.2241520 -0.2337890 -0.4021150  0.0222269           1.0
            binyes        0  0.0000000         NA         NA         NA            NA

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
                x1        1  1.4863100
                x2        1 -1.1943800
                x3        1 -0.0940964
                x4        1 -0.0466742
             g3mid        1 -0.0907332
              g3hi        1  0.4647080
            spmore        1  0.4258750
       spmuch.less        1 -0.1932320
            binyes        1 -0.1756130

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
                x1        1  1.3871500
                x2        1 -1.1988800
                x3        1 -0.0924048
                x4        1 -0.0488139
             g3mid        1 -0.1906800
              g3hi        1  0.9766040
            spmore        1  0.8949960
       spmuch.less        1 -0.3966360
            binyes        1 -0.3517830

---

    Code
      cat(label, "\n")
    Output
      predict(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
           pred
       0.276709
       3.473430
       1.472480
       4.051340
       0.375012
       2.915540
       4.528140
       1.040300
       2.437770
       3.477300

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
                x1        1  1.4863100  1.5034800  1.282470  1.7245000 2.82125e-21
                x2        1 -1.1943800 -1.2208500 -1.455170 -0.9865370 7.89978e-16
                x3        1 -0.0940964 -0.1096710 -0.324653  0.1053100 3.12444e-01
                x4        1 -0.0466742 -0.0638407 -0.282522  0.1548410 5.62272e-01
             g3mid        1 -0.0907332 -0.1067120 -0.363011  0.1495860 4.09134e-01
              g3hi        1  0.4647080  0.4831120  0.221034  0.7451900 4.59788e-04
            spmore        1  0.4258750  0.4345930  0.176079  0.6931070 1.29323e-03
       spmuch.less        1 -0.1932320 -0.2025840 -0.464278  0.0591096 1.27110e-01
            binyes        1 -0.1756130 -0.1915700 -0.407677  0.0245368 8.14203e-02

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
                x1        1  1.3871500  1.4031800  1.196910  1.6094500 2.82125e-21
                x2        1 -1.1988800 -1.2254600 -1.460660 -0.9902550 7.89978e-16
                x3        1 -0.0924048 -0.1077000 -0.318817  0.1034170 3.12444e-01
                x4        1 -0.0488139 -0.0667674 -0.295474  0.1619390 5.62272e-01
             g3mid        1 -0.1906800 -0.2242610 -0.762883  0.3143620 4.09134e-01
              g3hi        1  0.9766040  1.0152800  0.464513  1.5660500 4.59788e-04
            spmore        1  0.8949960  0.9133160  0.370037  1.4566000 1.29323e-03
       spmuch.less        1 -0.3966360 -0.4158320 -0.952995  0.1213310 1.27110e-01
            binyes        1 -0.3517830 -0.3837480 -0.816647  0.0491514 8.14203e-02

---

    Code
      cat(label, "\n")
    Output
      infer_boot(B = 10) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low    ci_high prop_selected null_radius
       (Intercept)        1  1.0753900  1.0919000  0.972918  1.2687700           1.0          NA
                x1        1  1.4863100  1.4870600  1.337000  1.6937900           1.0     0.01446
                x2        1 -1.1943800 -1.2394300 -1.480650 -0.8683520           1.0     0.01446
                x3        1 -0.0940964 -0.0846096 -0.238619  0.1090820           0.9     0.01446
                x4        1 -0.0466742 -0.0124933 -0.227781  0.1377380           0.9     0.01446
             g3mid        1 -0.0907332 -0.0961997 -0.284247  0.0370119           0.9     0.01446
              g3hi        1  0.4647080  0.4990650  0.229779  0.6741910           1.0     0.01446
            spmore        1  0.4258750  0.4213000  0.265123  0.7515920           1.0     0.01446
       spmuch.less        1 -0.1932320 -0.2110670 -0.397754  0.0356223           1.0     0.01446
            binyes        1 -0.1756130 -0.1383660 -0.259260  0.0138628           0.9     0.01446

---

    Code
      cat(label, "\n")
    Output
      infer_selective() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef   estimate    ci_low   ci_high     p_value
       (Intercept)        1  1.0753900         NA        NA        NA          NA
                x1        1  1.4863100  1.5034800  1.285560  1.733310 0.000000000
                x2        1 -1.1943800 -1.2208500 -1.451640 -0.988935 0.000000000
                x3        1 -0.0940964 -0.1096710 -0.317840  0.370630 0.349056000
                x4        1 -0.0466742 -0.0638407 -0.258425  0.898333 0.640102000
             g3mid        1 -0.0907332 -0.1067120 -0.350079  0.586088 0.450672000
              g3hi        1  0.4647080  0.4831120  0.221220  0.872812 0.000265801
            spmore        1  0.4258750  0.4345930  0.173247  0.712824 0.000845094
       spmuch.less        1 -0.1932320 -0.2025840 -0.459194  0.175377 0.129994000
            binyes        1 -0.1756130 -0.1915700 -0.405736  0.100605 0.087268900

# golden: glmnet binomial

    Code
      cat(label, "\n")
    Output
      tidy(selector) 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef
       (Intercept)        1  0.1524850
                x1        1  1.3449000
                x2        1 -0.5874700
                x3        1  0.0265834
                x4        1  0.3980650
             g3mid        1 -0.1754870
              g3hi        1  0.3102150
            spmore        1  0.3720470
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
       (Intercept)        1  0.1524850
                x1        1  1.2551700
                x2        1 -0.5896840
                x3        1  0.0261055
                x4        1  0.4163140
             g3mid        1 -0.3687930
              g3hi        1  0.6519310
            spmore        1  0.7818730
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
       -0.986973
        0.840392
       -0.475482
        2.709580
        1.041350
        2.192260
        2.217940
        0.163363
        1.726490
        3.014210

---

    Code
      cat(label, "\n")
    Output
      infer_upsi() 
    Code
      print(x, row.names = FALSE, digits = 6)
    Output
              term selected       coef  estimate      ci_low   ci_high     p_value
       (Intercept)        1  0.1524850  0.154604 -0.44129200  0.766067 6.10946e-01
                x1        1  1.3449000  1.957810  1.16025000  3.007830 2.32559e-05
                x2        1 -0.5874700 -1.078150 -1.94048000 -0.356807 6.93370e-03
                x3        1  0.0265834  0.217132 -0.37076400  0.844411 4.76574e-01
                x4        1  0.3980650  0.738113  0.09521690  1.478320 3.32838e-02
             g3mid        1 -0.1754870 -0.328043 -1.08985000  0.389572 3.77623e-01
              g3hi        1  0.3102150  0.576991 -0.16992600  1.384850 1.40580e-01
            spmore        1  0.3720470  0.589143  0.00199117  1.235370 5.76887e-02
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
       (Intercept)        1  0.1524850  0.1546040 -0.44129200  0.766067 6.10946e-01
                x1        1  1.2551700  1.8271900  1.08284000  2.807160 2.32559e-05
                x2        1 -0.5896840 -1.0822100 -1.94780000 -0.358152 6.93370e-03
                x3        1  0.0261055  0.2132280 -0.36409900  0.829231 4.76574e-01
                x4        1  0.4163140  0.7719510  0.09958200  1.546090 3.32838e-02
             g3mid        1 -0.3687930 -0.6893980 -2.29037000  0.818703 3.77623e-01
              g3hi        1  0.6519310  1.2125700 -0.35710700  2.910330 1.40580e-01
            spmore        1  0.7818730  1.2381100  0.00418452  2.596180 5.76887e-02
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
              term selected       coef  estimate     ci_low   ci_high   p_value
       (Intercept)        1  0.1524850        NA         NA        NA        NA
                x1        1  1.3449000  1.759760  0.0315896  2.418360 0.0230762
                x2        1 -0.5874700 -0.929498 -1.5971400 -0.145195 0.0117496
                x3        1  0.0265834  0.172926 -9.7103000  0.454872 0.8860130
                x4        1  0.3980650  0.633873 -0.4184200  1.190890 0.1030380
             g3mid        1 -0.1754870 -0.279795 -1.0285800  1.974480 0.5155420
              g3hi        1  0.3102150  0.504850 -0.9259590  1.710790 0.2261190
            spmore        1  0.3720470  0.523317 -0.2838140  1.782790 0.0824057
       spmuch.less        0  0.0000000        NA         NA        NA        NA
            binyes        0  0.0000000        NA         NA        NA        NA

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
              term selected       coef   estimate    ci_low    ci_high prop_selected null_radius
       (Intercept)        1  1.0753900  1.0931000  0.977447  1.2727800             1          NA
                x1        1  1.5034800  1.5137200  1.371670  1.7127000             1  0.00379677
                x2        1 -1.2208500 -1.2733000 -1.508050 -0.9154000             1  0.00379677
                x3        1 -0.1096710 -0.0905470 -0.248426  0.1236060             1  0.00379677
                x4        1 -0.0638407 -0.0136587 -0.234385  0.1469160             1  0.00379677
             g3mid        1 -0.1067120 -0.0982457 -0.290601  0.0585470             1  0.00379677
              g3hi        1  0.4831120  0.5273280  0.257611  0.7064580             1  0.00379677
            spmore        1  0.4345930  0.4328640  0.284478  0.7506270             1  0.00379677
       spmuch.less        1 -0.2025840 -0.2187110 -0.395497  0.0447188             1  0.00379677
            binyes        1 -0.1915700 -0.1511510 -0.262486  0.0118950             1  0.00379677

