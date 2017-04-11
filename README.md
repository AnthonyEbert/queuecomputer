
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/queuecomputer)](https://CRAN.R-project.org/package=queuecomputer) [![Downloads](http://cranlogs.r-pkg.org/badges/queuecomputer)](https://CRAN.R-project.org/package=queuecomputer) [![Build Status](https://travis-ci.org/AnthonyEbert/queuecomputer.svg?branch=simple_exports2)](https://travis-ci.org/AnthonyEbert/queuecomputer) [![codecov](https://codecov.io/gh/AnthonyEbert/queuecomputer/branch/master/graph/badge.svg)](https://codecov.io/gh/AnthonyEbert/queuecomputer)

<!-- --- -->
<!-- output: html -->
<!-- bibliography: references.bib -->
<!-- --- -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
queuecomputer
=============

Overview
--------

queuecomputer implements a new and computationally efficient method for simulating from a general set of queues. The current most popular method for simulating queues is Discete Event Simulation (DES). The top R package for DES is called simmer and the top Python package is called SimPy. We have validated and benchmarked queuecomputer against both these packages and found that queuecomputer is two orders of magnitude faster than either package.

Simulating arbitrary queues is difficult, however once:

1.  The arrival times A and service times S are known for all customers and,
2.  the server resource schedule is specified

then the departure times D for all customers can be computed deterministically.

The focus on this package is:

-   fast computation of departure times given arrival and service times, and
-   a flexible framework to allow for extensions such as server effects.

It is up to the user to provide arrival and service times, and therefore very complicated distributions can be simulated (by the user) and tested with this package.

Installation
------------

``` r
install.packages('queuecomputer')

# For the in-development version
devtools::install_github("AnthonyEbert/queuecomputer")
```

Usage
-----

In this example of a queueing network, customers must pass through two queues. The arrival times to the first queue come in two waves starting at time 100 and time 500. The arrival times to the second queue are the departure times of the first queue plus the time they spent walking to the second queue.

``` r
library(queuecomputer)
library(ggplot2)
library(reshape2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

set.seed(1)

n <- 100

arrivals_1 <- c(100 + cumsum(rexp(n)), 500 + cumsum(rexp(n)))
service_1 <- rexp(2*n, 1/2.5)

queue_1 <- queue_step(arrivals = arrivals_1, service = service_1, servers = 2)

walktimes <- rexp(2*n, 1/100)

arrivals_2 <- lag_step(arrivals = queue_1, service = walktimes)
service_2 <- rexp(2*n, 1/3)

queue_2 <- queue_step(arrivals = arrivals_2, service = service_2, servers = 1)

head(arrivals_1)
#> [1] 100.7552 101.9368 102.0825 102.2223 102.6584 105.5534
head(queue_1$departures_df)
#> # A tibble: 6 × 6
#>   arrivals   service departures       waiting system_time server
#>      <dbl>     <dbl>      <dbl>         <dbl>       <dbl>  <dbl>
#> 1 100.7552 0.1890576   100.9442 -6.383782e-15   0.1890576      1
#> 2 101.9368 2.5656478   104.5025  4.440892e-15   2.5656478      2
#> 3 102.0825 1.6870828   103.7696  0.000000e+00   1.6870828      1
#> 4 102.2223 2.0013664   105.7710  1.547288e+00   3.5486540      1
#> 5 102.6584 0.4351757   104.9376  1.844077e+00   2.2792529      2
#> 6 105.5534 1.6836613   107.2370  0.000000e+00   1.6836613      2
head(arrivals_2)
#> [1] 120.3923 105.6711 227.5242 175.9008 339.9853 108.7119
head(queue_2$departures_df)
#> # A tibble: 6 × 6
#>   arrivals   service departures       waiting system_time server
#>      <dbl>     <dbl>      <dbl>         <dbl>       <dbl>  <dbl>
#> 1 120.3923 5.1599751   125.5523 -2.664535e-15    5.159975      1
#> 2 105.6711 1.5841166   107.2552  0.000000e+00    1.584117      1
#> 3 227.5242 0.1136285   290.7928  6.315502e+01   63.268650      1
#> 4 175.9008 2.3516652   186.2768  8.024359e+00   10.376024      1
#> 5 339.9853 3.1978792   404.4597  6.127651e+01   64.474391      1
#> 6 108.7119 1.2304520   109.9423  0.000000e+00    1.230452      1

summary(queue_1)
#> $number_customers
#> [1] 200
#> 
#> $missed_customers
#> [1] 0
#> 
#> $qlength_sum
#> # A tibble: 24 × 2
#>    queuelength  proportion
#>          <dbl>       <dbl>
#> 1            0 0.635293164
#> 2            1 0.017711485
#> 3            2 0.019022001
#> 4            3 0.013198290
#> 5            4 0.015530023
#> 6            5 0.013069273
#> 7            6 0.005422745
#> 8            7 0.015264468
#> 9            8 0.013490444
#> 10           9 0.028604615
#> # ... with 14 more rows
#> 
#> $qlength_mean
#> [1] 3.704555
#> 
#> $slength_sum
#> # A tibble: 26 × 2
#>    queuelength  proportion
#>          <dbl>       <dbl>
#> 1            0 0.609011851
#> 2            1 0.013772165
#> 3            2 0.013349133
#> 4            3 0.017670692
#> 5            4 0.018978190
#> 6            5 0.013167892
#> 7            6 0.015494255
#> 8            7 0.013039172
#> 9            8 0.005410256
#> 10           9 0.015229312
#> # ... with 16 more rows
#> 
#> $slength_mean
#> [1] 4.464227
#> 
#> $mrt
#> [1] 13.74786
#> 
#> $mwt
#> [1] 11.38213
#> 
#> $departures_sum
#>     arrivals        service            departures       waiting      
#>  Min.   :100.8   Min.   : 0.004252   Min.   :100.9   Min.   : 0.000  
#>  1st Qu.:149.5   1st Qu.: 0.712788   1st Qu.:166.8   1st Qu.: 6.928  
#>  Median :352.5   Median : 1.646313   Median :366.5   Median :12.493  
#>  Mean   :350.7   Mean   : 2.365731   Mean   :364.4   Mean   :11.382  
#>  3rd Qu.:544.9   3rd Qu.: 3.240546   3rd Qu.:561.0   3rd Qu.:14.950  
#>  Max.   :596.9   Max.   :15.828209   Max.   :615.9   Max.   :25.153  
#>   system_time      server     response      
#>  Min.   : 0.1891   1: 83   Min.   : 0.1891  
#>  1st Qu.:10.0196   2:117   1st Qu.:10.0196  
#>  Median :14.5664           Median :14.5664  
#>  Mean   :13.7479           Mean   :13.7479  
#>  3rd Qu.:17.4366           3rd Qu.:17.4366  
#>  Max.   :31.0380           Max.   :31.0380  
#> 
#> $utilization
#> [1] 0.3841021

summary(queue_2)
#> $number_customers
#> [1] 200
#> 
#> $missed_customers
#> [1] 0
#> 
#> $qlength_sum
#> # A tibble: 32 × 2
#>    queuelength  proportion
#>          <dbl>       <dbl>
#> 1            0 0.545312292
#> 2            1 0.022278305
#> 3            2 0.025908718
#> 4            3 0.030868563
#> 5            4 0.023298844
#> 6            5 0.024395660
#> 7            6 0.009027140
#> 8            7 0.020010249
#> 9            8 0.013111985
#> 10           9 0.008009216
#> # ... with 22 more rows
#> 
#> $qlength_mean
#> [1] 5.711736
#> 
#> $slength_sum
#> # A tibble: 33 × 2
#>    queuelength  proportion
#>          <dbl>       <dbl>
#> 1            0 0.480839692
#> 2            1 0.066087407
#> 3            2 0.022199185
#> 4            3 0.025816704
#> 5            4 0.030758934
#> 6            5 0.023216100
#> 7            6 0.024309019
#> 8            7 0.008995081
#> 9            8 0.019939183
#> 10           9 0.013065419
#> # ... with 23 more rows
#> 
#> $slength_mean
#> [1] 6.210611
#> 
#> $mrt
#> [1] 37.19555
#> 
#> $mwt
#> [1] 34.08628
#> 
#> $departures_sum
#>     arrivals         service            departures        waiting     
#>  Min.   : 105.7   Min.   : 0.005783   Min.   : 107.3   Min.   : 0.00  
#>  1st Qu.: 235.8   1st Qu.: 1.168675   1st Qu.: 297.2   1st Qu.:10.60  
#>  Median : 520.7   Median : 2.185065   Median : 522.2   Median :31.00  
#>  Mean   : 468.0   Mean   : 3.109268   Mean   : 505.2   Mean   :34.09  
#>  3rd Qu.: 645.8   3rd Qu.: 4.284097   3rd Qu.: 693.7   3rd Qu.:53.76  
#>  Max.   :1193.6   Max.   :16.737074   Max.   :1197.8   Max.   :84.59  
#>   system_time      server     response      
#>  Min.   : 0.1203   1:200   Min.   : 0.1203  
#>  1st Qu.:14.1416           1st Qu.:14.1416  
#>  Median :36.0862           Median :36.0862  
#>  Mean   :37.1955           Mean   :37.1955  
#>  3rd Qu.:57.2661           3rd Qu.:57.2661  
#>  Max.   :86.0167           Max.   :86.0167  
#> 
#> $utilization
#> [1] 0.5191603

curve(ecdf(arrivals_1)(x) * 2*n , from = 0, to = 1500,
    xlab = "time", ylab = "Number of customers")
curve(ecdf(depart(queue_1))(x) * 2*n , add = TRUE, col = "red")
curve(ecdf(arrivals_2)(x) * 2*n, add = TRUE, col = "blue")
curve(ecdf(depart(queue_2))(x) * 2*n, add = TRUE, col = "green")
legend(600,70, legend = c("Customer arrivals to firstqeue",
    "Customer output - firstqueue",
    "Customer arrivals to second queue", 
    "Customer output - secondqueue"),
    col = c("black","red","blue", "green"), lwd = 1, cex = 0.8
)
```

![](README-unnamed-chunk-3-1.png)

More information
----------------

For more information on how to use the package see the package vignettes or the R help files.

Acknowledgements
----------------

I'd like to thank my supervisors [Professor Kerrie Mengersen](https://bragqut.wordpress.com/mengersen/), [Dr Paul Wu](https://bragqut.wordpress.com/people/research-staff/wu/) and [Professor Fabrizio Ruggeri](http://www.mi.imati.cnr.it/fabrizio/).

This work was supported by the [ARC Centre of Excellence for Mathematical and Statistical Frontiers (ACEMS)](http://acems.org.au/). This work was funded through the ARC Linkage Grant “Improving the Productivity and Efficiency of Australian Airports” (LP140100282).
