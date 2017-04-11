
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
#> Total customers:
#>  200
#> Missed customers:
#>  0
#> Mean waiting time:
#>  11.4
#> Mean response time:
#>  13.7
#> Utilization factor:
#>  0.384
#> Mean queue length:
#>  3.7
#> Mean number of customers in system:
#>  4.46

summary(queue_2)
#> Total customers:
#>  200
#> Missed customers:
#>  0
#> Mean waiting time:
#>  34.1
#> Mean response time:
#>  37.2
#> Utilization factor:
#>  0.519
#> Mean queue length:
#>  5.71
#> Mean number of customers in system:
#>  6.21
```

More information
----------------

For more information on how to use the package see the package vignettes or the R help files.

Acknowledgements
----------------

I'd like to thank my supervisors [Professor Kerrie Mengersen](https://bragqut.wordpress.com/mengersen/), [Dr Paul Wu](https://bragqut.wordpress.com/people/research-staff/wu/) and [Professor Fabrizio Ruggeri](http://www.mi.imati.cnr.it/fabrizio/).

This work was supported by the [ARC Centre of Excellence for Mathematical and Statistical Frontiers (ACEMS)](http://acems.org.au/). This work was funded through the ARC Linkage Grant “Improving the Productivity and Efficiency of Australian Airports” (LP140100282).
