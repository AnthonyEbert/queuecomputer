
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/queuecomputer)](https://CRAN.R-project.org/package=queuecomputer)
[![Downloads](http://cranlogs.r-pkg.org/badges/queuecomputer)](https://CRAN.R-project.org/package=queuecomputer)
[![Build
Status](https://travis-ci.org/AnthonyEbert/queuecomputer.svg?branch=master)](https://travis-ci.org/AnthonyEbert/queuecomputer)
[![codecov](https://codecov.io/gh/AnthonyEbert/queuecomputer/branch/master/graph/badge.svg)](https://codecov.io/gh/AnthonyEbert/queuecomputer)

<!-- --- -->

<!-- output: html -->

<!-- bibliography: references.bib -->

<!-- --- -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# queuecomputer

## Overview

queuecomputer implements a new and computationally efficient method for
simulating from a general set of queues. The current most popular method
for simulating queues is Discete Event Simulation (DES). The top R
package for DES is called simmer and the top Python package is called
SimPy. We have validated and benchmarked queuecomputer against both
these packages and found that queuecomputer is two orders of magnitude
faster than either package.

Simulating arbitrary queues is difficult, however once:

1.  The arrival times A and service times S are known for all customers
    and,
2.  the server resource schedule is specified

then the departure times D for all customers can be computed
deterministically.

The focus on this package is:

  - fast computation of departure times given arrival and service times,
    and
  - a flexible framework to allow for extensions such as server effects.

It is up to the user to provide arrival and service times, and therefore
very complicated distributions can be simulated (by the user) and tested
with this package.

For detailed information regarding the algorithm used in this package
see our paper:

Ebert A, Wu P, Mengersen K, Ruggeri F (2020). “Computationally Efficient
Simulation of Queues: The R Package queuecomputer.” *Journal of
Statistical Software*, *95*(5), 1-29. doi: 10.18637/jss.v095.i05 (URL:
<https://doi.org/10.18637/jss.v095.i05>).

## Installation

``` r
install.packages('queuecomputer')

# For the in-development version
devtools::install_github("AnthonyEbert/queuecomputer")
```

## Usage

In this example of a queueing network, customers must pass through two
queues. The arrival times to the first queue come in two waves starting
at time 100 and time 500. The arrival times to the second queue are the
departure times of the first queue plus the time they spent walking to
the second queue.

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
#> # A tibble: 6 x 6
#>   arrivals service departures   waiting system_time server
#>      <dbl>   <dbl>      <dbl>     <dbl>       <dbl>  <int>
#> 1     101.   0.189       101. -6.38e-15       0.189      1
#> 2     102.   2.57        105.  4.44e-15       2.57       2
#> 3     102.   1.69        104.  0.             1.69       1
#> 4     102.   2.00        106.  1.55e+ 0       3.55       1
#> 5     103.   0.435       105.  1.84e+ 0       2.28       2
#> 6     106.   1.68        107.  0.             1.68       2
head(arrivals_2)
#> [1] 120.3923 105.6711 227.5242 175.9008 339.9853 108.7119
head(queue_2$departures_df)
#> # A tibble: 6 x 6
#>   arrivals service departures   waiting system_time server
#>      <dbl>   <dbl>      <dbl>     <dbl>       <dbl>  <int>
#> 1     120.   5.16        126. -2.66e-15        5.16      1
#> 2     106.   1.58        107.  0.              1.58      1
#> 3     228.   0.114       291.  6.32e+ 1       63.3       1
#> 4     176.   2.35        186.  8.02e+ 0       10.4       1
#> 5     340.   3.20        404.  6.13e+ 1       64.5       1
#> 6     109.   1.23        110.  0.              1.23      1

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
#>  0.38410206651912
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
#>  0.519160307775743
#> Mean queue length:
#>  5.71
#> Mean number of customers in system:
#>  6.21
```

## Acknowledgements

I’d like to thank my supervisors [Professor Kerrie
Mengersen](https://bragqut.wordpress.com/mengersen/), [Dr Paul
Wu](https://bragqut.wordpress.com/people/research-staff/wu/) and
[Professor Fabrizio Ruggeri](http://www.mi.imati.cnr.it/fabrizio/).

This work was supported by the [ARC Centre of Excellence for
Mathematical and Statistical Frontiers (ACEMS)](http://acems.org.au/).
This work was funded through the ARC Linkage Grant “Improving the
Productivity and Efficiency of Australian Airports” (LP140100282).
