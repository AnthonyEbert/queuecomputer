
<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/queuecomputer)](https://CRAN.R-project.org/package=queuecomputer)
[![Downloads](https://cranlogs.r-pkg.org/badges/queuecomputer)](https://CRAN.R-project.org/package=queuecomputer)
[![R-CMD-check](https://github.com/AnthonyEbert/queuecomputer/workflows/R-CMD-check/badge.svg)](https://github.com/AnthonyEbert/queuecomputer/actions)
[![codecov](https://codecov.io/gh/AnthonyEbert/queuecomputer/branch/master/graph/badge.svg)](https://app.codecov.io/gh/AnthonyEbert/queuecomputer)
[![DOI](https://img.shields.io/badge/doi-10.18637/jss.v095.i05-informational.svg)](https://doi.org/10.18637/jss.v095.i05)
<!-- badges: end -->

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
```

## Usage

### Simple example

We demonstrate simulating the first 50 customers from a M/M/2 queue. In
queueing theory, M/M/2 refers to a queue with exponential inter-arrival
times, exponential service times and two servers.

``` r
library(queuecomputer)

n <- 50

arrivals <- cumsum(rexp(n, 1.9))
service <- rexp(n)

queue_mm2 <- queue_step(arrivals = arrivals, service = service, servers = 2)
```

You can see the table of customer arrival, service and departure times
by accessing the `departures_df` object from queue_mm2.

``` r
queue_mm2$departures_df
#> # A tibble: 50 × 6
#>    arrivals service departures waiting system_time server
#>       <dbl>   <dbl>      <dbl>   <dbl>       <dbl>  <int>
#>  1    0.397   0.422      0.820   0           0.422      1
#>  2    1.02    2.18       3.20    0           2.18       2
#>  3    1.10    3.22       4.31    0           3.22       1
#>  4    1.17    0.558      3.76    2.03        2.59       2
#>  5    1.40    0.595      4.35    2.36        2.95       2
#>  6    2.92    0.977      5.29    1.39        2.37       1
#>  7    3.57    0.210      4.56    0.781       0.991      2
#>  8    3.85    0.309      4.87    0.706       1.02       2
#>  9    4.36    1.11       5.98    0.512       1.62       2
#> 10    4.43    0.774      6.07    0.856       1.63       1
#> # ℹ 40 more rows
```

You can see visualisations of the queueing system.

``` r
plot(queue_mm2)
#> [[1]]
```

![](README-unnamed-chunk-5-1.png)<!-- -->

    #> 
    #> [[2]]

![](README-unnamed-chunk-5-2.png)<!-- -->

    #> 
    #> [[3]]

![](README-unnamed-chunk-5-3.png)<!-- -->

    #> 
    #> [[4]]

![](README-unnamed-chunk-5-4.png)<!-- -->

    #> 
    #> [[5]]

![](README-unnamed-chunk-5-5.png)<!-- -->

A summary of the performance of the queueing system can be computed.

``` r
summary(queue_mm2)
#> Total customers:
#>  50
#> Missed customers:
#>  0
#> Mean waiting time:
#>  2.91
#> Mean response time:
#>  3.99
#> Utilization factor:
#>  0.897026364068736
#> Mean queue length:
#>  4.9
#> Mean number of customers in system:
#>  6.64
```

### Queueing network

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
#> Vedhæfter pakke: 'dplyr'
#> De følgende objekter er maskerede fra 'package:stats':
#> 
#>     filter, lag
#> De følgende objekter er maskerede fra 'package:base':
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
#>   arrivals service departures  waiting system_time server
#>      <dbl>   <dbl>      <dbl>    <dbl>       <dbl>  <int>
#> 1     101.   0.189       101. 0              0.189      1
#> 2     102.   2.57        105. 4.44e-15       2.57       2
#> 3     102.   1.69        104. 0              1.69       1
#> 4     102.   2.00        106. 1.55e+ 0       3.55       1
#> 5     103.   0.435       105. 1.84e+ 0       2.28       2
#> 6     106.   1.68        107. 0              1.68       2
head(arrivals_2)
#> [1] 120.3923 105.6711 227.5242 175.9008 339.9853 108.7119
head(queue_2$departures_df)
#> # A tibble: 6 × 6
#>   arrivals service departures waiting system_time server
#>      <dbl>   <dbl>      <dbl>   <dbl>       <dbl>  <int>
#> 1     120.   5.16        126.    0           5.16      1
#> 2     106.   1.58        107.    0           1.58      1
#> 3     228.   0.114       291.   63.2        63.3       1
#> 4     176.   2.35        186.    8.02       10.4       1
#> 5     340.   3.20        404.   61.3        64.5       1
#> 6     109.   1.23        110.    0           1.23      1

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
