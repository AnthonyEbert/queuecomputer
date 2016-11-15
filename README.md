
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
# For the CRAN version (yet to be released)
install.packages('queuecomputer')

# For the in-development version
devtools::install_github("AnthonyEbert/queuecomputer")
```

Usage
-----

In this example of a customers must pass through two queues. The arrival times to the first queue come in two waves starting at time 100 and time 500. The arrival times to the second queue are the departure times of the first queue plus the time they spent walking to the second queue.

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

arrival_df1 <- data.frame(ID = c(1:200), times = c(100 + cumsum(rexp(100)), 500 + cumsum(rexp(100))))
service1 <- rexp(200, 1/2.5)

queue_1 <- queue_step(arrival_df = arrival_df1, service = service1, servers = 2)

walktimes <- rexp(200, 1/100)

arrival_df2 <- lag_step(arrival_df = queue_1, service = walktimes)
service2 <- rexp(200, 1/3)

queue_2 <- queue_step(arrival_df = arrival_df2, service = service2, servers = 1)

head(arrival_df1)
#>   ID    times
#> 1  1 100.7552
#> 2  2 101.9368
#> 3  3 102.0825
#> 4  4 102.2223
#> 5  5 102.6584
#> 6  6 105.5534
head(queue_1)
#>   ID    times
#> 1  1 100.9442
#> 2  2 104.5025
#> 3  3 103.7696
#> 4  4 105.7710
#> 5  5 104.9376
#> 6  6 107.2370
head(arrival_df2)
#>   ID    times
#> 1  1 120.3923
#> 2  2 105.6711
#> 3  3 227.5242
#> 4  4 175.9008
#> 5  5 339.9853
#> 6  6 108.7119
head(queue_2)
#>   ID    times
#> 1  1 125.5523
#> 2  2 107.2552
#> 3  3 290.7928
#> 4  4 186.2768
#> 5  5 404.4597
#> 6  6 109.9423

summary(queue_1)
#> 
#> Mean waiting time:
#>  11.3821
#> Mean response time:
#>  13.7479
#> Utilization factor:
#>  0.3841
#> Queue Lengths:
#>           0           1           2           3           4           5 
#> 0.636038961 0.017694805 0.019155844 0.012824675 0.015584416 0.013474026 
#>           6           7           8           9          10          11 
#> 0.004870130 0.015584416 0.013474026 0.028571429 0.036201299 0.044805195 
#>          12          13          14          15          16          17 
#> 0.031331169 0.025974026 0.015584416 0.022564935 0.012012987 0.011525974 
#>          18          19          20          21          22          23 
#> 0.005681818 0.005681818 0.001461039 0.005519481 0.000974026 0.003409091 
#> 
#> System Lengths:
#>           0           1           2           3           4           5 
#> 0.608928571 0.013961039 0.013149351 0.017694805 0.019155844 0.012824675 
#>           6           7           8           9          10          11 
#> 0.015584416 0.013474026 0.004870130 0.015584416 0.013474026 0.028571429 
#>          12          13          14          15          16          17 
#> 0.036201299 0.044805195 0.031331169 0.025974026 0.015584416 0.022564935 
#>          18          19          20          21          22          23 
#> 0.012012987 0.011525974 0.005681818 0.005681818 0.001461039 0.005519481 
#>          24          25 
#> 0.000974026 0.003409091

summary(queue_2)
#> 
#> Mean waiting time:
#>  34.0863
#> Mean response time:
#>  37.1955
#> Utilization factor:
#>  0.5192
#> Queue Lengths:
#>            0            1            2            3            4 
#> 5.467902e-01 2.245596e-02 2.571166e-02 3.088739e-02 2.304032e-02 
#>            5            6            7            8            9 
#> 2.437599e-02 9.015778e-03 1.995158e-02 1.302279e-02 7.930545e-03 
#>           10           11           12           13           14 
#> 2.120377e-02 2.813257e-02 1.769764e-02 2.671342e-02 1.369063e-02 
#>           15           16           17           18           19 
#> 1.010101e-02 1.001753e-02 1.819851e-02 8.848819e-03 9.600134e-03 
#>           20           21           22           23           24 
#> 1.001753e-02 9.349695e-03 9.349695e-03 1.928375e-02 2.804909e-02 
#>           25           26           27           28           29 
#> 9.015778e-03 7.513148e-03 8.598380e-03 1.502630e-03 8.097504e-03 
#>           30           31 
#> 8.347942e-05 1.753068e-03 
#> 
#> System Lengths:
#>            0            1            2            3            4 
#> 4.809250e-01 6.586526e-02 2.245596e-02 2.571166e-02 3.088739e-02 
#>            5            6            7            8            9 
#> 2.304032e-02 2.437599e-02 9.015778e-03 1.995158e-02 1.302279e-02 
#>           10           11           12           13           14 
#> 7.930545e-03 2.120377e-02 2.813257e-02 1.769764e-02 2.671342e-02 
#>           15           16           17           18           19 
#> 1.369063e-02 1.010101e-02 1.001753e-02 1.819851e-02 8.848819e-03 
#>           20           21           22           23           24 
#> 9.600134e-03 1.001753e-02 9.349695e-03 9.349695e-03 1.928375e-02 
#>           25           26           27           28           29 
#> 2.804909e-02 9.015778e-03 7.513148e-03 8.598380e-03 1.502630e-03 
#>           30           31           32 
#> 8.097504e-03 8.347942e-05 1.753068e-03

curve(ecdf(arrival_df1$times)(x) * 200 , from = 0, to = 1500,
    xlab = "time", ylab = "Number of customers")
curve(ecdf(queue_1$times)(x) * 200 , add = TRUE, col = "red")
curve(ecdf(arrival_df2$times)(x) * 200, add = TRUE, col = "blue")
curve(ecdf(queue_2$times)(x) * 200, add = TRUE, col = "green")
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
