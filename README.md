
<!-- README.md is generated from README.Rmd. Please edit that file -->
queuecomputer
=============

\[ \]

Install
-------

``` r
devtools::install_github("AnthonyEbert/queuecomputer")
library(queuecomputer)
```

    #> Loading queuecomputer

Why more queueing software?
---------------------------

There is already a lot of queueing simulation packages out there including the following R packages:

-   [liqueueR](https://cran.r-project.org/web/packages/liqueueR/index.html),
-   [queueing](https://cran.r-project.org/web/packages/queueing/index.html) &
-   [rstackdeque](https://cran.r-project.org/web/packages/rstackdeque/index.html).

So what does this package do differently to the others? The focus of this package is on queue computation rather than queue simulation. Real world queues have all sorts of strange arrival and service distributions so this can make the job of queue simulation difficult. However once

1.  The arrival times \(t^a\) and service times \(s\) are known for all customers and,
2.  the number of servers is specified

then the departure times \(t^d\) for all customers can be computed exactly. The computation of departure times for a single-server FIFO (first-in-first-out) queue is simple and efficient:

\[ t^d_k = \text{max}(t^d_{k-1}, t^a_k) + s_k \]

where \(t^d_k\), \(t^a_k\) and \(s_k\) are the departure, arrival and service times for the \(k\)th customer respectively, see (Sutton and Jordan (2011), pg 258). Note: the customers have been sorted in order of arrival times. The intuition here is that the \(k\)th customer must wait for the \(k-1\)th customer to be served.

This result can be extended to multi-server queues in the following manner:

\[ t_k^d = \text{max}(\text{min}(\text{n latest departure times for all customers 1:(k-1)}), \quad t^a_k) + s_k \]

for a queue with \(n\) servers. This simple, but unreported extension to Sutton and Jordan (2011) greatly simplifies the computation of departure times for multi-server queues. It is up to the user to provide arrival and service times, and therefore very complicated distributions can be simulated (by the user) and tested with this package.

This package was inspired by the problem of modelling passenger flows through an international airport terminal. Batch arrivals (planes) happen throughout the day at predetermined times at different parts of the airport. A completely flexible queueing computation engine is needed to allow for the complex arrival and service distributions. An efficient computation engine is needed to allow for Bayesian sampling of the parameters to return posteriors for the queue parameters.

``` r
arrival_df <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 3))
service <- rlnorm(100)
firstqueue <- queue_step(arrival_df = arrival_df, service = service)
secondqueue <- queue_step(arrival_df = arrival_df,
    Number_of_queues = stepfun(c(15,30,50), c(1,3,1,10)), service = service)

curve(ecdf(arrival_df$times)(x) * 100 , from = 0, to = 200,
    xlab = "time", ylab = "Number of customers")
curve(ecdf(firstqueue$times)(x) * 100 , add = TRUE, col = "red")
curve(ecdf(secondqueue$times)(x) * 100, add = TRUE, col = "blue")
legend(100,40, legend = c("Customer input - arrivals",
    "Customer output - firstqueue",
    "Customer output - secondqueue"),
    col = c("black","red","blue"), lwd = 1, cex = 0.8
)
```

![](README-unnamed-chunk-4-1.png)

References
==========

Sutton, Charles, and Michael I Jordan. 2011. “Bayesian Inference for Queueing Networks and Modeling of Internet Services.” *The Annals of Applied Statistics*. JSTOR, 254–82.
