

library(dplyr)
library(testthat)
library(queuecomputer)

context("addons")

base::load(file = "../create_batch_test.RData")

set.seed(1)

flight_schedule <- dplyr::tibble(
  flight = c("F1", "F2"),
  time = c(0, 50),
  n = c(100, 100),
  shape = c(5, 5),
  rate = c(1 , 1),
  log_mu = c(1 , 2)
)

## Queue length test

set.seed(2)

n_customers <- 1e3

arrivals <- cumsum(rexp(n_customers, 1.8))
service <- rexp(n_customers)

departures <- queue(arrivals, service, servers = 2)
queue_obj <- queue_step(arrivals, service, servers = 2, labels = 1:n_customers)

queue_length_obj2 <- queue_lengths(arrivals, service, departures)

test_that("Queue_lengths", {
  expect_equal(queue_obj$queuelength_df, queue_length_obj2)
})

plot_obj <- plot(queue_obj, which = c(1:6))

if(require(ggplot2)){
  test_that("plot_object", {
    expect_equal(class(plot_obj), "list")
  })
}

queuecomputer:::performance_output(arrivals, service, queue_obj$departures, servers = 2)
