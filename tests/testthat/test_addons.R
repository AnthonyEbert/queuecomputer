

library(dplyr)
library(testthat)
library(queuecomputer)


base::load(file = "../create_batch_test.RData")

set.seed(1)

flight_schedule <- data_frame(
  flight = c("F1", "F2"),
  time = c(0, 50),
  n = c(100, 100),
  shape = c(5, 5),
  rate = c(1 , 1),
  log_mu = c(1 , 2)
)

passenger_df_2 <- flight_schedule %>% group_by(flight) %>%
  do(create_batches(., arrival_dist = "rgamma",
    service_rate = exp(1/.$log_mu),
    time = .$time)
  )

test_that("create_batches", {
  expect_equal(passenger_df_2$arrivals, passenger_df$arrivals)
  expect_equal(passenger_df_2$service, passenger_df$service)
})

## Queue length test

set.seed(2)

n_customers <- 1e3

arrivals <- cumsum(rexp(n_customers, 1.8))
service <- rexp(n_customers)

departures <- queue(arrivals, service, servers = 2)
queue_obj <- queue_step(arrivals, service, servers = 2)

queue_length_obj2 <- queue_lengths(arrivals, service, departures)

test_that("Queue_lengths", {
  expect_equal(queue_obj$queuelength_df, queue_length_obj2)
})
