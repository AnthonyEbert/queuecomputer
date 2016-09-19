library(testthat)
library(queuecomputer)
library(session)

test_check("queuecomputer")

#Deterministic check -----------------------------------

##Tested and checked data -------------------------------------

# Same computation as newdataframe

##New data -------------------------

restore.session(file = "tests/20161909testdata.rda")

set.seed(700)

arrivals <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 4))
service <- rlnorm(100)

ord <- order(arrivals$times)

firstqueue <- queue_step(arrival_df = arrivals, Number_of_queues = stepfun(c(15,30,50), c(1,3,1,10)), service = service)

newdataframe <- data.frame(arrivals[ord, ], service[ord], firstqueue$times[ord])

test_that("test that deterministic queue simulation departure times haven't changed", {
  expect_equal(newdataframe$times, testdataframe$times)
  expect_equal(newdataframe$ID   , testdataframe$ID   )
})

#Check lag_step and queue_step with large number of servers returns same results ------------------

lag_queue <- lag_step(arrival_df = arrivals, service = service)
secondqueue <- queue_step(arrival_df = arrivals, Number_of_queues = stepfun(1, c(100,100)), service = service)

test_that("lag_step returns same results as queue_step with large number of servers", {
  expect_equal(lag_queue$times, secondqueue$times)
})


