
library(testthat)
library(queuecomputer)

# test_check("queuecomputer")

#Deterministic check -----------------------------------

##Tested and checked data -------------------------------------

# Same computation as newdataframe

##New data -------------------------

# session::restore.session(file = "20161909testdata.rda")

base::load(file = "../20161017testdataframe.RData")

set.seed(700)

arrival_df <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 3))
ord <- order(arrival_df$times)

service <- rlnorm(100)
server_list <- server_split(c(15,30,50),c(1,3,1,10))

firstqueue <- queue_step(arrival_df = arrival_df, service = service)
secondqueue <- queue_step(arrival_df = arrival_df,
  server_list = server_list, service = service)

# testdataframe <- data.frame(arrival_df[ord, ], service[ord], firstqueue$times[ord], secondqueue$times[ord])

newdataframe <- data.frame(arrival_df[ord, ], service[ord], firstqueue$times[ord], secondqueue$times[ord])

# save(testdataframe, file = "tests/20160921testdataframe.RData")

# save(testdataframe, file = "tests/testdataframe.RData")

test_that("test that deterministic queue simulation departure times haven't changed", {
  expect_equal(newdataframe, testdataframe)
})

#Check lag_step and queue_step with large number of servers returns same results ------------------

lag_queue <- lag_step(arrival_df = arrival_df, service = service)
server_list <- server_split(c(1),c(100,100))

secondqueue <- queue_step(arrival_df = arrival_df, server_list = server_list, service = service)

test_that("lag_step returns same results as queue_step with large number of servers", {
  expect_equal(lag_queue$times, secondqueue$times)
})
