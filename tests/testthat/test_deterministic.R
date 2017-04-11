
library(testthat)
library(queuecomputer)

# Same computation as newdataframe

##New data -------------------------

base::load(file = "../20161017testdataframe.RData")

#### queue_step

set.seed(700)

arrival_df <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 3))
ord <- order(arrival_df$times)

service <- rlnorm(100)
server_list <- as.server.stepfun(c(15,30,50),c(1,3,1,10))

firstqueue <- queue_step(arrival_df$times, service = service)
secondqueue <- queue_step(arrival_df$times,
  servers = server_list, service = service)

# testdataframe <- data.frame(arrival_df[ord, ], service[ord], firstqueue$times[ord], secondqueue$times[ord])

newdataframe <- data.frame(arrival_df[ord, ], service[ord], firstqueue$departures_df$departures[ord], secondqueue$departures_df$departures[ord])

names(newdataframe) <- c("ID", "times", "service.ord.", "firstqueue.times.ord.", "secondqueue.times.ord.")

# save(testdataframe, file = "tests/20160921testdataframe.RData")

# save(testdataframe, file = "tests/testdataframe.RData")

test_that("test that deterministic queue simulation departure times haven't changed", {
  expect_equal(newdataframe, testdataframe)
})


#Check lag_step and queue_step with large number of servers returns same results ------------------


lag_queue <- arrival_df$times + service
server_list <- as.server.stepfun(c(1),c(100,100))

secondqueue <- queue_step(arrival_df$times, servers = server_list, service = service)

test_that("lag_step returns same results as queue_step with large number of servers", {
  expect_equal(lag_queue, secondqueue$departures_df$departures)
})


# Check reordering doesn't change anything -------------------

arrival_df <- data.frame(ID = c(1:500), times = rlnorm(500, meanlog = 3))

service <- rlnorm(500)

ord <- order(arrival_df$times)

## Numeric -----------------------

q1n <- queue(arrival_df$times, service, servers = 2, serveroutput = TRUE)
q2n <- queue(arrival_df$times[ord], service[ord], servers = 2, serveroutput = TRUE)

test_that("reorder numeric", {
  expect_equal(as.numeric(q1n), as.numeric(q2n)[order(ord)])
  expect_equal(attr(q1n, "server") ,attr(q2n, "server")[order(ord)])
})


## server.stepfun ----------------------

server_sf <- as.server.stepfun(c(50, 200),c(1,0,2))


q1sf <- queue_step(arrival_df$times, service, servers = server_sf)
q2sf <- queue_step(arrival_df$times[ord], service[ord], servers = server_sf)

test_that("reorder stepfun", {
  expect_equal(q1sf$departures_df , q2sf$departures_df[order(ord),])
})

## server.list -------------------------

server_list <- queuecomputer:::server_make(c(50, 200),c(1,0,2))


q1sl <- queue_step(arrival_df$times, service, servers = server_list)
q2sl <- queue_step(arrival_df$times[ord], service[ord], servers = server_list)

test_that("reorder server.list", {
  expect_equal(q1sl$departures_df , q2sl$departures_df[order(ord),])
})

## Check summaries don't return errors ----------------

summary(q1n)
summary(q1sf)

# Check service times for server.stepfun ------------------

arrival_df <- data.frame(ID = c(1:500), times = rlnorm(500, meanlog = 1))

service <- pmin.int(rlnorm(500, meanlog = 0.5), 24)

ord <- order(arrival_df$times)


server_sf <- as.server.stepfun(c(50, 200, 250, 275),c(1,2,1,3,1))
server_list <- queuecomputer:::server_make(c(50, 200, 250, 275),c(1,2,1,3,1))

qsf <- queue_step(arrival_df$times, service, servers = server_sf)
qsl <- queue_step(arrival_df$times, service, servers = server_list)

test_that("Check service times", {
  expect_equal(qsf$departures_df , qsl$departures_df)
})

service <- pmin.int(rlnorm(500, meanlog = 3), 24)

server_sf <- as.server.stepfun(c(50, 200, 250, 275),c(1,0,1,0,1))
server_list <- queuecomputer:::server_make(c(50, 200, 250, 275),c(1,0,1,0,1))

qsf <- queue_step(arrival_df$times, service, servers = server_sf)
qsl <- queue_step(arrival_df$times, service, servers = server_list)

test_that("Check service times, zeros", {
  expect_equal(qsf$departures_df , qsl$departures_df)
})

server_sf <- as.server.stepfun(c(50, 200, 250, 275),c(1,0,1,0,1))
server_list <- as.server.list(list(c(50, 200, 250, 275)),1)

qsf <- queue_step(arrival_df$times, service, servers = server_sf)
qsl <- queue_step(arrival_df$times, service, servers = server_list)

test_that("Check service times, zeros with as.server.list", {
    expect_equal(qsf$departures_df , qsl$departures_df)
})

# Check that a warning is produced

set.seed(1)

service <- rlnorm(500, meanlog = 3)

server_sf <- as.server.stepfun(c(50, 200, 250, 275),c(1,0,1,0,1))

test_that("Check warning is produced", {
  expect_warning(qsf <- queue_step(arrival_df$times, service, servers = server_sf))
})


# Check queue_step.server.stepfun and queue_step.numeric give same answer.

arrival_df <- data.frame(ID = c(1:500), times = rlnorm(500, meanlog = 3))

service <- pmin.int(rlnorm(500, meanlog = 4), 25)

ord <- order(arrival_df$times)


server_sf <- as.server.stepfun(c(50), c(2,2))

qsf <- queue_step(arrival_df$times, service, servers = server_sf)
qn <- queue_step(arrival_df$times, service, servers = 2)

test_that("Check queue_step.server.stepfun and queue_step.numeric", { expect_equal(qsf$departures_df, qn$departures_df)
})

# Check second queue output is later than first --------------

set.seed(1L)
n_customers <- 100
arrival_df <- data.frame(ID = c(1:n_customers), times = rlnorm(n_customers, meanlog = 3))
service_1 <- rlnorm(n_customers)


firstqueue <- queue_step(arrival_df$times,
  servers = 2, service = service_1)

server_list <- as.server.stepfun(c(50),c(1,2))

service_2 <- rlnorm(n_customers)
secondqueue <- queue_step(firstqueue,
  servers = server_list, service = service_2)

expect_true(info = "second queue later than first", {
  all(firstqueue$departures_df$departures <= secondqueue$departures_df$departures)
})


# Post analysis, just running some functions to see if they work.

summary(firstqueue)


