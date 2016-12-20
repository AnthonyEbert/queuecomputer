
library(testthat)
library(queuecomputer)

# Same computation as newdataframe

##New data -------------------------

base::load(file = "../20161017testdataframe.RData")

set.seed(700)

arrival_df <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 3))
ord <- order(arrival_df$times)

service <- rlnorm(100)
server_list <- as.server.stepfun(c(15,30,50),c(1,3,1,10))

firstqueue <- queue_step(arrival_df = arrival_df, service = service)
secondqueue <- queue_step(arrival_df = arrival_df,
  servers = server_list, service = service)

# testdataframe <- data.frame(arrival_df[ord, ], service[ord], firstqueue$times[ord], secondqueue$times[ord])

newdataframe <- data.frame(arrival_df[ord, ], service[ord], firstqueue$times[ord], secondqueue$times[ord])

# save(testdataframe, file = "tests/20160921testdataframe.RData")

# save(testdataframe, file = "tests/testdataframe.RData")

test_that("test that deterministic queue simulation departure times haven't changed", {
  expect_equal(newdataframe, testdataframe)
})

#Check lag_step and queue_step with large number of servers returns same results ------------------

lag_queue <- lag_step(arrival_df = arrival_df, service = service)
server_list <- as.server.stepfun(c(1),c(100,100))

secondqueue <- queue_step(arrival_df = arrival_df, servers = server_list, service = service)

test_that("lag_step returns same results as queue_step with large number of servers", {
  expect_equal(lag_queue$times, secondqueue$times)
})


# Check reordering doesn't change anything -------------------

arrival_df <- data.frame(ID = c(1:500), times = rlnorm(500, meanlog = 3))

service <- rlnorm(500)

ord <- order(arrival_df$times)

## Numeric -----------------------

q1n <- queue_step(arrival_df, service, servers = 2)
q2n <- queue_step(arrival_df[ord,], service[ord], servers = 2)

test_that("reorder numeric", {
  expect_equal(q1n$times, q2n[order(ord),]$times)
  expect_equal(attr(q1n, "server") ,attr(q2n, "server")[order(ord)])
})


## server.stepfun ----------------------

server_sf <- as.server.stepfun(c(50, 200),c(1,0,2))


q1sf <- queue_step(arrival_df, service, servers = server_sf)
q2sf <- queue_step(arrival_df[ord,], service[ord], servers = server_sf)

test_that("reorder stepfun", {
  expect_equal(q1sf$times , q2sf[order(ord),]$times)
})

## server.list -------------------------

server_list <- queuecomputer:::server_make(c(50, 200),c(1,0,2))


q1sl <- queue_step(arrival_df, service, servers = server_list)
q2sl <- queue_step(arrival_df[ord,], service[ord], servers = server_list)

test_that("reorder server.list", {
  expect_equal(q1sl$times , q2sl[order(ord),]$times)
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

qsf <- queue_step(arrival_df, service, servers = server_sf)
qsl <- queue_step(arrival_df, service, servers = server_list)

test_that("Check service times", {
  expect_equal(qsf$times , qsl$times)
})

service <- pmin.int(rlnorm(500, meanlog = 3), 24)

server_sf <- as.server.stepfun(c(50, 200, 250, 275),c(1,0,1,0,1))
server_list <- queuecomputer:::server_make(c(50, 200, 250, 275),c(1,0,1,0,1))

qsf <- queue_step(arrival_df, service, servers = server_sf)
qsl <- queue_step(arrival_df, service, servers = server_list)

test_that("Check service times, zeros", {
  expect_equal(qsf$times , qsl$times)
})

server_sf <- as.server.stepfun(c(50, 200, 250, 275),c(1,0,1,0,1))
server_list <- as.server.list(list(c(50, 200, 250, 275)),1)

qsf <- queue_step(arrival_df, service, servers = server_sf)
qsl <- queue_step(arrival_df, service, servers = server_list)

test_that("Check service times, zeros with as.server.list", {
    expect_equal(qsf$times , qsl$times)
})

# Check that a warning is produced

set.seed(1)

service <- rlnorm(500, meanlog = 3)

server_sf <- as.server.stepfun(c(50, 200, 250, 275),c(1,0,1,0,1))

test_that("Check warning is produced", {
  expect_warning(qsf <- queue_step(arrival_df, service, servers = server_sf))
})


# Check queue_step.server.stepfun and queue_step.numeric give same answer.

arrival_df <- data.frame(ID = c(1:500), times = rlnorm(500, meanlog = 3))

service <- pmin.int(rlnorm(500, meanlog = 4), 25)

ord <- order(arrival_df$times)


server_sf <- as.server.stepfun(c(50), c(2,2))

qsf <- queue_step(arrival_df, service, servers = server_sf)
qn <- queue_step(arrival_df, service, servers = 2)

all(qsf == qn)

# Check second queue output is later than first --------------

set.seed(1L)
n_customers <- 100
arrival_df <- data.frame(ID = c(1:n_customers), times = rlnorm(n_customers, meanlog = 3))
service_1 <- rlnorm(n_customers)


firstqueue <- queue_step(arrival_df = arrival_df,
  servers = 2, service = service_1)

server_list <- as.server.stepfun(c(50),c(1,2))

service_2 <- rlnorm(n_customers)
secondqueue <- queue_step(arrival_df = firstqueue,
  servers = server_list, service = service_2)

all(firstqueue$times >= secondqueue$times)


# Wait step

test_that("Wait step for bags", {
    expect_equal(wait_step(arrival_df, service)$times, pmax.int(arrival_df$times, service))
})


# Post analysis, just running some functions to see if they work.

summary(firstqueue)


