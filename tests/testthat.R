
library(testthat)
library(queuecomputer)

# test_check("queuecomputer")

#Deterministic check -----------------------------------

##Tested and checked data -------------------------------------

# Same computation as newdataframe

##New data -------------------------

# session::restore.session(file = "20161909testdata.rda")

base::load(file = "tests/testdataframe.RData")

set.seed(700)

arrivals <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 4))
service <- rlnorm(100)

ord <- order(arrivals$times)

firstqueue <- queue_step(arrival_df = arrivals, Number_of_queues = stepfun(c(15,30,50), c(1,3,1,10)), service = service)

newdataframe <- data.frame(arrivals[ord, ], service[ord], firstqueue$times[ord])

# save(testdataframe, file = "tests/testdataframe.RData")

test_that("test that deterministic queue simulation departure times haven't changed", {
  expect_equal(newdataframe$times, testdataframe$times)
  expect_equal(newdataframe$ID   , testdataframe$ID   )
})

#Second check

set.seed(800)

arrivals2 <- data.frame(ID = c(1:200), times = rlnorm(200, meanlog = 4))
service2 <- rlnorm(200)

ord2 <- order(arrivals2$times)

bigqueue2 <- queue_step(arrival_df = arrivals2, Number_of_queues = stepfun(c(15,30,50,80), c(1,3,5,3,4)), service = service2)

testdataframe2 <- data.frame(arrivals2[ord2, ], service2[ord2], bigqueue2$times[ord2])

write.csv(testdataframe2, file = "tests/testdataframe2.csv")

#Same check with large amount of data

set.seed(800)

arrivals2 <- data.frame(ID = c(1:50000), times = rlnorm(50000, meanlog = 4))
service2 <- rlnorm(50000)

ord2 <- order(arrivals2$times)

bigqueue2 <- queue_step(arrival_df = arrivals2, Number_of_queues = stepfun(c(1), c(3,3)), service = service2)

testdataframe2 <- data.frame(arrivals2[ord, ], service[ord], bigqueue2$times[ord2])

save(testdataframe2, file = "tests/testdataframe2.RData")


#Check lag_step and queue_step with large number of servers returns same results ------------------

lag_queue <- lag_step(arrival_df = arrivals, service = service)
secondqueue <- queue_step(arrival_df = arrivals, Number_of_queues = stepfun(1, c(100,100)), service = service)

test_that("lag_step returns same results as queue_step with large number of servers", {
  expect_equal(lag_queue$times, secondqueue$times)
})

for(i in 1:50){
  newstarttime <- stats::knots(Number_of_queues)[findInterval(n_max(input = output_df[!is.na(output_df)], n = n),stats::knots(Number_of_queues))]
  starttime <- max(starttime,newstarttime)
  print(newstarttime)
  n <- Number_of_queues(max(n_max(input = output_df, n = n), arrival_df$times[i], starttime))
  output_df[i] <- max(n_max(input = output_df, n = n), arrival_df$times[i], starttime) + service[i]
}
