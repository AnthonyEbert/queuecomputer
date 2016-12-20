#

#' Compute the departure times of a set of customers in a queue from their arrival and service times.
#' @param arrivals numeric vector of non-negative arrival times
#' @param service numeric vector of non-negative service times
#' @param servers a non-zero natural number, an object of class \code{server.stepfun}
#' or an object of class \code{server.list}.
#' @param serveroutput boolean whether the server used by each customer should be returned.
#' @description \code{queue} is a faster internal version of \code{queue_step}. It is not compatible with the \code{summary.queue_df} method.
#' @examples
#' queue(rep(1, 7), service = rep(0.2, 7),
#' servers = as.server.stepfun(3.1, c(2, 1)))
#' @seealso
#' \code{\link{queue_step}}
#' @useDynLib queuecomputer
#' @importFrom Rcpp sourceCpp
#' @export
queue <- function(arrivals, service, servers = 1, serveroutput = FALSE){

  stopifnot(all(service >= 0))
  stopifnot(all(arrivals >= 0))
  stopifnot(length(arrivals) == length(service))
  stopifnot(anyNA(c(arrivals, service)) == FALSE )
  stopifnot(is.numeric(arrivals))
  stopifnot(is.numeric(service))

  ordstatement <- is.unsorted(arrivals)

  # Order arrivals and service according to time

  if(ordstatement){
    ord <- order(arrivals, method = "radix")
    arrivals <- arrivals[ord]
    service <- service[ord]
  }

  output <- queue_pass(arrivals = arrivals, service = service, servers = servers)

  departures <- output[1:length(arrivals)]
  queue_vector <- (output[I(length(arrivals) + 1):I(length(output) - 1)])

  if(ordstatement){
    new_ord <- order(ord)
    departures <- departures[new_ord]
    queue_vector <- queue_vector[new_ord]
  }

  if(serveroutput){
    attr(departures, "server") <- queue_vector
  }

  return(departures)
}


queue_pass <- function(arrivals, service, servers){
  UseMethod("queue_pass", servers)
}


queue_pass.numeric <- function(arrivals, service, servers){
  stopifnot((servers%%1 == 0) & (servers > 0))
  stopifnot(length(servers) == 1)
  output <- qloop_numeric(arrivals, service, n_servers = servers)
  return(output)
}


queue_pass.server.stepfun <- function(arrivals, service, servers){

  x = c(servers$x, Inf)
  y = c(servers$y, 1)

  if(length(x) != 2){
    if( any(max(service) >= diff(x))){
      warning("Service times may span multiple server epochs. Results may not be 100% accurate")
    }
  }

  output <- qloop_qq(arrivals, service, x, y)
  return(output)
}


queue_pass.server.list <- function(arrivals, service, servers){
  Number_of_queues <- length(servers)

  queue_times <- mapply(next_function, servers, rep(0, Number_of_queues))
  output <- rep(NA, length(arrivals))
  queue_vector <- rep(NA,length(arrivals))

  for(i in 1:length(arrivals)){
    test_queue_times <- pmax.int(queue_times, arrivals[i])
    new_queue_times <- mapply(next_function, servers, test_queue_times)
    queue <- which.min(new_queue_times)

    queue_times[queue] <- new_queue_times[queue] + service[i]
    output[i] <- queue_times[queue]
    queue_vector[i] <- queue
  }
  return(c(output, queue_vector))
}





#' Compute the queue departure times of customers given a set of arrival times, a set of service times, and a resource schedule.
#'
#'
#' @param arrival_df dataframe with column names ID and times . The ID column is a key
#'     for the customers. The times column is of class \code{numeric} and represents the
#'     arrival times of the customers.
#' @param service vector of service times with the same ordering as arrival_df.
#' @param servers a non-zero natural number, an object of class \code{server.stepfun}
#' or an object of class \code{server.list}.
#' @return A vector of response times for the input of arrival times and service times.
#' @examples
#'
#' # We simulate two queues in series.
#' set.seed(1L)
#' n_customers <- 100
#' arrival_df <- data.frame(ID = c(1:n_customers), times = rlnorm(n_customers, meanlog = 3))
#' service_1 <- rlnorm(n_customers)
#'
#'
#' firstqueue <- queue_step(arrival_df = arrival_df,
#'     servers = 2, service = service_1)
#'
#' server_list <- as.server.stepfun(c(50),c(1,2))
#'
#' service_2 <- rlnorm(n_customers)
#' secondqueue <- queue_step(arrival_df = firstqueue,
#'     servers = server_list, service = service_2)
#'
#' curve(ecdf(arrival_df$times)(x) * n_customers , from = 0, to = 200,
#'     xlab = "time", ylab = "Number of customers")
#' curve(ecdf(firstqueue$times)(x) * n_customers , add = TRUE, col = "red")
#' curve(ecdf(secondqueue$times)(x) * n_customers, add = TRUE, col = "blue")
#' legend(100,40, legend = c("Customer input - arrivals",
#'     "Customer output - firstqueue",
#'     "Customer output - secondqueue"),
#'     col = c("black","red","blue"), lwd = 1, cex = 0.8
#' )
#'
#'summary(firstqueue)
#'summary(secondqueue)
#' @seealso \code{\link{wait_step}}, \code{\link{lag_step}}, \code{\link{as.server.list}}, \code{\link{as.server.stepfun}}
#' @export
queue_step <- function(arrival_df, service, servers = 1){

  output <- queue(arrivals = arrival_df$times, service = service, servers = servers, serveroutput = TRUE)

  output_df <- data.frame(ID = arrival_df$ID, times = as.numeric(output))
  attr(output_df, "server") <- attr(output, "server")
  attr(output_df, "arrival_df") = arrival_df
  attr(output_df, "service") = service
  attr(output_df, "servers_input") = servers

  class(output_df) <- c("queue_df", "data.frame")

  return(output_df)


}

#' Add lag to vector of arrival times.
#' @param arrival_df A dataframe with column names ID and times . The ID column is a key
#'     for the customers. The times column is of class \code{numeric} and represents the
#'     arrival times of the customers.
#' @param service A vector of service times with the same ordering as arrival_df.
#' @return A vector of response times for the input of arrival times and service times.
#' @examples
#' # Create arrival times
#' arrival_df <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 3))
#'
#' # Create service times
#' service <- rlnorm(100)
#' lag_step(arrival_df = arrival_df, service = service)
#'
#' # lag_step is equivalent to queue_step with a large number of queues, but it's faster to compute.
#'
#' cbind(queue_step(arrival_df = arrival_df, service = service, servers = 100),
#' lag_step(arrival_df = arrival_df, service = service))
#' @seealso \code{\link{wait_step}}, \code{\link{queue_step}}.
#' @export
lag_step <- function(arrival_df, service){

  # Add service time to arrival_vector
  output_df <- data.frame(ID = arrival_df$ID, times = arrival_df$times + service)
  return(output_df)
}

#' Compute maximum time for each row from two vectors of arrival times.
#' @param arrival_df A dataframe with column names ID and times . The ID column is a key
#'     for the customers. The times column is of class \code{numeric} and represents the
#'     arrival times of the customers.
#' @param service A vector of times which represent the arrival times of the second type
#'  of customers. The ordering of this vector should have the same ordering as \code{arrival_df}.
#' @return The maximum time from two vectors of arrival times.
#' @details A good real-world example of this is finding the departure times for passengers
#'  after they pick up their bags from the baggage carosel. The time at which they leave is
#'  the maximum of the passenger and bag arrival times.
#' @examples
#'set.seed(500)
#'arrivals <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 4))
#'service <- rlnorm(100)
#'
#'#Airport example ------------------------
#'
#'# Create a number of bags for each of 100 customers
#'bags <- rpois(100,1)
#'
#'# Create a bags dataframe, with each bag associated with one customer.
#'bags.df <- data.frame(BagID = 1:sum(bags),
#'    ID = rep(1:100, bags), times = rlnorm(sum(bags), meanlog = 2))
#'
#'# Create a function which will return the maximum time from each customer's set of bags.
#'
#'reduce_bags <- function(bagdataset, number_of_passengers){
#'    ID = NULL
#'    times = NULL
#'
#'    zerobags <- data.frame(BagID = NA, ID = c(1:number_of_passengers), times = 0)
#'    reduced_df <- as.data.frame(dplyr::summarise(dplyr::group_by(
#'    rbind(bagdataset, zerobags), ID), n = max(times, 0)))
#'    ord <- order(reduced_df$ID)
#'    reduced_df <- reduced_df[order(ord),]
#'    names(reduced_df) <- c("ID", "times")
#'    return(reduced_df)
#'}
#'
#'
#'arrivals2 <- reduce_bags(bags.df, 100)$times
#'
#'# Find the time when customers can leave with their bags.
#'wait_step(arrival_df = arrivals, service = arrivals2)
#' @seealso \code{\link{lag_step}}, \code{\link{queue_step}}.
#' @export
wait_step <- function(arrival_df, service){

  times <- pmax.int(arrival_df$times, service)
  output_df <- data.frame(ID = arrival_df$ID, times = times)
  return(output_df)
}




