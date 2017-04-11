#

#' Compute the departure times for a set of customers in a queue from their arrival and service times.
#' @param arrivals numeric vector of non-negative arrival times
#' @param service numeric vector of non-negative service times
#' @param servers a non-zero natural number, an object of class \code{server.stepfun}
#' or an object of class \code{server.list}.
#' @param serveroutput boolean whether the server used by each customer should be returned.
#' @param adjust non-negative number, an adjustment parameter for scaling the service times.
#' @description \code{queue} is a faster internal version of \code{queue_step}. It is not compatible with the \code{summary.queue_list} method or the \code{plot.queue_list} method.
#' @examples
#' n <- 1e2
#' arrivals <- cumsum(rexp(n, 1.8))
#' service <- rexp(n)
#'
#' departures <- queue(
#'     arrivals, service, servers = 2)
#'
#' head(departures)
#' curve(ecdf(departures)(x) * n,
#'     from = 0, to = max(departures),
#'     xlab = "Time", ylab = "Number of customers")
#' curve(ecdf(arrivals)(x) * n,
#'     from = 0, to = max(departures),
#'     col = "red", add = TRUE)
#'
#' @seealso
#' \code{\link{queue_step}}
#' @useDynLib queuecomputer, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export
queue <- function(arrivals, service, servers = 1, serveroutput = FALSE, adjust = 1){

  service = service * adjust
  check_queueinput(arrivals, service)

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
  return(c(output, queue_vector, NA))
}





#' Compute the departure times and queue lengths for a queueing system from arrival and service times.
#'
#' @param arrivals numeric vector of non-negative arrival times
#' @param service numeric vector of service times with the same ordering as arrival_df.
#' @param servers a non-zero natural number, an object of class \code{server.stepfun}
#' or an object of class \code{server.list}.
#' @param labels character vector of customer labels.
#' @param adjust non-negative number, an adjustment parameter for scaling the service times.
#' @return A vector of response times for the input of arrival times and service times.
#' @examples
#'
#' # With two servers
#' set.seed(1)
#' n <- 100
#'
#' arrivals <- cumsum(rexp(n, 3))
#' service <- rexp(n)
#'
#'
#' queue_obj <- queue_step(arrivals,
#'     service = service, servers = 2)
#'
#'
#' summary(queue_obj)
#' plot(queue_obj, which = 5)
#'
#' # It seems like the customers have a long wait.
#' # Let's put two more servers on after time 20
#'
#'
#' server_list <- as.server.stepfun(c(20),c(2,4))
#'
#' queue_obj2 <- queue_step(arrivals,
#'     service = service,
#'     servers = server_list)
#'
#' summary(queue_obj2)
#' if(require(ggplot2, quietly = TRUE)){
#'
#'     plot(queue_obj2, which = 5)
#'
#' }
#'
#'
#' @seealso
#' \code{\link{queue}}, \code{\link{summary.queue_list}}, \code{\link{plot.queue_list}}
#' @export
queue_step <- function(arrivals, service, servers = 1, labels = NULL, adjust = 1){

  arrivals <- depart(arrivals)

  departures <- queue(arrivals = arrivals, service = service, servers = servers, serveroutput = TRUE, adjust = 1)

  server <- attr(departures, "server")
  attributes(departures) <- NULL

  if(is.null(labels) == FALSE){
    departures_df <- dplyr::data_frame(
      labels = labels,
      arrivals = arrivals,
      service = service,
      departures = departures,
      waiting = departures - arrivals - service,
      system_time = departures - arrivals,
      server = server
    )
  } else {
    departures_df <- dplyr::data_frame(
      arrivals = arrivals,
      service = service,
      departures = departures,
      waiting = departures - arrivals - service,
      system_time = departures - arrivals,
      server = server
    )
  }

  queuelength_df <- queue_lengths(
    arrivals, service, departures
  )

  systemlength_df <- queue_lengths(
    arrivals, departures = departures
  )

  output <- list(
    departures = departures,
    server = server,
    departures_df = departures_df,
    queuelength_df = queuelength_df,
    systemlength_df = systemlength_df,
    servers_input = servers
  )

  class(output) <- c("queue_list", "list")

  return(output)
}



#' Add lag to vector of arrival times.
#' @param arrivals Either a numeric vector or an object of class \code{queue_list}. It represents the arrival times.
#' @param service A vector of service times with the same ordering as arrivals
#' @return A vector of response times for the input of arrival times and service times.
#' @examples
#' # Create arrival times
#' arrivals <- rlnorm(100, meanlog = 3)
#'
#' # Create service times
#' service <- rlnorm(100)
#' lag_step(arrivals = arrivals, service = service)
#'
#' # lag_step is equivalent to queue_step with a large number of queues, but it's faster to compute.
#'
#' cbind(queue(arrivals, service = service, servers = 100),
#' lag_step(arrivals = arrivals, service = service))
#' @seealso \code{\link{wait_step}}, \code{\link{queue_step}}.
#' @export
lag_step <- function(arrivals, service){

  arrivals <- depart(arrivals)

  output  <- arrivals + service

  return(output)
}






#' Compute maximum time for each row from two vectors of arrival times.
#' @param arrivals Either a numeric vector or an object of class \code{queue_list}. It represents the arrival times.
#' @param service A vector of times which represent the arrival times of the second type
#'  of customers. The ordering of this vector should have the same ordering as \code{arrivals}.
#' @return The maximum time from two vectors of arrival times.
#' @details A good real-world example of this is finding the departure times for passengers
#'  after they pick up their bags from the baggage carosel. The time at which they leave is
#'  the maximum of the passenger and bag arrival times.
#' @examples
#'set.seed(500)
#'arrivals <- rlnorm(100, meanlog = 4)
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
#'wait_step(arrivals = arrivals, service = arrivals2)
#' @seealso \code{\link{lag_step}}, \code{\link{queue_step}}.
#' @export
wait_step <- function(arrivals, service){

  arrivals <- depart(arrivals)

  output <- pmax.int(arrivals, service)
  return(output)
}



#' get departure times from \code{queue_list} object
#' @export
#' @param x an \code{queue_list} object
#' @return departure times
#' @examples
#' arrivals <- cumsum(rexp(10))
#' service <- rexp(10)
#' queue_obj <- queue_step(arrivals, service)
#'
#' depart(queue_obj)
#' queue_obj$departures_df$departures
depart <- function(x){
  if("numeric" %in% class(x)){
    departures <- x
  } else {
    departures <- x$departures_df$departures
  }
  return(departures)
}

