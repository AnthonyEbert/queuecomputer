
#' Compute queue lengths from arrival, service and departure data
#' @export
#' @importFrom dplyr %>%
#' @importFrom stats rexp
#' @param arrivals vector of arrival times
#' @param service vector of service times. Leave as zero if you want to compute the number of customers in the system rather than queue length.
#' @param departures vector of departure times
#' @param epsilon numeric small number added to departures to prevent negative queue lengths
#' @examples
#' library(dplyr)
#' library(queuecomputer)
#'
#' set.seed(1L)
#' n_customers <- 100
#'
#' queueoutput_df <- data.frame(
#'   arrivals = runif(n_customers, 0, 300),
#'   service = rexp(n_customers)
#' )
#'
#' queueoutput_df <- queueoutput_df %>% mutate(
#'   departures = queue(arrivals, service, servers = 2)
#' )
#'
#' queue_lengths(
#'   queueoutput_df$arrivals,
#'   queueoutput_df$service,
#'   queueoutput_df$departures
#' )
#'
#' # The dplyr way
#' queueoutput_df %>% do(
#'   queue_lengths(.$arrivals, .$service, .$departures))
#'
#' n_customers <- 1000
#'
#' queueoutput_df <- data.frame(
#'   arrivals = runif(n_customers, 0, 300),
#'   service = rexp(n_customers),
#'   route = sample(c("a", "b"), n_customers, TRUE)
#' )
#'
#' server_df <- data.frame(
#'   route = c("a", "b"),
#'   servers = c(2, 3)
#' )
#'
#' output <- queueoutput_df %>%
#'   left_join(server_df) %>%
#'   group_by(route) %>%
#'   mutate(
#'     departures = queue(arrivals, service, servers = servers[1])
#'   ) %>%
#'   do(queue_lengths(.$arrivals, .$service, .$departures))
#'
#'
#' if(require(ggplot2, quietly = TRUE)){
#'     ggplot(output) +
#'       aes(x = times, y = queuelength) + geom_step() +
#'       facet_grid(~route)
#' }
queue_lengths <- function(arrivals, service = 0, departures, epsilon = 1e-10){

  if(length(service) == 1){
    stopifnot(service == 0)
    check_queueinput(arrivals, service = departures)
  } else {
    check_queueinput(arrivals, service, departures)
  }

  qd_times = c(0, arrivals, departures - service + epsilon)
  qd_state = c(0L, rep.int(1L, length(arrivals)), rep.int(-1L, length(arrivals)))

  out <- sort.int(qd_times, index.return = TRUE)

  queuedata <- data.frame(
    times = out$x,
    queuelength = cumsum(.subset(qd_state, out$ix))
  )

  return(queuedata)

}

queue_lengths_old <- function(arrivals, service = 0, departures){

  if(length(service) == 1){
    stopifnot(service == 0)
    check_queueinput(arrivals, service = departures)
  } else {
    check_queueinput(arrivals, service, departures)
  }

  queuedata <- data.frame(
    times = c(0, arrivals, departures - service),
    state = c(0L, rep.int(1L, length(arrivals)), rep.int(-1L, length(arrivals))
    ))

  ord <- order(queuedata$times, queuedata$state * -1L, method = "radix")

  queuedata <- queuedata[ord, ]

  queuedata$queuelength <- cumsum(queuedata$state)

  queuedata <- queuedata[c("times", "queuelength")]

  return(queuedata)

}

#' Compute time average queue length
#' @param times numeric vector of times
#' @param queuelength numeric vector of queue lengths
#' @examples
#' n <- 1e3
#' arrivals <- cumsum(rexp(n))
#' service <- rexp(n)
#' departures <- queue(arrivals, service, 1)
#'
#' queuedata <- queue_lengths(arrivals, service, departures)
#' average_queue(queuedata$times, queuedata$queuelength)
#' @export
average_queue <- function(times, queuelength){
  ((c(diff(times),0) %*% queuelength) / (times[length(times)] - times[1])) %>% as.numeric()
}

#' Summarise queue lengths
#' @param times numeric vector of times
#' @param queuelength numeric vector of queue lengths
#' @examples
#' n <- 1e3
#' arrivals <- cumsum(rexp(n))
#' service <- rexp(n)
#' departures <- queue(arrivals, service, 1)
#'
#' queuedata <- queue_lengths(arrivals, service, departures)
#' ql_summary(queuedata$times, queuedata$queuelength)
#' @export
ql_summary <- function(times, queuelength){
  n <- length(times)

  x <- dplyr::data_frame(
    times = times, queuelength = queuelength
  )

  diff_times <- NULL
  proportion <- NULL
  # summary_queue <- NULL
  # queue_df

  x <- x %>%
    dplyr::mutate(diff_times = c(diff(times), 0)) %>%
    dplyr::group_by(queuelength) %>%
    dplyr::summarise(proportion = sum(diff_times)) %>%
    dplyr::ungroup()

  tot <- sum(x$proportion)
  x$proportion <- x$proportion / tot

  return(x)
}


# summary_details <- function(arrivals, service, departures){
#   serviced_customers = is.finite(queue_df$times)
#   sc <- serviced_customers
#
#   response_time <- departures[sc] - arrivals[sc]
#   waiting_time <- departures[sc] - service[sc] - arrivals[sc]
#
#   mean_response_time <- mean(response_time)
#   mean_waiting_time <- mean(waiting_time)
# }


#' Creates batches of customer arrivals from a dataframe within a \code{dplyr::do} command
#'
#' @param data a dataframe with parameters for each batch
#' @param arrival_dist a distribution whose support is strictly positive. Either as an object or a non-empty character string. It represents the distribution of arrival times.
#' @param service_rate a strictly positive number representing the rate parameter in the exponential distribution for the service times.
#' @param time a number greater than or equal to zero.
#' @export
#' @examples
#'
#' library(dplyr)
#' flight_schedule <- data_frame(
#' flight = c("F1", "F2"),
#' time = c(0, 50),
#' n = c(100, 100),
#' shape = c(5, 5),
#' rate = c(1 , 1),
#' log_mu = c(1 , 1)
#' )
#'
#' passenger_df <- flight_schedule %>% group_by(flight) %>%
#' do(create_batches(., arrival_dist = "rgamma",
#'     service_rate = 0.4,
#'     time = .$time)
#' )
#'
#' queue_obj <- with(passenger_df,
#'     queue_step(arrivals, service, servers = 5)
#' )
#' if(require(ggplot2, quietly = TRUE)){
#'     plot(queue_obj)
#' }
#'
create_batches <- function(data, arrival_dist, service_rate = NULL, time = 0){

  output_df <- dplyr::data_frame(arrivals = time + do_func_ignore_things(data, arrival_dist))

  if(is.null(service_rate) == FALSE){
    output_df <- output_df %>% dplyr::mutate(
      service = stats::rexp(data$n, service_rate)
    )
  }
  return(output_df)
}







