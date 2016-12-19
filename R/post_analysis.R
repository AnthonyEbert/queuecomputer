
#' Compute resource statistics from queue_df object
#' @param object an object of class \code{queue_df}, the result of a call to \code{queue_step}.
#' @param ... futher arguments to be passed to or from other methods.
#' @return A list of performance statistics for the queue. "Mean waiting time": The mean time each customer had to wait in queue for service. "Mean response time": The mean time that each customer spends in the system (departure time - arrival time). "Utilization factor": The ratio of available time for all servers and time all servers were used. It can be greater than one if a customer arrives near the end of a shift and keeps a server busy. "Queue Lengths" and "System Lengths", the proportion of time for each queue length or number of customers in the system.
#' @examples
#' set.seed(1L)
#' n_customers <- 100
#' arrival_df <- data.frame(ID = c(1:n_customers), times = rlnorm(n_customers, meanlog = 3))
#' service <- rexp(n_customers)
#' queue <- queue_step(arrival_df = arrival_df, service = service)
#' summary(queue)
#'
#' @export
summary.queue_df <- function(object, ...){

  queue_df = object

  stopifnot(all(queue_df$times == Inf) == FALSE)

  arrival_df = attr(queue_df, "arrival_df")
  arrival_df$service = attr(queue_df, "service")

  serviced_customers = is.finite(queue_df$times)

  response_time <- queue_df$times - arrival_df$times
  waiting_time <- queue_df$times - arrival_df$service - arrival_df$times

  mean_response_time <- mean(response_time[serviced_customers])
  mean_waiting_time <- mean(waiting_time[serviced_customers])

  n_customers <- dim(queue_df)[1]

  max_time <- max(queue_df$times[serviced_customers])

  queue_lengths <- round((stats::ecdf(arrival_df$times)(seq(0, max_time, by = 0.1)) - stats::ecdf(queue_df$times - arrival_df$service)(seq(0, max_time, by = 0.1)) ) * n_customers)

  system_lengths <- round((stats::ecdf(arrival_df$times)(seq(0, max_time, by = 0.1)) - stats::ecdf(queue_df$times)(seq(0, max_time, by = 0.1)) ) * n_customers)

  queue_lengths <- (summary(as.factor(queue_lengths))/(length(seq(0, max_time, by = 0.1))))
  system_lengths <- (summary(as.factor(system_lengths))/(length(seq(0, max_time, by = 0.1))))

  if("server.stepfun" %in% class(attr(queue_df, "servers_input"))){

    x = attr(queue_df, "servers_input")$x
    y = attr(queue_df, "servers_input")$y

    if(y[length(x)] == 0){
      last_server_time = x[length(x)]
    } else {
      last_server_time = 0
    }

    service_available <- integrate_stepfun(x = x, y = y, last = max(max_time, last_server_time))

    service_rendered <- sum(arrival_df$service[serviced_customers])
    utilization <- service_rendered/service_available
  }

  if("numeric" %in% class(attr(queue_df, "servers_input"))){

    service_available <- integrate_stepfun(x = c(1), y = c(attr(queue_df, "servers_input"), attr(queue_df, "servers_input")),
      last = max_time)

    service_rendered <- sum(arrival_df$service[serviced_customers])
    utilization <- service_rendered/service_available
  }

  if("server.list" %in% class(attr(queue_df, "servers_input"))){
    warning("utilization ratio not yet available for queue with server.list input")

    utilization <- NA
  }

  missed_customers <- length(queue_df$times) - length(queue_df$times[serviced_customers])

  output <- list(mc = missed_customers, mrt = mean_response_time, mwt = mean_waiting_time, queue_lengths = queue_lengths,
    system_lengths = system_lengths, utilization = utilization)
  attr(output, "data") <- data.frame(response_time = response_time, waiting_time = waiting_time)

  class(output) <- c("summary_queue_df", "list")

  return(output)
}

#' Print method for output of \code{summary.queue_df}.
#' @param x an object of class \code{summary_queue_df}, the result of a call to \code{summary.queue_df()}.
#' @param ... futher arguments to be passed to or from other methods.
#' @return A list of performance statistics for the queue. "Mean waiting time": The mean time each customer had to wait in queue for service. "Mean response time": The mean time that each customer spends in the system (departure time - arrival time). "Utilization factor": The ratio of available time for all servers and time all servers were used. It can be greater than one if a customer arrives near the end of a shift and keeps a server busy. "Queue Lengths" and "System Lengths", the proportion of time for each queue length or number of customers in the system.
#' @export
print.summary_queue_df <- function(x, ...){
  cat("\nMissed customers:\n", paste(signif(x$mc)))
  cat("\nMean waiting time:\n", paste(signif(x$mwt)))
  cat("\nMean response time:\n", paste(signif(x$mrt)))
  cat("\nUtilization factor:\n", paste(signif(x$utilization, 4)))
  cat("\nQueue Lengths (%):\n")
  print(signif(x$queue_lengths * 100, 3))
  cat("\nSystem Lengths (%):\n")
  print(signif(x$system_lengths * 100, 3))
}

integrate_stepfun <- function(x, y, last = 1000){
  x <- c(0,x,last)
  x_diff <- diff(x)
  return(y %*% x_diff)
}
