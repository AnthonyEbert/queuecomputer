




#' Summary method for queue_list object
#' @export
#' @param object an object of class \code{queue_list}, the result of a call to \code{queue_step}.
#' @param ... futher arguments to be passed to or from other methods.
summary.queue_list <- function(object, ...){

  # Non-standard evaluation binding

  arrivals <- NULL
  service <- NULL
  departures <- NULL
  response <- NULL
  waiting <- NULL
  times <- NULL

  # Number of customers

  n <- length(object$departures_df$departures)

  # Missed customers and initial input

  missed_customers = length(
    which(
      is.infinite(object$departures_df$departures)
    )
  )

  departures_df <- object$departures_df %>%
    dplyr::filter(is.finite(departures))
  queuelength_df <- object$queuelength_df %>%
    dplyr::filter(is.finite(times))
  systemlength_df <- object$systemlength_df %>%
    dplyr::filter(is.finite(times))
  servers_input <- object$servers_input

  # Compute response times and waiting times

  departures_df <- departures_df %>%
    dplyr::mutate(
      response = departures - arrivals,
      waiting = departures - arrivals - service
    )

  departures_df$server <- factor(departures_df$server)
  departures_sum <- summary(departures_df)

  mwt <- mean(departures_df$waiting)
  mrt <- mean(departures_df$response)

  # Summarise queuelengths

  qlength_sum <- ql_summary(
    queuelength_df$times, queuelength_df$queuelength
  )

  slength_sum <- ql_summary(
    systemlength_df$times, systemlength_df$queuelength
  )

  qlength_mean <- average_queue(queuelength_df$times, queuelength_df$queuelength)

  slength_mean <- average_queue(systemlength_df$times, systemlength_df$queuelength)

  # Utilisation

  max_time <- max(departures_df$departures)

  if("numeric" %in% class(object$servers_input)){

    n_server <- object$servers_input

    service_available <- integrate_stepfun(x = c(1), y = c(n_server, n_server),
      last = max_time)

    service_rendered <- sum(departures_df$service)

    utilization <- service_rendered/service_available
  }


  if("server.stepfun" %in% class(object$servers_input)){

    x = object$servers_input$x
    y = object$servers_input$y

    if(y[length(x)] == 0){
      last_server_time = x[length(x)]
    } else {
      last_server_time = 0
    }

    service_available <- integrate_stepfun(x = x, y = y, last = max(max_time, last_server_time))

    service_rendered <- sum(departures_df$service)

    utilization <- service_rendered/service_available
  }

  if("server.list" %in% class(object$servers_input)){

    utilization <- NA

    warning("Not yet implemented for server.list")
  }

  output_list <- list(
    number_customers = n,
    missed_customers = missed_customers,
    qlength_sum = qlength_sum,
    qlength_mean = qlength_mean,
    slength_sum = slength_sum,
    slength_mean = slength_mean,
    mrt = mrt,
    mwt = mwt,
    departures_sum = departures_sum,
    utilization = utilization
  )

  class(output_list) <- c("summary_queue_list", "list")

  return(output_list)

}








#' Print method for output of \code{summary.queue_list}.
#' @param x an object of class \code{summary_queue_list}, the result of a call to \code{summary.queue_list()}.
#' @param ... futher arguments to be passed to or from other methods.
#' @return A list of performance statistics for the queue:
#'
#' "Total customers": Total customers in sinulation,
#'
#' "Missed customers": Customers who never saw a server,
#'
#' "Mean waiting time": The mean time each customer had to wait in queue for service,
#'
#' "Mean response time": The mean time that each customer spends in the system (departure time - arrival time),
#'
#' "Utilization factor": The ratio of available time for all servers and time all servers were used. It can be greater than one if a customer arrives near the end of a shift and keeps a server busy,
#'
#' "Mean queue length": Average queue length, and
#'
#' "Mean number of customers in system": Average number of customers in queue or currently being served.
#' @examples
#' n <- 1e3
#' arrivals <- cumsum(rexp(n, 1.8))
#' service <- rexp(n)
#'
#' queue_obj <- queue_step(arrivals, service, servers = 2)
#' summary(queue_obj)
#' @export
print.summary_queue_list <- function(x, ...){
  sig <- 3

  cat("Total customers:\n", paste(x$n, ...))
  cat("\nMissed customers:\n", paste(x$missed_customers), ...)
  cat("\nMean waiting time:\n", paste(signif(x$mwt, sig)), ...)
  cat("\nMean response time:\n", paste(signif(x$mrt, sig)), ...)
  cat("\nUtilization factor:\n", paste(x$utilization, ...))
  cat("\nMean queue length:\n", paste(signif(x$qlength_mean, sig)), ...)
  cat("\nMean number of customers in system:\n", paste(signif(x$slength_mean, sig)), ...)
}










