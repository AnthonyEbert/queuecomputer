




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

  return(output_list)

}



















