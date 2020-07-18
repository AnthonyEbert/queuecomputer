

#' ggplot2 method for output from queueing model
#' @importFrom dplyr row_number
#' @export
#' @param x an object of class \code{queue_list}
#' @param which Numeric vector of integers from 1 to 6 which represents which plots are to be created. See examples.
#' @param annotated logical, if \code{TRUE} annotations will be added to the plot.
#' @param ... other parameters to be passed through to plotting functions.
#' @examples
#' if(require(ggplot2, quietly = TRUE)){
#'
#' n_customers <- 50
#' arrival_rate <- 1.8
#' service_rate <- 1
#' arrivals <- cumsum(rexp(n_customers, arrival_rate))
#' service <- rexp(n_customers, service_rate)
#' queue_obj <- queue_step(arrivals, service, servers = 2)
#' plot(queue_obj)
#'
#' }
#'
#' \dontrun{
#'
#' library(ggplot2)
#'
#' ## density plots of arrival and departure times
#' plot(queue_obj, which = 1)
#'
#' ## histograms of arrival and departure times
#' plot(queue_obj, which = 2)
#'
#' ## density plots of waiting and system times
#' plot(queue_obj, which = 3)
#'
#' ## step function of queue length
#' plot(queue_obj, which = 4)
#'
#' ## line range plot of customer and server status
#' plot(queue_obj, which = 5)
#'
#' ## empirical distribution plot of arrival and departure times
#' plot(queue_obj, which = 6)
#'
#' }
#'
plot.queue_list <- function(x, which = c(2:6), annotated = TRUE, ...){

  if(requireNamespace("ggplot2", quietly = TRUE)){

    stopifnot(is.numeric(which) & all(which > 0) & which <= 6 & all(which %% 1 == 0))

    output <- lapply(which, function(i){
      suppressMessages(plot_loop(x, i, annotated = annotated))
    })

    return(output)
  } else {
    warning('ggplot2 package required for this function')
  }
}


plot_loop <- function(x, i, annotated){

  switch(i,
         plot_departures(x, annotated),
         plot_dep_histogram(x, annotated),
         plot_waiting(x, annotated),
         plot_queues(x, annotated),
         plot_status(x, annotated),
         plot_empiricaldist(x, annotated)

  )
}


plot_departures <- function(x, annotated){

  value <- NULL
  key <- NULL

  departures_df <- x$departures_df

  melted <- departures_df %>% dplyr::select(arrivals, departures) %>% tidyr::gather()
  output <- ggplot2::ggplot(melted) + ggplot2::aes(x = value, colour = key) + ggplot2::geom_density()

  edited_output <- output + ggplot2::xlab("Time") +
    ggplot2::ggtitle("Density plot of arrival and departure times")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}

plot_dep_histogram <- function(x, annotated){

  value <- NULL

  departures_df <- x$departures_df

  melted <- departures_df %>% dplyr::select(arrivals, departures) %>% tidyr::gather()
  output <- ggplot2::ggplot(melted) + ggplot2::aes(x = value) + ggplot2::geom_histogram(bins = 10, color = "black") + ggplot2::facet_grid(.~key)
  edited_output <- output + ggplot2::xlab("Time") +
    ggplot2::ggtitle("Histogram of arrival and departure times")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}

plot_waiting <- function(x, annotated){

  value <- key <- NULL
  departures_df <- x$departures_df

  melted <- departures_df %>% dplyr::select(waiting, system_time) %>% tidyr::gather()
  output <- ggplot2::ggplot(melted) + ggplot2::aes(x = value, colour = key) + ggplot2::geom_density()
  edited_output <- output + ggplot2::xlab("Time") +
    ggplot2::ggtitle("Density plot of waiting and system times")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}

plot_queues <- function(x, annotated){

  times <- queuelength <- type <- NULL
  queuelength_df <- x$queuelength_df
  systemlength_df <- x$systemlength_df

  queuelength_df$type = "customers in queue"
  systemlength_df$type = "customers in entire system"

  input <- dplyr::bind_rows(queuelength_df, systemlength_df)

  output <- ggplot2::ggplot(input) + ggplot2::aes(x = times, y = queuelength, colour = type) + ggplot2::geom_step()
  edited_output <- output + ggplot2::ylab("Number of customers") + ggplot2::xlab("Time") +
    ggplot2::ggtitle("Step function plot of customers in queue and system")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}

plot_status <- function(x, annotated){

  departures <- service <- arrivals <- type <- NULL
  start_service <- server <- ymin <- ymax <- status <- NULL

  departures_df <- x$departures_df

  departures_df <- departures_df %>% dplyr::mutate(
    start_service = departures - service
  )

  start_df <- departures_df %>%
    dplyr::transmute(
      x = row_number(),
      ymin = arrivals,
      ymax = start_service,
      type = "customers",
      status = "waiting"
    )

  end_df <- departures_df %>%
    dplyr::transmute(
      x = row_number(),
      ymin = start_service,
      ymax = departures,
      type = "customers",
      status = "service"
    )

  server_df <- departures_df %>%
    dplyr::transmute(
      x = server,
      ymin = start_service,
      ymax = departures,
      type = "servers",
      status = "service"
    )

  tidydata_for_line <- dplyr::bind_rows(start_df, end_df, server_df)

  output <- ggplot2::ggplot(tidydata_for_line) +
    ggplot2::aes(x = x, ymin = ymin, ymax = ymax, col = status) +
    ggplot2::geom_linerange()

  edited_output <- output +
    ggplot2::ylab("Time") + ggplot2::facet_grid(.~type, scales = "free_x") + ggplot2::scale_x_discrete() + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank()) +
    ggplot2::ggtitle("Line range plot of customer and server status")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))

}

plot_empiricaldist <- function(x, annotated){

  value <- key <- NULL
  departures_df <- x$departures_df

  melted <- departures_df %>% dplyr::select(arrivals, departures) %>% tidyr::gather()

  output <- ggplot2::ggplot(melted) + ggplot2::aes(x = value, colour = key) + ggplot2::stat_ecdf()

  edited_output <- output + ggplot2::xlab("Time") + ggplot2::ylab("Empirical cumulative distribution function") +
    ggplot2::ggtitle("empirical distribution plot of arrival and departure times")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}






