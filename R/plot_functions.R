

#' ggplot method for output from queueing model
#' @export
#' @param x an object of class \code{queue_list}
#' @param which Numeric vector of integers from 1 to 7 which represents which plots are to be created. See \link{examples}
#' @param annotated logical, if \code{TRUE} annotations will be added to the plot
#' @examples
#' if(require(ggplot2, quietly = TRUE)){
#'     n_customers <- 50
#'     arrival_rate <- 1.8
#'     service_rate <- 1
#'     arrivals <- cumsum(rexp(n_customers, arrival_rate))
#'     service <- rexp(n_customers, service_rate)
#'     queue_obj <- QDC(arrivals, service, servers = 2)
#'     plot(queue_obj)
#' }
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
plot.queue_list <- function(x, which = c(2:6), annotated = TRUE, ...){

  assign(names(x$departures_df, x$queue), NULL)
  ymin <- NULL
  ymax <- NULL

  if(requireNamespace("ggplot2", quietly = TRUE)){
    loadNamespace

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
         plot_departures(x$departures_df, annotated),
         plot_dep_histogram(x$departures_df, annotated),
         plot_waiting(x$departures_df, annotated),
         plot_queues(x$queuelength_df, annotated),
         plot_status(x$departures_df, annotated),
         plot_empiricaldist(x$departures_df, annotated)

  )
}


plot_departures <- function(x, annotated){

  melted <- departures_df %>% dplyr::select(arrivals, departures) %>% reshape2::melt()
  output <- ggplot2::ggplot(melted) + ggplot2::aes(x = value, colour = variable) + ggplot2::geom_density()

  edited_output <- output + ggplot2::xlab("Time") +
    ggplot2::ggtitle("Density plot of arrival and departure times")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}

plot_dep_histogram <- function(x, annotated){
  melted <- departures_df %>% dplyr::select(arrivals, departures) %>% reshape2::melt()
  output <- ggplot2::ggplot(melted) + ggplot2::aes(x = value) + ggplot2::geom_histogram(bins = 10, color = "black") + ggplot2::facet_grid(.~variable)
  edited_output <- output + ggplot2::xlab("Time") +
    ggplot2::ggtitle("Histogram of arrival and departure times")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}

plot_waiting <- function(x, annotated){
  melted <- departures_df %>% dplyr::select(waiting, system_time) %>% reshape2::melt()
  output <- ggplot2::ggplot(melted) + ggplot2::aes(x = value, colour = variable) + ggplot2::geom_density()
  edited_output <- output + ggplot2::xlab("Time") +
    ggplot2::ggtitle("Density plot of waiting and system times")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}

plot_queues <- function(x, annotated){

  output <- ggplot2::ggplot(queuelength_df) + ggplot2::aes(x = times, y = queuelength) + ggplot2::geom_step()
  edited_output <- output + ggplot2::ylab("Queue length") + ggplot2::xlab("Time") +
    ggplot2::ggtitle("Step function plot of queue length")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}

# plot_aw <- function(departures_df){
#   output <- ggplot2::ggplot(departures_df) + ggplot2::aes(x = arrivals, y = waiting) + ggplot2::geom_point()
#   edited_output <- output
#   return(edited_output)
# }

plot_status <- function(x, annotated){

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
      status = paste0("server ", server) %>% factor()
    )

  server_df <- departures_df %>%
    dplyr::transmute(
      x = server,
      ymin = start_service,
      ymax = departures,
      type = "server utilization",
      status = paste0("server ", server) %>% factor()
    )

  tidydata_for_line <- dplyr::bind_rows(start_df, end_df, server_df)

  output <- ggplot2::ggplot(tidydata_for_line) +
    ggplot2::aes(x = x, ymin = ymin, ymax = ymax, col = status) +
    ggplot2::geom_linerange()
  edited_output <- output + ggplot2::xlab("customer") +
    ggplot2::ylab("Time") + ggplot2::facet_grid(.~type, scales = "free_x") + ggplot2::scale_x_discrete() + ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::ggtitle("Line range plot of customer and server status")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))

}

plot_empiricaldist <- function(departures_df){
  melted <- departures_df %>% dplyr::select(arrivals, departures) %>% reshape2::melt()

  output <- ggplot2::ggplot(melted) + ggplot2::aes(x = value, colour = variable) + ggplot2::stat_ecdf()

  edited_output <- output + xlab("Time") + ylab("Empirical cumulative distribution function") +
    ggplot2::ggtitle("empirical distribution plot of arrival and departure times")

  switch(as.numeric(annotated) + 1, return(output), return(edited_output))
}






