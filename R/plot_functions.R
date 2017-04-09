

#' plot method
#' @export
#' @param x an object of class \code{queue_list}
#' @examples
#' n_customers <- 1e3
#' arrival_rate <- 0.8
#' service_rate <- 1
#' arrivals <- cumsum(rexp(n_customers, arrival_rate))
#' service <- rexp(n_customers, service_rate)
#' queue_obj <- QDC(arrivals, service, servers = 1)
#' plot(queue_obj)
plot.queue_list <- function(x, server_same = TRUE, which = c(1:4)){
  requireNamespace(ggplot2, quietly = TRUE)

  stopifnot(is.numeric(which) & all(which > 0) & which <= 5)

  output <- lapply(which, function(i, ...){
    suppressMessages(plot_loop(x,i))
  })

  return(output)
}


plot_loop <- function(x, i){

  switch(i,
         plot_departures(x$departures_df),
         plot_waiting(x$departures_df),
         plot_queues(x$queuelength_df),
         plot_status(x$departures_df),
         plot_aw(x$departures_df)
  )
}


plot_departures <- function(departures_df){

  melted <- queue_obj$departures_df %>% dplyr::select(arrivals, departures) %>% reshape2::melt()
  output <- ggplot(melted) + aes(x = value, colour = variable) + geom_density()
  edited_output <- output + xlab("Time")
  return(edited_output)
}

plot_waiting <- function(departures_df){
  melted <- queue_obj$departures_df %>% dplyr::select(waiting, system_time) %>% reshape2::melt()
  output <- ggplot(melted) + aes(x = value, colour = variable) + geom_density()
  edited_output <- output + xlab("Time")
  return(edited_output)
}

plot_queues <- function(queuelength_df){

  output <- ggplot(queuelength_df) + aes(x = times, y = queuelength) + geom_step()
  edited_output <- output + ylab("Queue length") + xlab("Time")
  return(edited_output)
}

plot_aw <- function(departures_df){
  output <- ggplot(departures_df) + aes(x = arrivals, y = waiting) + geom_point()
  edited_output <- output
  return(edited_output)
}

plot_status <- function(departures_df){

  departures_df <- departures_df %>% dplyr::mutate(
    start_service = departures - service
  )

  start_df <- departures_df %>%
    dplyr::transmute(
      x = row_number(),
      ymin = arrivals,
      ymax = start_service,
      status = "waiting"
    )

  end_df <- departures_df %>%
    dplyr::transmute(
      x = row_number(),
      ymin = start_service,
      ymax = departures,
      status = paste0("server ", server) %>% factor()
    )

  tidydata_for_line <- bind_rows(start_df, end_df)

  output <- ggplot(tidydata_for_line) +
    aes(x = x, ymin = ymin, ymax = ymax, col = status) +
    geom_linerange()
  edited_output <- output + theme(axis.title.x=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())
  return(edited_output)

}








