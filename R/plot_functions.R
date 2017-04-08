

#' plot method
#' @export
#' @param x an object of class \code{queue_list}
plot.queue_list <- function(x){

  print(plot_departures(x$departures_df))
  print(plot_queues(x$queuelength_df))

}

plot_departures <- function(departures_df){
  require(ggplot2)

  melted <- queue_obj$departures_df %>% select(arrivals, departures) %>% reshape2::melt()

  ggplot(melted) + aes(x = value, colour = variable) + geom_density() + xlab("Time")
}

plot_queues <- function(queuelength_df){
  require(ggplot2)

  ggplot(queuelength_df) + aes(x = times, y = queuelength) + geom_step() + ylab("Queue length") + xlab("Time")
}
