
#' Compute queue lengths from arrival, service and departure data
#' @export
#' @param arrivals vector of arrival times
#' @param service vector of service times
#' @param departure vector of departure times
#' @examples
#' library(ggplot2)
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
#' ggplot(output) +
#'   aes(x = time, y = QueueLength) + geom_step() +
#'   facet_grid(~route)
queue_lengths <- function(arrivals, service = 0, departures, scales = NULL){

  if(length(service) == 1){
    stopifnot(service == 0)
    check_queueinput(arrivals, service = departures)
  } else {
    check_queueinput(arrivals, service, departures)
  }

  queuedata <- data.frame(input = arrivals, output = departures - service + 1e-8) %>% tidyr::gather()

  queuedata$key <- as.factor(queuedata$key)

  state_df <- data.frame(
    key = as.factor(c("input", "output")),
    state = c(1, -1)
  )

  queuedata <- suppressMessages(left_join(queuedata, state_df))

  levels(queuedata$key) <- c("input", "output")

  queuedata <- queuedata %>% arrange(value, key) %>% mutate(
    QueueLength = cumsum(state),
    time = value
  )

  queuedata <- queuedata %>% select(key, time, QueueLength)

  return(queuedata)

}

#' Compute time average queue length
#' @export
average_queue <- function(times, QueueLength){
  times <- c(0, times)
  (diff(times) %*% QueueLength) / length(QueueLength)
}
