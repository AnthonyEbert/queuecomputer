

#' Compute resource statistics from queue_df object
#'
#' @export
summary.queue_df <- function(queue_df){
  arrival_df = attr(queue_df, "input")

  response_time <- queue_df$times - arrival_df$times
  waiting_time <- queue_df$times - arrival_df$service - arrival_df$times

  mean_response_time <- mean(response_time)
  mean_waiting_time <- mean(waiting_time)

  n_customers <- dim(queue_df)[1]

  system_lengths <- ((ecdf(arrival_df$times)(seq(0, max(queue_df$times), by = 0.1)) - ecdf(queue_df$times)(seq(0, max(queue_df$times), by = 0.1)) ) * n_customers) %>% round()

  summarised_lengths <- (summary(system_lengths %>% as.factor)/(length(seq(0, max(queue_df$times), by = 0.1))))

  output <- list(mrt = mean_response_time, mwt = mean_waiting_time, summarised_lengths = summarised_lengths)
  attr(output, "data") <- data.frame(response_time = response_time, waiting_time = waiting_time)

  class(output) <- c("summary.queue_df", "list")

  return(output)
}

#' @export
print.summary.queue_df <- function(x){
  cat("\nMean Waiting Time:\n", paste(signif(x$mwt)))
  cat("\nMean Response Time:\n", paste(signif(x$mrt)))
}

#' @export
plot.queue_df <- function(queue_df, xlab = "time", ylab = "Number of customers"){
  arrival_df = attr(queue_df, "input")

  n_customers <- dim(queue_df)[1]

  seq_plot <- (seq(0, max(queue_df$times), length.out = 1000))

  system_lengths <- ((ecdf(arrival_df$times)(seq_plot) - ecdf(queue_df$times)(seq_plot) ) * n_customers) %>% round()

  head(system_lengths)

  queue_lengths <- ((ecdf(arrival_df$times)(seq_plot) - ecdf(queue_df$times - arrival_df$service)(seq_plot) ) * n_customers) %>% round()

  par(mfrow = c(2,1))

  curve(ecdf(arrival_df$times)(x) * n_customers , from = 0, to = max(queue_df$times),
    xlab = xlab, ylab = ylab, main = "")
  curve(ecdf(firstqueue$times)(x) * n_customers , add = TRUE, col = "red")

  plot(seq_plot, system_lengths, type = "s", xlab = xlab, ylab = ylab )
  lines(seq_plot, queue_lengths, type = "s", col = "blue")

}




