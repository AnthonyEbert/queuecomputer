
# Utilization step function

#' busy server stepfun (and server utilisation)
#' @param systemlength_df systemlength_df from queue_step
#' @param servers server stepfunction
#' @export
#' @examples
#' n <- 100
#' arrivals <- cumsum(rexp(n, 2.5))
#' service <- rexp(n)
#' x <- c(10, 40)
#' y <- c(1, 2, 1)
#' queue_obj <- queue_step(arrivals, service, servers = as.server.stepfun(x,y))
#'
#' output <- busy_server_stepfun(queue_obj$systemlength_df, stepfun(x,y))
#'
#' plot(output$busy_stepfun)
#' plot(output$util_stepfun)
busy_server_stepfun <- function(systemlength_df, servers){

  sys_length_stepfun <- stepfun(systemlength_df$times[-1], systemlength_df$queuelength)

  x_new <- unique(sort(c(knots(sys_length_stepfun), knots(servers))))
  y_new_busy <- rep(NA, length(x_new))
  y_new_util <- rep(NA, length(x_new))

  for(i in 1:length(x_new)){
    sys_len <- sys_length_stepfun(x_new[i])
    sys_k <- servers(x_new[i])

    y_new_busy[i] <- min(sys_len, sys_k)
    y_new_util[i] <- y_new_busy[i] / sys_k
  }

  #keep_busy <- !duplicated(y_new_busy)
  #keep_util <- !duplicated(y_new_util)

  keep_busy <- c(1, diff(y_new_busy)) != 0
  keep_util <- c(1, diff(y_new_util)) != 0

  x_new_busy <- x_new[keep_busy]
  x_new_util <- x_new[keep_util]

  y_new_busy <- y_new_busy[keep_busy]
  y_new_util <- y_new_util[keep_util]

  output <- list(
    busy_stepfun = stepfun(x_new_busy, c(0, y_new_busy)),
    util_stepfun = stepfun(x_new_util, c(0, y_new_util))
  )

  return(output)
}



