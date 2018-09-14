
# Utilization step function


#' Performance data
#' @noRd
#' @param arrivals arrivals
#' @param service service
#' @param departures departures
#' @param servers server object
#' @param epsilon epsilon
#' @examples
#' n <- 300
#' arrivals <- cumsum(rexp(n, 2.8))
#' service <- rexp(n)
#' x <- c(10, 40, 100)
#' y <- c(1, 5, 2, 5)
#' queue_obj <- queue_step(arrivals, service, servers = as.server.stepfun(x,y))
#'
#' output <- performance_output(arrivals, service, queue_obj$departures, queue_obj$servers_input)
#'
#' plot(output$busy_df, type = "l")
#' plot(output$util_df, type = "l")
performance_output <- function(arrivals, service = 0, departures, servers, epsilon = 1e-10 ){

  systemlength_df <- queue_lengths(arrivals, service = 0, departures, epsilon)
  #queuelength_df <- queue_lengths(arrivals, service, departures, epsilon)

  if("server.stepfun" %in% class(servers)){
    servers <- stats::stepfun(servers$x, servers$y)
  }

  if(is.numeric(servers)){
    stopifnot(length(servers) == 1)
    servers <- stats::stepfun(1, c(servers, servers))
  }

  output <- busy_util_stepfun(systemlength_df, servers, return_stepfun = FALSE, shorten = FALSE)

  return(output)
}

p_busy <- function(arrivals, service, departures, epsilon = 1e-10, servers){
  output <- performance_output(arrivals, service, departures, epsilon, servers)

  return(output$busy_df)
}

p_util <- function(arrivals, service, departures, epsilon = 1e-10, servers){
  output <- performance_output(arrivals, service, departures, epsilon, servers)

  return(output$util_df)
}


#' busy server stepfun (and server utilisation)
#' @noRd
#' @param systemlength_df systemlength_df from queue_step
#' @param servers server stepfunction
#' @param return_stepfun return performance measures as stepfunction or alternatively as dataframes
#' @param shorten should the stepfunctions be shortened? Conceptually this should always be true but we're having problems with it at the moment.
#' @examples
#' n <- 100
#' arrivals <- cumsum(rexp(n, 2.5))
#' service <- rexp(n)
#' x <- c(10, 40)
#' y <- c(1, 2, 1)
#' queue_obj <- queue_step(arrivals, service, servers = as.server.stepfun(x,y))
#'
#' #output <- busy_util_stepfun(queue_obj$systemlength_df, stats::stepfun(x,y))
#'
#' #plot(output$busy_stepfun, type = "l")
#' #plot(output$util_stepfun, type = "l")
busy_util_stepfun <- function(systemlength_df, servers, return_stepfun = TRUE, shorten = TRUE){

  sys_length_stepfun <- stats::stepfun(systemlength_df$times[-1], systemlength_df$queuelength)

  x_new <- unique(sort(c(stats::knots(sys_length_stepfun), stats::knots(servers))))
  y_new_busy <- rep(NA, length(x_new))
  y_new_util <- rep(NA, length(x_new))

  for(i in 1:length(x_new)){
    sys_len <- sys_length_stepfun(x_new[i])
    sys_k <- servers(x_new[i])

    y_new_busy[i] <- min(sys_len, sys_k)
    y_new_util[i] <- y_new_busy[i] / max(sys_k, 1)
  }

  #keep_busy <- !duplicated(y_new_busy)
  #keep_util <- !duplicated(y_new_util)

  if(shorten){
    keep_busy <- c(1, diff(y_new_busy)) != 0
    keep_util <- c(1, diff(y_new_util)) != 0

    x_new_busy <- x_new[keep_busy]
    x_new_util <- x_new[keep_util]

    y_new_busy <- y_new_busy[keep_busy]
    y_new_util <- y_new_util[keep_util]
  } else {
    x_new_busy <- x_new
    x_new_util <- x_new
  }

  if(return_stepfun == TRUE){

    output <- list(
      busy_stepfun = stats::stepfun(x_new_busy, c(0, y_new_busy)),
      util_stepfun = stats::stepfun(x_new_util, c(0, y_new_util))
    )

  } else {

    output <- list(
      busy_df = data.frame(
        times = c(0, x_new_busy),
        value = c(0, y_new_busy)
      ),
      util_df = data.frame(
        times = c(0, x_new_util),
        value = c(0, y_new_util)
      )
    )

  }

  return(output)
}



