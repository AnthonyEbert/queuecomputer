

## Testing Functions

reduce_bags <- function(bagdataset, number_of_passengers){
  ID = NULL
  times = NULL

  zerobags <- data.frame(BagID = NA, ID = c(1:number_of_passengers), times = 0)
  reduced_df <- as.data.frame(dplyr::summarise(dplyr::group_by(rbind(bagdataset, zerobags), ID), n = max(times, 0)))
  ord <- order(reduced_df$ID)
  reduced_df <- reduced_df[order(ord),]
  names(reduced_df) <- c("ID", "times")
  return(reduced_df)
}


next_function <- function(sf,time){
  output <- switch(sf(time) + 1, c(stats::knots(sf),Inf)[findInterval(time,stats::knots(sf)) + 1] , time)
  return(output)
}

#' Create a \code{server.stepfun} object with a roster of times and number of available servers.
#'
#' @param x numeric vector giving the times of changes in number of servers.
#' @param y numeric vector one longer than \code{x} giving the number of servers
#' available between x values.
#' @return A \code{list} and \code{server.stepfun} object with x and y as elements.
#' @details This function uses the analogy of a step function to specify the number of
#' available servers throughout the day. It is used as input to the \code{\link{queue_step}}
#' function. Alternativily one may use \code{as.server.list} to specify available servers as
#' a list, however \code{queue_step} is much faster when \code{as.server.stepfun} is used
#' as input rather than \code{as.server.list}.
#' @details If any of the service times are large compared to any element of \code{diff(x)} then the
#' \code{\link{as.server.list}} function should be used.
#' @seealso \code{\link{as.server.list}}, \code{\link{queue_step}}, \code{\link{stepfun}}.
#' @examples
#'
#' servers <- as.server.stepfun(c(15,30,50), c(0, 1, 3, 2))
#' servers
#'
#' @export
as.server.stepfun <- function(x, y){

  stopifnot(all(y%%1 == 0) & all(y >= 0)) # y's are integers
  stopifnot(!is.unsorted(x))              # x's are ordered
  stopifnot(length(x) >= 1)               # x is not of length zero
  stopifnot(length(y) == length(x) + 1)   # stepfun condition
  stopifnot(sum(y) != 0)                  # There is at least one server
  # stopifnot(y[length(y)] != 0)          # Last y value is not zero (removed)
  stopifnot(all(x >= 0))                  # All x's are positive
  stopifnot(all(is.finite(c(x,y))))       # All x's and y's are finite
  stopifnot(is.numeric(c(x,y)))           # All x's and y's are numeric

  output <- list()
  class(output) <- c("list", "server.stepfun")


  output$x <- x
  output$y <- y

  return(output)
}

server_make <- function(x, y){

  stopifnot(all(y%%1 == 0))
  stopifnot(!is.unsorted(x))


  plateaus <- y
  max_plat <- max(plateaus)
  num_plat <- length(plateaus)

  output <- list()

  intermediate_plateaus <- plateaus
  newplat <- matrix(NA,nrow=max_plat, ncol = num_plat)
  for(i in 1:max_plat){
    for(j in 1:num_plat){
      if(intermediate_plateaus[j] == 0){
        newplat[i,j] <- 0
      }else{
        newplat[i,j] <- 1
        intermediate_plateaus[j] <- intermediate_plateaus[j] - 1
      }
    }

    # Fix for issue 1
    newrow <- rep(TRUE, length(newplat[i,]))
    for(k in 2:length(newplat[i,])){
       if(newplat[i,][k-1] == 0 && newplat[i,][k] == 0){
         newrow[k] <- FALSE
       }
    }

    output[[i]] <- stats::stepfun(x[newrow[-1]],newplat[i,][newrow])
  }
  class(output) <- c("list", "server.list")

  return(output)
}

#' Creates a \code{"server.list"} object from a list of times and starting availability.
#'
#' @param times list of numeric vectors giving change times for each server.
#' @param init vector of 1s and 0s with equal length to \code{times}.
#' It represents whether the server starts in an availabile \code{(1)} or unavailable \code{(0)} state.
#' @return an object of class \code{"server.list"}, which is a list of step functions of range \{0, 1\}.
#' @seealso \code{\link{as.server.stepfun}}, \code{\link{queue_step}}
#' @examples
#' # Create a server.list object with the first server available anytime before time 10,
#' # and the second server available between time 15 and time 30.
#' as.server.list(list(10, c(15,30)), c(1,0))
#' @export
as.server.list <- function(times, init){

  stopifnot(class(times) == "list")
  stopifnot(all(init %in% c(1,0)))
  stopifnot(length(times) == length(init))

  n <- length(times)

  output <- list()

  for(i in 1:n){
    switch(init[i] + 1,
      y <- rep(c(0,1), length.out = length(times[[i]]) + 1),
      y <- rep(c(1,0), length.out = length(times[[i]]) + 1)
      )
    output[[i]] <- stats::stepfun(times[[i]], y)
  }

  class(output) <- c("list", "server.list")
  return(output)

}





do_func_ignore_things <- function(data, what){
  acceptable_args <- data[names(data) %in% (formals(what) %>% names)]

  do.call(what, acceptable_args %>% as.list)
}



check_queueinput <- function(arrivals, service, departures = NULL){
  stopifnot(all(service >= 0))
  stopifnot(all(arrivals >= 0))
  stopifnot(length(arrivals) == length(service))
  #stopifnot(anyNA(c(arrivals, service)) == FALSE )
  stopifnot(is.numeric(arrivals))
  stopifnot(is.numeric(service))

  if(!is.null(departures)){
    stopifnot(all(departures >= 0))
    stopifnot(length(departures) == length(service))
    #stopifnot(anyNA(departures) == FALSE )
    stopifnot(is.numeric(departures))
  }
}



generate_input <- function(mag = 3){
  n <- 10^mag
  arrivals <- cumsum(rexp(n))
  service <- stats::rexp(n)
  departures <- queue(arrivals, service, 1)

  output <- list(arrivals = arrivals, service = service, departures = departures)

  return(output)
}

integrate_stepfun <- function(x, y, last = 1000){
  x <- c(0,x,last)
  x_diff <- diff(x)
  return((y %*% x_diff) %>% as.numeric)
}

#' print method for objects of class \code{queue_list}
#' @export
#' @param x an object of class \code{queue_list} produced by the \code{\link{queue_step}} function.
#' @param ... further arguments to be passed to other methods
print.queue_list <- function(x, ...){
  print(x$departures_df, ...)
}




