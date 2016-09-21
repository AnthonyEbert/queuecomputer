

## Testing Functions

#' Return \code{n}th maximum of vector
#'
#' @param input A vector of numbers.
#' @param n An integer.
#' @return \code{n} th maximum of \code{input}
#' @examples
#' n_max(c(3,1,7),2)
#' @seealso wait_step
#' @export
n_max <- function(input, n, tail_variable = 500){
  input <- tail(input, tail_variable)
  for(i in 1:n){
    output <- suppressWarnings(max(input, na.rm = TRUE))
    input[which.max(input)] <- NA
  }
  return(output)
}

#' Find nth maximum of vector.
#'
#' @param input A vector.
#' @param n     An integer.
#' @return The position of the \code{n} th maximum of \code{input}
#' @examples
#' which_nmax(c(40,50,30),1)
#' which_nmax(c(40,50,30),2)
#' which_nmax(c(40,50,30),3)
#' @export
which_nmax <- function(input,n){
  for(i in 1:n){
    output <- which.max(input)
    input[which.max(input)] <- NA
  }
  return(output)
}

#' Find nth minimum of vector.
#'
#' @param input A vector.
#' @param n     An integer.
#' @return The position of the \code{n} th minimum of \code{input}
#' @examples
#' which_nmin(c(40,50,30),1)
#' which_nmin(c(40,50,30),2)
#' which_nmin(c(40,50,30),3)
#' @seealso \code{\link{which_nmax}}
#' @export
which_nmin <- function(input,n){
  for(i in 1:n){
    output <- which.min(input)
    input[which.min(input)] <- NA
  }
  return(output)
}

#' Return arrival vector of bags dataset.
#'
#' @param bagdataset A dataset of bags.
#' @param number_of_passengers The total number of passengers all the bags could belong to.
#' @return Maximum times for each passenger ID
#' @examples
#' bags <- rpois(100,1)
#' bags.df <- data.frame(BagID = 1:sum(bags),
#'     ID = rep(1:100, bags), times = rlnorm(sum(bags), meanlog = 2))
#' arrivals2 <- reduce_bags(bags.df, 100)
#' @seealso wait_step
#' @export
reduce_bags <- function(bagdataset, number_of_passengers){
  zerobags <- data.frame(BagID = NA, ID = c(1:number_of_passengers), times = 0)
  reduced_df <- as.data.frame(dplyr::summarise(dplyr::group_by(rbind(bagdataset, zerobags), ID), n = max(times)))
  ord <- order(reduced_df$ID)
  reduced_df <- reduced_df[order(ord),]
  names(reduced_df) <- c("ID", "times")
  return(reduced_df)
}

#' Return next available time for server given current departure times.
#'
#' @param sf A step function with heights either 0 or 1.
#' @param time the time in question.
#' @return Next available time for server in question.
#' @export
next_function <- function(sf,time){
  output <- switch(sf(time) + 1, c(knots(sf),Inf)[findInterval(time,knots(sf)) + 1] , time)
  return(output)
}

#' Makes it easier to input number of servers into queue_step.
#'
#' @param x numeric vector giving the knots or jump locations of the step function for stepfun().
#' @param y numeric vector giving the knots or jump locations of the step function for stepfun(). Must be integers.
#' @return List of height 1 step functions for input into queue_step.
#' @seealso queue_step()
#' @export
server_split <- function(x, y){
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
  return(output)
}
