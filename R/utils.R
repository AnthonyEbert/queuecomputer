

## Testing Functions

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
  ID = NULL
  times = NULL

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
  output <- switch(sf(time) + 1, c(stats::knots(sf),Inf)[findInterval(time,stats::knots(sf)) + 1] , time)
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
