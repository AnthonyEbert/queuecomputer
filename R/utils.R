

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
n_max <- function(input, n, tail_variable = 1000){
  input <- tail(input[!is.na(input)], tail_variable)
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
