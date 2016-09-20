




#' Compute the vector of response times from a queue with a vector of arrival times and service times.
#'
#'
#' @param arrival_df A dataframe with column names ID and times . The ID column is a key
#'     for the customers. The times column is of class \code{numeric} and represents the
#'     arrival times of the customers.
#' @param Number_of_queues A step function which describes the number of service desks open
#' @param service A vector of service times with the same ordering as arrival_df
#' @return A vector of response times for the input of arrival times and service times
#' @examples
#' arrival_df <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 3))
#' service <- rlnorm(100)
#' firstqueue <- queue_step(arrival_df = arrival_df, service = service)
#' secondqueue <- queue_step(arrival_df = arrival_df,
#'     Number_of_queues = stepfun(c(15,30,50), c(1,3,1,10)), service = service)
#'
#' curve(ecdf(arrival_df$times)(x) * 100 , from = 0, to = 200,
#'     xlab = "time", ylab = "Number of customers")
#' curve(ecdf(firstqueue$times)(x) * 100 , add = TRUE, col = "red")
#' curve(ecdf(secondqueue$times)(x) * 100, add = TRUE, col = "blue")
#' legend(100,40, legend = c("Customer input - arrivals",
#'     "Customer output - firstqueue",
#'     "Customer output - secondqueue"),
#'     col = c("black","red","blue"), lwd = 1, cex = 0.8
#' )
#'
#' # Queue lengths
#' ecdf(arrival_df$times)(c(1:200))*100 - ecdf(firstqueue$times)(c(1:200))*100
#' ecdf(arrival_df$times)(c(1:200))*100 - ecdf(secondqueue$times)(c(1:200))*100
#'
#' ord <- order(arrival_df$times)
#' cbind(arrival_df[ord,], service[ord],
#'     queue_step(arrival_df = arrival_df,
#'     Number_of_queues = stepfun(c(15,30,50), c(1,3,1,10)), service = service)$times[ord])
#' @seealso wait_step, lag_step
#' @references Sutton, C., & Jordan, M. I. (2011). Bayesian inference for queueing networks and modeling of internet services. The Annals of Applied Statistics, 254-282, page 258.
#' @export
queue_step <- function(arrival_df, Number_of_queues = stats::stepfun(1,c(1,1)), service){

  # Order arrivals and service according to time
  ord <- order(arrival_df$times)
  arrival_df <- arrival_df[ord, ]
  service <- service[ord]

  # Initialise loop
  output_df <- rep(NA, dim(arrival_df)[1])
  n <- Number_of_queues(0)
  starttime <- 0

  # Pre-compute knots
  pc_knots <- stats::knots(Number_of_queues)

  for(i in 1:dim(arrival_df)[1]){
    newstarttime <- pc_knots[findInterval(n_max(input = output_df[1:i], n = n),pc_knots)]
    starttime <- max(starttime,newstarttime)
    n <- Number_of_queues(max(n_max(input = output_df[1:i], n = n), arrival_df$times[i], starttime))
    output_df[i] <- max(n_max(input = output_df[1:i], n = n), arrival_df$times[i], starttime) + service[i]
  }

  # Put order back to original ordering
  output_df <- output_df[order(ord)]
  arrival_df <- arrival_df[order(ord),]

  output_df <- data.frame(ID = arrival_df$ID, times = output_df)
  return(output_df)
}

#' Add lag to vector of arrival times.
#' @param arrival_df A dataframe with column names ID and times . The ID column is a key
#'     for the customers. The times column is of class \code{numeric} and represents the
#'     arrival times of the customers.
#' @param service A vector of service times with the same ordering as arrival_df.
#' @return A vector of response times for the input of arrival times and service times.
#' @examples
#' # Create arrival times
#' arrival_df <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 3))
#'
#' # Create service times
#' service <- rlnorm(100)
#' lag_step(arrival_df = arrival_df, service = service)
#'
#' # lag_step is equivalent to queue_step with a large number of queues, but it's faster to compute.
#'
#' cbind(queue_step(arrival_df = arrival_df, service = service,
#'     Number_of_queues = stepfun(1,c(100,100))), lag_step(arrival_df = arrival_df, service = service))
#' @seealso queue_step
#' @export
lag_step <- function(arrival_df, service){

  # Add service time to arrival_vector
  output_df <- data.frame(ID = arrival_df$ID, times = arrival_df$times + service)
  return(output_df)
}

#' Compute maximum time from two vectors of arrival times.
#' @param arrival_df1 A dataframe with column names ID and times . The ID column is a key
#'     for the customers. The times column is of class \code{numeric} and represents the
#'     arrival times of the customers.
#' @param arrival_df2 A dataframe with column names ID and times . The ID column is a key
#'     for the customers. The times column is of class \code{numeric} and represents the
#'     arrival times of the customers.
#' @return The maximum time from two vectors of arrival times.
#' @details A good real-world example of this is finding the departure times for passengers
#'  after they pick up their bags from the baggage carosel. The time at which they leave is
#'  the maximum of the passenger and bag arrival times.
#' @examples
#'arrivals <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 4))
#'service <- rlnorm(100)
#'
#'#Airport example ------------------------
#'
#'# Create a number of bags for each of 100 customers
#'bags <- rpois(100,1)
#'bags.df <- data.frame(BagID = 1:sum(bags),
#'    ID = rep(1:100, bags), times = rlnorm(sum(bags), meanlog = 2))
#'
#'# Find when the last bag for each customer arrives
#'arrivals2 <- reduce_bags(bags.df, 100)
#'
#'# Find the time when customers can leave with their bags.
#'wait_step(arrival_df1 = arrivals, arrival_df2 = arrivals2)
#' @export
wait_step <- function(arrival_df1, arrival_df2){
  mat <- cbind(arrival_df1$times,arrival_df2$times)
  output_df <- data.frame(ID = arrival_df1$ID, times = apply(mat, 1, max))
  return(output_df)
}









