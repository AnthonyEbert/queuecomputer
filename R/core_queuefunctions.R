




#' Compute the departure times of customers given: a set of arrival and service times, and a resource schedule.
#'
#'
#' @param arrival_df dataframe with column names ID and times . The ID column is a key
#'     for the customers. The times column is of class \code{numeric} and represents the
#'     arrival times of the customers.
#' @param service vector of service times with the same ordering as arrival_df.
#' @param servers an integer or an object of class \code{"server.list"}.
#' @param queueoutput A boolean variable which indicates whether the server number should be returned.
#' @return A vector of response times for the input of arrival times and service times.
#' @examples
#' set.seed(700)
#' arrival_df <- data.frame(ID = c(1:100), times = rlnorm(100, meanlog = 3))
#' service <- rlnorm(100)
#' server_list <- server_split(c(15,30,50),c(1,3,1,10))
#'
#' firstqueue <- queue_step(arrival_df = arrival_df, service = service)
#' secondqueue <- queue_step(arrival_df = arrival_df,
#'     servers = server_list, service = service, queueoutput = TRUE)
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
#'     secondqueue$times[ord], secondqueue$queue[ord])
#' @seealso \code{\link[queuecomputer]{wait_step}}, \code{\link[queuecomputer]{lag_step}}, \code{\link[queuecomputer]{as.server.list}}, \code{\link[queuecomputer]{server_split}}
#' @export
queue_step <- function(arrival_df, service, servers = 1, queueoutput = FALSE){

  UseMethod("queue_step", servers)

}

queue_step.server.list <- function(arrival_df, service, servers, queueoutput = FALSE){

  # Order arrivals and service according to time
  ord <- order(arrival_df$times)
  arrival_df <- arrival_df[ord, ]
  service <- service[ord]

  Number_of_queues <- length(servers)

  queue_times <- mapply(next_function, servers, rep(0, Number_of_queues))
  output_df <- rep(NA, dim(arrival_df)[1])
  queue_vector <- rep(NA,dim(arrival_df)[1])

  times <- arrival_df$times # This step leads to a big improvement in speed for some reason.

  for(i in 1:dim(arrival_df)[1]){
    test_queue_times <- pmax.int(queue_times, times[i])
    new_queue_times <- mapply(next_function, servers, test_queue_times)
    queue <- which.min(new_queue_times)

    queue_times[queue] <- new_queue_times[queue] + service[i]
    output_df[i] <- queue_times[queue]
    queue_vector[i] <- queue
  }

  # Put order back to original ordering
  output_df <- output_df[order(ord)]
  arrival_df <- arrival_df[order(ord),]
  queue_vector <- queue_vector[order(ord)]

  if(queueoutput == TRUE){
    output_df <- data.frame(ID = arrival_df$ID, times = output_df, queues = queue_vector)
  } else {
    output_df <- data.frame(ID = arrival_df$ID, times = output_df)
  }

  return(output_df)
}

queue_step.numeric <- function(arrival_df, service, servers = 1, queueoutput = FALSE){

  # Order arrivals and service according to time
  ord <- order(arrival_df$times)
  arrival_df <- arrival_df[ord, ]
  service <- service[ord]

  stopifnot(servers%%1 == 0)

  queue_times <- rep(0, servers)
  output_df <- rep(NA, dim(arrival_df)[1])
  queue_vector <- rep(NA,dim(arrival_df)[1])

  for(i in 1:dim(arrival_df)[1]){
    queue <- which.min(queue_times)
    queue_times[queue] <- max(arrival_df$times[i], queue_times[queue]) + service[i]

    output_df[i] <- queue_times[queue]
    queue_vector[i] <- queue
  }

  # Put order back to original ordering
  output_df <- output_df[order(ord)]
  arrival_df <- arrival_df[order(ord),]
  queue_vector <- queue_vector[order(ord)]

  if(queueoutput == TRUE){
    output_df <- data.frame(ID = arrival_df$ID, times = output_df, queues = queue_vector)
  } else {
    output_df <- data.frame(ID = arrival_df$ID, times = output_df)
  }

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
#' cbind(queue_step(arrival_df = arrival_df, service = service, servers = 100),
#' lag_step(arrival_df = arrival_df, service = service))
#' @seealso \code{\link[queuecomputer]{wait_step}}, \code{\link[queuecomputer]{queue_step}}
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
#'set.seed(500)
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
#' @seealso \code{\link[queuecomputer]{lag_step}}, \code{\link[queuecomputer]{queue_step}}
#' @export
wait_step <- function(arrival_df1, arrival_df2){
  arrival_df1 <- arrival_df1[order(arrival_df1$ID), ]
  arrival_df2 <- arrival_df2[order(arrival_df2$ID), ]

  times <- pmax.int(arrival_df1$times,arrival_df2$times)
  output_df <- data.frame(ID = arrival_df1$ID, times = times)
  return(output_df)
}









