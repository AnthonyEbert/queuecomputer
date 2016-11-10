
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//#include <Rcpp.h>
using namespace std;
using namespace Rcpp;
using namespace arma;


// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
NumericVector qloop_numeric(NumericVector times, NumericVector service, NumericVector output, int n_servers) {
  int n = times.size();
  int queue = 0;

  std::vector<double> queue_times(n_servers, 0);

  for( int i=0; i < n; ++i)
  {
    std::vector<double>::iterator result = std::min_element(queue_times.begin(), queue_times.end());
    queue = std::distance(queue_times.begin(), result);
    queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];
    output[i] = queue_times[queue];
    output[i + n] = queue + 1;
    if( i % 100 == 0 )
    {
      Rcpp::checkUserInterrupt();
    }
  }

  return output;

}

// [[Rcpp::export]]
NumericVector qloop_numeric_test(NumericVector times, NumericVector service, int n_servers) {
  int n = times.size();
  vec output = vec((n+1) * 2);
  int queue = 0;

  vec queue_times = vec(n_servers);

  for( int i=0; i < n; ++i)
  {
    queue = index_min(queue_times);
    queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];
    output[i] = queue_times[queue];
    output[i + n] = queue + 1;
    if( i % 100 == 0 )
    {
      Rcpp::checkUserInterrupt();
    }
  }

  return(wrap(output));

}

// [[Rcpp::export]]
NumericVector test(NumericVector input) {
  vec x = vec(10);
  x.fill(datum::inf);
  int z=index_max(x);
  return(wrap(x));
}

// [[Rcpp::export]]
NumericVector qloop_qq_arma(NumericVector times, NumericVector service, NumericVector x, NumericVector y) {

  int n_servers = max(y);

  // std::vector<double> queue_times(n_servers, INT_MAX);
  vec queue_times = vec(n_servers);
  queue_times.fill(datum::inf);

  for(int i = 0; i < y[0]; i++)
  {
    queue_times[i] = 0;
  }

  int n = times.size();
  vec output = vec(n);
  int queue = 0;
  double next_time = x[0];

  int current_size = y[0];
  int next_size = y[1];
  int diff_size = 0;
  int iter = 0;

  for( int i=0; i < n; ++i)
  {

    if( all(queue_times >= next_time))
    {
      // printf("is true!");
      diff_size = next_size - current_size;

      if(diff_size == 0){
        // printf("error");
      }

      if(diff_size > 0)
      {
        // printf("big");
        for(int j = current_size; j < next_size; j++)
        {
          queue_times[j] = next_time;
        }
      }

      if(diff_size < 0)
      {
        // printf("small");
        for(int j = next_size; j < current_size; j++)
        {
          queue_times[j] = datum::inf;
        }
      }

      current_size = next_size;
      iter += 1;
      next_size = y[iter];
      next_time = x[iter];

    }

    queue = index_min(queue_times);
    queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];
    output[i] = queue_times[queue];
    //output[i + n] = queue + 1;
    if( i % 100 == 0 )
    {
      Rcpp::checkUserInterrupt();
    }

  }

  return(wrap(output));

}



