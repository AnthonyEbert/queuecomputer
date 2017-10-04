
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//#include <Rcpp.h>
using namespace std;
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector qloop_numeric(NumericVector times, NumericVector service, int n_servers) {
  int n = times.size();
  vec output = vec((n+1) * 2 - 1);
  int queue = 0;

  vec queue_times = vec(n_servers);
  queue_times.fill(0);

  for( int i=0; i < n; ++i)
  {
    queue = index_min(queue_times);
    queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];
    output[i] = queue_times[queue];
    output[i + n] = queue + 1;
    if( i % 512 == 0 )
    {
      Rcpp::checkUserInterrupt();
    }
  }

  return(wrap(output));

}

// [[Rcpp::export]]
NumericVector qloop_qq(NumericVector times, NumericVector service, NumericVector x, NumericVector y) {

  int n_servers = max(y);

  vec queue_times = vec(n_servers);
  queue_times.fill(datum::inf);

  for(int i = 0; i < y[0]; i++)
  {
    queue_times[i] = 0;
  }

  int n = times.size();
  vec output = vec((n+1) * 2 - 1);
  output.fill(datum::inf);
  int queue = 0;
  double next_time = x[0];

  int current_size = y[0];
  int next_size = y[1];
  int diff_size = 0;
  int iter = 0;

  for( int i=0; i < n; ++i)
  {

    if( all(queue_times >= next_time) | (times[i] >= next_time))
    {
      diff_size = next_size - current_size;

      if(diff_size > 0)
      {
        for(int j = current_size; j < next_size; j++)
        {
          queue_times[j] = next_time;
        }
      }

      if(diff_size < 0)
      {
        for(int j = next_size; j < current_size; j++)
        {
          queue_times[j] = datum::inf;
        }
      }

      current_size = next_size;
      iter += 1;
      next_size = y[iter+1];
      next_time = x[iter];

    }

    queue = index_min(queue_times);
    queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];

    output[i] = queue_times[queue];
    output[i + n] = queue + 1;

    // in case user presses stop.
    if( i % 512 == 0 )
    {
      Rcpp::checkUserInterrupt();
    }

    // in case number of servers is zero.
    if( current_size == 0 )
    {
      i = i - 1;
      if( next_time == datum::inf )
      {
        break;
      }
    }

  }

  return(wrap(output));

}







