#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
NumericVector qloop_numeric(NumericVector queue_times,
  NumericVector times, NumericVector service, NumericVector output) {
  int n = times.size();
  int qlen = queue_times.size();
  int queue = 0;

  for( int i=0; i < n; ++i)
  {
    queue = which_min(queue_times);
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


// NumericVector qloop_quickq(NumericVector queue_times,
//   NumericVector times, NumericVector service, NumericVector x, NumericVector y, NumericVector output) {
//   int n = times.size();
//   int qlen = queue_times.size();
//   int queue = 0;
//   NumericMatrix output_matrix;
//   for( int i=0; i < n; ++i)
//   {
//     queue = which_min(queue_times);
//     queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];
//     output[i] = queue_times[queue];
//     output[i + n] = queue + 1;
//     if( i % 100 == 0 )
//     {
//       Rcpp::checkUserInterrupt();
//     }
//   }
//
//   return output;
//
// }


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


