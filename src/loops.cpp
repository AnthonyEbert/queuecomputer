
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

// [[Rcpp::export]]
NumericVector qloop_quick2(NumericVector queue_times,
                            NumericVector times, NumericVector service, NumericVector output,
                            NumericVector x, NumericVector y) {
  int n = times.size();
  int queue = 0;
  double next_time = x[0];
  int current_size = y[0];
  int next_size = y[1];
  int diff_size = 0;
  int iter = 0;

  for( int i=0; i < n; ++i)
  {


    Rf_PrintValue(queue_times);

    if( is_true( all (queue_times >= next_time)))
    {
      printf("is true!");
      diff_size = next_size - current_size;

      if(diff_size == 0){
        printf("error");
      }

      if(diff_size > 0)
      {
        printf("big");
        for(int j = current_size; j < next_size; j++)
        {
          queue_times[j] = next_time;
        }
      }

      current_size = next_size;
      iter = iter + 1;
      next_size = y[iter];
      next_time = x[iter];

    }

    queue = which_min(queue_times);
    queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];
    output[i] = queue_times[queue];
    //output[i + n] = queue + 1;
    if( i % 100 == 0 )
    {
      Rcpp::checkUserInterrupt();
    }

  }

  return output;

}


// NumericVector qloop_quickq(NumericVector Infinity, NumericVector times, NumericVector service, NumericVector x, NumericVector y, NumericVector output) {
//
//   int n = times.size();
//   int most_servers = max(y);
//
//   int next_time = x[0];
//   double next_size = y[1];
//   NumericVector next_size_vector = y[1];
//
//   int output_check = 0;
//   int iter_size = 0;
//
//   int queue = 0;
//   int number = 7;
//
//   NumericVector queue_times = rep_each(Infinity, most_servers);
//
//   for( int z=0; z <= y[0] - 1; ++z )
//   {
//     queue_times[z] = 0;
//   }
//
//   int diff_size = 0;
//
//
//   for( int i=0; i < n; ++i)
//   {
//
//     if( is_true( all (queue_times >= next_time)))
//     {
//
//
//       printf("update");
//       diff_size = next_size - y[iter_size];
//       iter_size = iter_size + 1;
//       if( diff_size > 0 )
//       {
//         printf("big");
//
//         for( int j=x[iter_size]; j < next_size; j++)
//         {
//           queue_times[j] = next_time;
//         }
//
//       }
//       if( diff_size < 0 )
//       {
//         printf("small");
//         for( int j= next_size ; j < x[iter_size]; j++)
//         {
//           queue_times[j] = Infinity[1];
//         }
//       }
//
//
//       next_size = x[iter_size];
//       next_time = y[iter_size];
//     }
//     Rf_PrintValue(queue_times);
//
//     queue = which_min(queue_times);
//     queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];
//     output[i] = queue_times[queue];
//     // output[i + n] = queue + 1;
//     if( i % 100 == 0 )
//     {
//       Rcpp::checkUserInterrupt();
//     }
//   }
//
//
//   return output;
//
// }

// queue = which_min(queue_times);
// queue_times[queue] = std::max(times[i], queue_times[queue]) + service[i];
// output[i] = queue_times[queue];
// output[i + n] = queue + 1;
// if( i % 100 == 0 )
// {
//   Rcpp::checkUserInterrupt();
// }

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

// for(int j=0; j < queue_times.size(); ++j)
// {
//   if(queue_times[j] <= next_time)
//   {
//     output_check = output_check + 1;
//   }
//   if(output_check > 0)
//   {
//     queue_times.size() = next_size
//     Rcpp::
//   }


