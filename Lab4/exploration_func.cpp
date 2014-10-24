#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
int coarsen(Rcpp::DataFrame data, Rcpp::vector pos_cols, Rcpp::Vector data_cols, 
            float dx, float dy, float w_x, float w_y, float coarsen_rad, bool y_fast) {
   x = data[pos_cols[0]]
   y = data[pos_cols[1]]
   printf('')
}
