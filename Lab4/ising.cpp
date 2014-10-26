#include <Rcpp.h>
#include <vector>
#include <cassert>
#include <iostream>
// using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
Rcpp::NumericVector LearnJ(Rcpp::NumericVector x, 
                           Rcpp::NumericVector y,
                           Rcpp::NumericVector extent,
                           Rcpp::NumericVector image_labels){
  
  assert(x.size() == y.size());
  assert(y.size() == image_labels.size());
  const int size_x = extent[1] - extent[0] + 1;
  const int size_y = extent[3] - extent[2] + 1;
  int x0 = extent[0];
  int y0 = extent[2];
  std::cout << "Size of grid: "<< size_x << " " << size_y << std::endl;
  // Construct the grid of image_labels from the 1D data
  std::vector<std::vector<int> > grid(size_y, std::vector<int>(size_x,0));
  
  for (int i = 0 ; i < x.size() ; i++){
		int posy = y[i] - y0;
		int posx = x[i] - x0;
    //std::cout << "i: " << i << " x, y: ";
		//std::cout << posy << ", ";
    //std::cout << posx << std::endl;
    grid[posy][posx] = image_labels[i];
  }
  return 0;
}
