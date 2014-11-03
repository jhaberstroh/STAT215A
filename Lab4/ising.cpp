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


// [[Rcpp::export]]
Rcpp::NumericVector IsingPert(Rcpp::NumericVector x, 
                           Rcpp::NumericVector y,
                           Rcpp::NumericVector extent,
                           Rcpp::NumericVector posteriorA,
                           Rcpp::NumericVector posteriorB, 
                           float lambda, int iterations = 1){
  
  assert(x.size() == y.size());
  assert(y.size() == image_labels.size());
  const int size_x = extent[1] - extent[0] + 1;
  const int size_y = extent[3] - extent[2] + 1;
  int x0 = extent[0];
  int y0 = extent[2];
  std::cout << "Size of grid: "<< size_x << " " << size_y << std::endl;
  // Construct the grid of image_labels from the 1D data
  std::vector<std::vector<float> > gridA0(size_y, std::vector<float>(size_x,-1));
  std::vector<std::vector<float> > gridB0(size_y, std::vector<float>(size_x,-1));
  std::vector<std::vector<float> > gridA1(size_y, std::vector<float>(size_x,-1));
  std::vector<std::vector<float> > gridB1(size_y, std::vector<float>(size_x,-1));
  for (int i = 0 ; i < x.size() ; i++)
    {
  	  int posy = y[i] - y0;
		  int posx = x[i] - x0;
      //std::cout << "i: " << i << " x, y: ";
		  //std::cout << posy << ", ";
      //std::cout << posx << std::endl;
      gridA0[posy][posx] = posteriorA[i];
      gridA1[posy][posx] = posteriorA[i];
      gridB0[posy][posx] = posteriorB[i];
      gridB1[posy][posx] = posteriorB[i];
    }
  
	for (int step = 0 ; step < iterations ; step++)
		{
  		for (int i = 0 ; i < size_y ; i++)
  		  {
  		    for (int j = 0 ; j < size_x ; j++)
  		      {
							// Check that the grid point has a valid starting posterior value
							if ( (gridA1[i][j] >= 0) && (gridB1[i][j] >= 0) )
								{
  		        		for (int dir = 0 ; dir < 4 ; dir++)
										{
											int dy = 0;
											int dx = 0;
											if (dir / 2 == 2)
												{
													dy = dir % 2;
												}
											if (dir / 2 == 1)
												{
													dx = dir % 2;
												}

											// Check that:
											// 		the point is in the grid, 
											// 		the neighbor is a positive number
											// If it is, add it into the adjacent point scaled by lambda
											if ((i + dy < size_y) && (j + dx < size_x) &&
													(i + dy >= 0 )    && (j + dx >= 0)     &&
													(gridA0[i + dy][j + dx] >= 0) &&
													(gridB0[i + dy][j + dx] >= 0) )
												{
													gridA1[i][j] += lambda * gridA0[i + dy][j + dx];
													gridB1[i][j] += lambda * gridB0[i + dy][j + dx];
												}
										}
								}
  		      }
  		  }
  		for (int i = 0 ; i < size_y ; i++)
  		  {
  		    for (int j = 0 ; j < size_x ; j++)
  		      {
							gridA0[i][j] = gridA1[i][j];
							gridB0[i][j] = gridB1[i][j];
						}
				}
		}



	Rcpp::NumericVector classAB(posteriorA.size());

  for (int i = 0 ; i < x.size() ; i++)
    {
  	  int posy = y[i] - y0;
		  int posx = x[i] - x0;
			classAB[i] = (gridA0[posy][posx] > gridB0[posy][posx]) * 2 - 1;
    }
	
  return classAB;
}


