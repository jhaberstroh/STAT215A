#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <cassert>
#include <iostream>
#include <math.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar
    
    
// [[Rcpp::export]]
float CliquePotentialCpp (int pixel_ind, Rcpp::NumericVector x, 
                          Rcpp::NumericVector y, Rcpp::NumericMatrix label_mat,
                          int x_min, int x_max, int y_min, int y_max ) { 
int v = 0;
int x_ind = x[pixel_ind] - x_min;
int y_ind = y[pixel_ind] - y_min;
if (x[pixel_ind] < x_max)   
{
  if(label_mat(y_ind,x_ind) != label_mat(y_ind,x_ind+1))
    {
     v += 1;
    }
   else if (label_mat(y_ind,x_ind) == label_mat(y_ind,x_ind+1))
   {
     v += -1;
   }
}
if (x[pixel_ind] > x_min)   
{
    if(label_mat(y_ind,x_ind) != label_mat(y_ind,x_ind-1))
    {
     v += 1;
    }
   else if (label_mat(y_ind,x_ind) == label_mat(y_ind,x_ind-1))
   {
     v += -1;
   }
}
if (y[pixel_ind] < y_max)   
{
    if(label_mat(y_ind+1,x_ind) != label_mat(y_ind,x_ind))
    {
     v += 1;
    }
   else if (label_mat(y_ind+1,x_ind) == label_mat(y_ind,x_ind))
   {
     v += -1;
   }
}
if (y[pixel_ind] > y_min)   
{
    if(label_mat(y_ind-1,x_ind) != label_mat(y_ind,x_ind))
    {
     v += 1;
    }
   else if (label_mat(y_ind-1,x_ind) == label_mat(y_ind,x_ind))
   {
     v += -1;
   }
}
return v;
}
     

// [[Rcpp::export]]
float SingletonPotentialCpp (float y,
                             float mean, float var) {
  const float  PI_F=3.14159265358979f;
  float v = 0;
  v = log(sqrt(2*PI_F*var)) + 0.5*pow((y-mean),2)/var;
  return v;
}
    
    
// [[Rcpp::export]]
Rcpp::NumericVector CrfClassifier (int iterationNum, float beta, 
                     Rcpp::NumericVector x, Rcpp::NumericVector y,
                     Rcpp::NumericVector label,
                     Rcpp::NumericVector label_temp, 
                     Rcpp::NumericVector ndai_vec, Rcpp::NumericVector mean,
                     Rcpp::NumericVector var) {
   // Create NDIA matrix using geographical coordinates
   int x_min;
   x_min = *std::min_element(std::begin(x), std::end(x));
   int x_max;
   x_max = *std::max_element(std::begin(x), std::end(x));
   int y_min;
   y_min = *std::min_element(std::begin(y), std::end(y));
   int y_max;
   y_max = *std::max_element(std::begin(y), std::end(y));
   Rcpp::NumericMatrix label_mat(y_max-y_min+1, x_max-x_min+1);
   for (int i = 0; i < ndai_vec.size(); i ++)
   {
     label_mat(int(y[i])-y_min,int(x[i])-x_min) = label_temp[i];
   }

   float post_energy1, post_energy2;
   Rcpp::NumericVector label_pre = label_temp;
   for (int k = 0; k < iterationNum; k++)
   {
     for (int i = 0; i < label_temp.size(); i ++) 
     {
       // posterior enregy if the label is cloud
       label_mat(int(y[i])-y_min, int(x[i])-x_min) = 1;                                   
       post_energy1 = SingletonPotentialCpp(ndai_vec[i], mean[1], var[1]) + 
                      beta * CliquePotentialCpp(i, x, y, label_mat, 
                                          x_min,x_max, y_min, y_max );
       // posterior energy if no cloud
       label_mat(int(y[i])-y_min, int(x[i])-x_min) = -1;
       post_energy2 = SingletonPotentialCpp(ndai_vec[i], mean[0], var[0]) + 
                      beta * CliquePotentialCpp(i, x, y, label_mat, 
                                          x_min,x_max, y_min, y_max );

       if (post_energy1 < post_energy2)
       {
         label_pre[i] = 1;
               // std::cout << "hi" << std::endl;
       }
       else
       {
         label_pre[i] = -1;
       }
       label_mat(int(y[i])-y_min, int(x[i])-x_min) = label_pre[i];
         
     }
   }

   
   double count = 0;
   for (int i = 0; i < label.size(); i ++) 
   {
     if ((label[i] != 0) && (label[i] != label_pre[i]))
     {
       count += 1;
     }
   }
   float err;
   err = count/label.size();
   std::cout << err << std::endl;
   return label_pre;
}


                            
