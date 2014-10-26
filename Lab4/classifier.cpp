#include <Rcpp.h>
#include <vector>
#include <cassert>
#include <iostream>


bool lt_toggle(float x, float x_thresh, bool lt)
{
	if (lt)
		{
			return x < x_thresh;
		}
	else
		{
			return x > x_thresh;
		}
}


// Function takes v, w1 & w2 and computes labels:
// 		(v_lt * (v < v1_thresh)) | 
// 			(w1_lt * (w1 < w1_thresh) && 
// 				 w2_lt * (w2 < w2_thresh)) = -1
// 		else = +1
//
// [[Rcpp::export]]
float ClassificationError(Rcpp::NumericVector v, float v_thresh, bool v_lt,
													Rcpp::NumericVector w1, float w1_thresh, bool w1_lt,
													Rcpp::NumericVector w2, float w2_thresh, bool w2_lt,
													Rcpp::NumericVector label)
{
	assert(v.size() == w1.size());
	assert(w1.size() == w2.size());
	
	std::vector<int> new_labels(v.size(),0);
	
	int err = 0;
  int count = 0;
	for (int i = 0 ; i < v.size() ; i++)
		{
			bool v_ok = lt_toggle( v[i],  v_thresh,  v_lt);
			bool w1_ok= lt_toggle(w1[i], w1_thresh, w1_lt);
			bool w2_ok= lt_toggle(w2[i], w2_thresh, w2_lt);
			int label_i = 0;
			if (v_ok || (w1_ok && w2_ok))
				{
					label_i = -1;
				}
			else
				{
					label_i = 1;
				}
      if (label[i] != 0)
		    {
			    if (label_i != label[i])
				    {
					    err++;
				    }
          count++;
		    }
		}
		return float(err) / float(count);
}

// [[Rcpp::export]]
Rcpp::NumericVector ClassificationSearch_w2thresh(
													Rcpp::NumericVector v, float v_thresh, bool v_lt,
													Rcpp::NumericVector w1, float w1_thresh, bool w1_lt,
													Rcpp::NumericVector w2, Rcpp::NumericVector w2_thresh_v, bool w2_lt,
													Rcpp::NumericVector label, bool verbose=0){
	Rcpp::NumericVector errs(w2_thresh_v.size());
	for (int i = 0 ; i < errs.size(); i++)
		{
			errs[i] = ClassificationError( v,  v_thresh,       v_lt,
																		w1, w1_thresh, 			w1_lt,
																		w2, w2_thresh_v[i], w2_lt,
																		label);
			if (i%1000 == 0 && verbose)
				{
					std::cout << i << std::endl;
				}
		}
	return errs;
}
