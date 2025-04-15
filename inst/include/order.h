#ifndef ORDER_H
#define ORDER_H

#include <Rcpp.h>

namespace raim {

inline Rcpp::IntegerVector order(const Rcpp::NumericVector& x, bool decrease)
{
	Rcpp::NumericVector sorted = clone(x).sort(decrease);
	return Rcpp::match(sorted, x);
}

}

#endif
