#ifndef WHICH_H
#define WHICH_H

#include <Rcpp.h>

namespace raim {

Rcpp::NumericVector concat(const Rcpp::NumericVector& x, const Rcpp::Nume
ricVector& y)
{
	unsigned int m = x.size();
	unsigned int n = y.size();

	Rcpp::NumericVector z(m + n);
	for (unsigned int i = 0; i < m; i++) {
		z(i) = x(i);
	}
	for (unsigned int i = 0; i < n; i++) {
		z(i + m) = y(i);
	}
	return z;
}

}

#endif
