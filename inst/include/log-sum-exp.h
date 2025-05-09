#ifndef LOG_SUM_EXP_H
#define LOG_SUM_EXP_H

#include <Rcpp.h>

/*
* C++ / Rcpp implementations of log-sum-exp functions.
*/

namespace raim {

inline double log_add2_exp(double x, double y)
{
	double s = std::min(x,y);
	double t = std::max(x,y);
	return t + std::log1p(exp(s - t));
}

inline double log_sub2_exp(double x, double y)
{
	if (std::isinf(x) && std::isinf(y) && x < 0 && y < 0) {
		return R_NegInf;
	}

	return x + std::log1p(-exp(y - x));
}

inline double log_sum_exp(const Rcpp::NumericVector& x)
{
    unsigned int k = x.size();

    Rcpp::NumericVector v = Rcpp::clone(x);
    std::sort(v.begin(), v.end(), std::greater<double>());

    double s = v(0);

    for (unsigned int j = 1; j < k; j++) {
        double ind = s > R_NegInf;
        double dd = v(j) - std::pow(s, ind);
        s = std::max(v(j), s) + std::log1p(std::exp(-std::fabs(dd)));
    }

    return s;
}

inline Rcpp::NumericVector log_sum_exp_mat(const Rcpp::NumericMatrix& x)
{
    unsigned int n = x.nrow();
    Rcpp::NumericVector out(n);

    for (unsigned int i = 0; i < n; i++) {
        out(i) = log_sum_exp(x.row(i));
    }

    return out;
}

inline Rcpp::NumericVector log_add2_exp(const Rcpp::NumericVector& x,
	const Rcpp::NumericVector& y)
{
	const Rcpp::NumericVector& s = Rcpp::pmin(x,y);
	const Rcpp::NumericVector& t = Rcpp::pmax(x,y);
	return  t + Rcpp::log1p(exp(s - t));
}

inline Rcpp::NumericVector log_sub2_exp(const Rcpp::NumericVector& x,
	const Rcpp::NumericVector& y)
{
	return x + Rcpp::log1p(-exp(y - x));
}

}

#endif

