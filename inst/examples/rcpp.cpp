// [[Rcpp::depends(raim)]]
#include <raim.h>

// [[Rcpp::export]]
Rcpp::IntegerVector test_which(Rcpp::NumericVector& x)
{
	return raim::which(x > 0);
}

// [[Rcpp::export]]
void test_logger()
{
	raim::logger("%Y-%m-%d %H", " - ", "Message %d\n", 1);
	raim::logger("Message %d\n", 2);
}

// [[Rcpp::export]]
double test_logsumexp(const Rcpp::NumericVector& x)
{
	return raim::log_sum_exp(x);
}

// [[Rcpp::export]]
Rcpp::NumericVector test_log_add2_exp(const Rcpp::NumericVector& x, const Rcpp::NumericVector& y)
{
	return raim::log_add2_exp(x, y);
}

// [[Rcpp::export]]
Rcpp::NumericVector test_log_sub2_exp(const Rcpp::NumericVector& x, const Rcpp::NumericVector& y)
{
	return raim::log_sub2_exp(x, y);
}
