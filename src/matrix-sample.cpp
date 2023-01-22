#include <Rcpp.h>

RcppExport SEXP sample_matrix(SEXP prob_)
{
	Rcpp::NumericMatrix prob = Rcpp::as<Rcpp::NumericMatrix>(prob_);

	int k = prob.ncol();
	int n = prob.nrow();
	double p[k];
	int rn[k];

	Rcpp::IntegerVector z(n, 0);

	for (int i = 0; i < prob.nrow(); i++) {
		double p_sum = sum(prob(i, Rcpp::_));
		for (int j = 0; j < k; j++) {
			p[j] = prob(i, j) / p_sum;
		}

		rmultinom(1, p, k, rn);

		for (int j = 0; j < k; j++) {
			z(i) += (j+1)*(rn[j] == 1);
		}
	}

	return z;
}

