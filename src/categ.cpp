#include <Rcpp.h>

//' r_categ_mat
//' 
//' Sample from a matrix of categorical probabilities.
//' 
//' @param prob An \eqn{n \times k} matrix of probabilities, where \eqn{k} is
//' the number of categories and \eqn{n} is the desired number of indepedent
//' draws.
//' 
//' @returns a vector of category zero-based indices whose elements are in
//' \eqn{0, \ldots, k-1}.
//' 
//' @details
//' It might be more useful to have a version of this function that takes
//' probabilities on the log-scale. We could consider using the Gumbel trick
//' for that.
//' 
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector r_categ_mat(Rcpp::NumericMatrix prob)
{
	unsigned int k = prob.ncol();
	unsigned int n = prob.nrow();
	double p[k];
	int rn[k];
	Rcpp::IntegerVector z(n);

	for (unsigned int i = 0; i < n; i++) {
		double p_sum = sum(prob(i, Rcpp::_));
		for (unsigned int j = 0; j < k; j++) {
			p[j] = prob(i, j) / p_sum;
		}

		R::rmultinom(1, p, k, rn);

		for (unsigned int j = 0; j < k; j++) {
			z(i) += j * (rn[j] == 1);
		}
	}

	return z;
}
