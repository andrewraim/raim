#' Dirichlet Distribution
#' 
#' Functions for the Dirichlet distribution on the \eqn{k} dimensional
#' probability simplex.
#' 
#' @param n Number of desired draws
#' @param x A \eqn{k \times n} matrix where each column represents a value in
#' the sample space.
#' @param alpha Vector of \eqn{k} rate parameters
#' @param log If \code{TRUE} return the log-density
#' 
#' @return A vector of draws
#' 
#' @details
#' We assume the Dirichlet distribution with density
#' \deqn{
#' f(x) = x_1^{\alpha_1-1} \cdots x_k^{\alpha_k-1} \Big/ B(\bm{\alpha}),
#' \quad
#' x_j \in [0,1], \quad
#' \sum_{j=1}^k x_j = 1,
#' }
#' where
#' \deqn{
#' B(\bm{\alpha}) =
#' \Gamma(\alpha_1) \cdots \Gamma(\alpha_k) \Big/ \Gamma(\sum_{j=1}^k \alpha_j).
#' }
#' Note that with any elements of \eqn{\bm{x}} exactly zero, the density
#' \eqn{f(\bm{x})} may be \code{Inf}, \code{-Inf}, or \code{NaN}, depending on
#' the value of the corresponding \eqn{\bm{\alpha}} parameter.
#' 
#' @examples
#' pi = 1:3 / sum(1:3)
#' 
#' # Concentration parameters as a probability vector
#' x = r_dirichlet(20, pi)
#' d_dirichlet(x, pi)
#' 
#' # Use same probability vector, but scale the total to a small number.
#' # Here, x should be starting to behave like Mult(1, pi)
#' x = r_dirichlet(500, 0.01 * pi)
#' round(x[,1:20], 6)
#' rowMeans(x)
#' d_dirichlet(x, 0.01 * pi)
#' 
#' # Use same probability vector, but scale the total to a large number.
#' # Here, x should be concentrating around pi.
#' x = r_dirichlet(500, 1000 * pi)
#' round(x[,1:20], 6)
#' rowMeans(x)
#' 
#' @name Dirichlet 
NULL

#' @name Dirichlet 
#' @export
d_dirichlet = function(x, alpha, log = FALSE)
{
	x = as.matrix(x)
	n = ncol(x)
	k = nrow(x)
	stopifnot(k == length(alpha))

	log_comp = matrix(NA, k, n)
	for (j in 1:k) {
		log_comp[j,] = (alpha[j] - 1) * log(x[j,])
	}

	out = lgamma(sum(alpha)) - sum(lgamma(alpha)) + colSums(log_comp)
	ifelse (rep(log, n), out, exp(out))
}

#' @name Dirichlet 
#' @export
r_dirichlet = function(n, alpha)
{
	k = length(alpha)

	if (FALSE) {
		# This version is much easier. However, rchisq seems to produce only zeros
		# (rather than small positive values) when alpha is close to zero.
		x = matrix(NA, k, n)
		for (j in 1:k) {
			x[j,] = rchisq(n, 2*alpha[j])
		}
		s = matrix(colSums(x), k, n, byrow = TRUE)
		return(x / s)
	}

	# This version uses the fact that marginal and conditionals of Dirichlet are
	# beta rvs. See <https://en.wikipedia.org/wiki/Dirichlet_distribution#From_marginal_beta_distributions>
	phi = matrix(1, k, n)
	for (j in seq_len(k-1)) {
		idx_post = setdiff(seq_len(k), seq_len(j))
		phi[j,] = rbeta(n, alpha[j], sum(alpha[idx_post]))
	}

	x = matrix(NA, k, n)
	for (j in 1:k) {
		if (j > 1) {
			idx_pre = seq_len(j-1)
			x[j,] = phi[j,] * (1 - colSums(x[idx_pre,,drop=FALSE]))
		} else {
			x[j,] = phi[j,]
		}
	}

	return(x)
}

