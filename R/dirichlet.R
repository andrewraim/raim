#' Dirichlet Distribution
#' 
#' Functions for the Dirichlet distribution on the \eqn{k} dimensional
#' probability simplex.
#' 
#' @param n Number of desired draws
#' @param x A value in the sample space
#' @param alpha Vector of eqn{k} rate parameters
#' @param log If \code{TRUE} return the log-density
#' 
#' @return A vector of draws
#' 
#' @details
#' We assume Dirichlet distribution with density
#' \deqn{
#' f(x) = \frac{x_1^{\alpha_1-1} \cdots x_k^{\alpha_k-1}}{B(\bm{\alpha})}.
#' }
#' 
#' @name Dirichlet 
NULL

#' @name Dirichlet 
#' @export
ddirichlet = function(x, alpha, log = FALSE)
{
	x = as.matrix(x)
	n = nrow(x)
	k = ncol(x)
	stopifnot(k == length(alpha))
	logf = rep(lgamma(sum(alpha)) - sum(lgamma(alpha)), n)

	for (j in 1:k) {
		logf = logf + (alpha[j]-1)*x[,j]
	}

	ifelse (log, log_f, exp(log_f))
}

#' @name Dirichlet 
#' @export
rdirichlet = function(n, alpha)
{
	k = length(alpha)
	x = matrix(NA, n, k)
	for (j in 1:k) {
		x[,j] = rchisq(n, 2*alpha[j])
	}
	S = tcrossprod(rowSums(x), rep(1,k))
	return(x / S)
}

