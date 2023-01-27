#' Dirichlet Distribution
#' 
#' Functions for the Dirichlet distribution
#' 
#' @param n Number of desired draws
#' @param x A value in the sample space
#' @param alpha Vector of eqn{k} rate parameters
#' 
#' @return A vector of draws
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
	stopifnot(k == ncol(alpha))

	log_f = lgamma(sum(alpha)) - sum(lgamma(alpha)) +
		rowSums((alpha-1) * log(x))

	# All of the values outside of Dirichlet sample space will now
	# have log_f = NaN, so convert them to -Inf. It might be better to
	# check the sample space purposefully though...
	idx = which(is.nan(log_f))
	log_f[idx] = -Inf

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

