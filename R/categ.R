#' Categorical Distribution
#' 
#' Sample from a matrix of categorical probabilities.
#' 
#' @param n Number of desired draws
#' @param p A \eqn{k}-dimensional vector of probabilities
#' @param P An \eqn{n \times k} matrix of probabilities, where \eqn{k} is
#' the number of categories and \eqn{n} is the desired number of independent
#' draws.
#' @param log_p If \code{TRUE}, interpret given probabilities as having been
#' supplied on the log-scale. Otherwise, interpret them on the original scale.
#' 
#' @returns a vector of category indices whose elements are in
#' \eqn{1, \ldots, k}.
#' 
#' @details
#' We make use of the Gumbel trick to draw from probabilities given on the
#' log-scale without having to normalize. Note that \code{r_categ} can be slow
#' with large \code{n} because it runs in a loop in plain R.
#' 
#' @name categ
NULL

#' @name categ
#' @export
r_categ = function(n, p, log_p = FALSE)
{
	k = length(p)
	lp = switch(log_p, "TRUE" = p, log(p))
	out = numeric(n)

	for (i in 1:n) {
		z = r_gumbel(k)
		out[i] = which.max(z + lp)
	}

	return(out)
}

#' @name categ
#' @export
r_categ_mat = function(P, log_p = FALSE)
{
	log_P = switch(log_p, "TRUE" = P, log(P))
	n = nrow(log_P)
	k = ncol(log_P)
	Z = matrix(r_gumbel(n*k), n, k)
	max.col(Z + log_P)
}

#' mult2cat
#' 
#' Ungroup a \eqn{k \times n} matrix of multinomial observations to individual
#' categories, where \eqn{k} is the number of categories abd \eqn{n} is the
#' number of observations.
#'
#' @param x A \eqn{k \times n} matrix of counts
#'
#' @return An \eqn{n} dimensional vector of indices representing categories of
#' individual trials.
#' @export
mult2cat = function(x) {
	k = nrow(x)
	rep(1:k, rowSums(x))
}

