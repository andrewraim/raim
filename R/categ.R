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
#' @returns a vector of category zero-based indices whose elements are in
#' \eqn{0, \ldots, k-1}.
#' 
#' @details
#' It might be more useful to have a version of this function that takes
#' probabilities on the log-scale. We could consider using the Gumbel trick
#' for that.
#' 
#' @name categ
NULL

#' @name categ
#' @export
r_categ = function(n, p, log_p = FALSE)
{
	k = length(p)
	P = matrix(p, n, k, byrow = TRUE)
	r_categ_mat(P, log_p = log_p)
}

#' @name categ
#' @export
r_categ_mat = function(P, log_p = FALSE)
{
	log_P = switch(log_p, "TRUE" = P, log(P))
	n = nrow(log_P)
	k = ncol(log_P)
	Z = matrix(rgumbel(n*k), n, k)
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

