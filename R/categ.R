#' Categorical Distribution
#' 
#' Sample from a matrix of categorical probabilities.
#' 
#' @param P An \eqn{n \times k} matrix of probabilities, where \eqn{k} is
#' the number of categories and \eqn{n} is the desired number of indepedent
#' draws.
#' 
#' @returns a vector of category zero-based indices whose elements are in
#' \eqn{0, \ldots, k-1}.
#' 
#' @details
#' It might be more useful to have a version of this function that takes
#' probabilities on the log-scale. We could consider using the Gumbel trick
#' for that.
#' 
#' @export
r_categ_mat = function(P)
{
	n = nrow(P)
	k = ncol(P)
	Z = matrix(rgumbel(n*k), n, k)
	max.col(Z + log(P))
}
