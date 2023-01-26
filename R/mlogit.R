#' Multinomial logit transformation
#' 
#' The multinomial logit transformation and its inverse.
#'
#' @param x A point in Euclidean space \eqn{R^{J-1}}.
#' @param p A point in the probability simplex \eqn{S^J}.
#' @param ref Index of the reference category. Default is \code{1}, the first
#' category.
#'
#' @details 
#' \itemize{
#' \item{mlogit}{Transforms from probability simplex \eqn{S^J} to \eqn{R^{J-1}}.}
#' \item{inv_mlogit}{Transforms from \eqn{R^{J-1}} to probability simplex \eqn{S^J}.}
#' }
#' 
#' @name mlogit
#' @examples
#' p = 1:5 / sum(1:5)
#' x = mlogit(p)
#' p_new = inv_mlogit(x)
#' 
NULL

#' @name mlogit
#' @export
mlogit = function(p, ref = 1)
{
	log(p[-ref] / p[ref])
}

#' @name mlogit
#' @export
inv_mlogit = function(x, ref = 1)
{
	p = numeric(length(x) + 1)
	z = exp(x)
	p[-ref] = z / (1 + sum(z))
	p[ref] = 1 / (1 + sum(z))
	return(p)
}

