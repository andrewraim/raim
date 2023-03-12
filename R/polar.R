#' Polar transformation
#' 
#' The polar transformation and its inverse, as expressed in Section 2.7 of
#' Anderson (2003).
#'
#' @param x A point in Euclidean space \eqn{\mathbb{R}^{d-1}}.
#' @param z A point in the unit sphere \eqn{\mathbb{S}^d = \{ \bm{z} \in \mathbb{R}^d: \bm{z}^\top \bm{z} = 1 \}}.
#'
#' @details 
#' \describe{
#' \item{polar}{Transforms from  \eqn{\mathbb{S}^d} to \eqn{\mathbb{R}^{d-1}}.}
#' \item{inv_polar}{Transforms from \eqn{\mathbb{R}^{d-1}} to \eqn{\mathbb{S}^d}.}
#' }
#' 
#' @name polar
#' @examples
#' p = 1:5 / sum(1:5)
#' x = mlogit(p)
#' p_new = inv_mlogit(x)
#' 
#' @references T.W. Anderson (2003). An Introduction to Multivariate
#' Statistical Analysis, Wiley-Interscience, 3rd Edition.
#' 
NULL

#' @name polar
#' @export
polar = function(z)
{
	stop("Need to figure out what this should be")
}

#' @name polar
#' @export
inv_polar = function(x)
{
	d = length(x) + 1
	stopifnot(all(0 <= x & x <= 1))

	# Compute angles phi from x
	# Shift and scale x_1, ... x_{d-2} to be between -pi/2 and pi/2
	# Shift and scale x_{d-1} to be between -pi and pi
	phi = numeric(d-1)
	phi[seq_len(d-2)] = pi*x[seq_len(d-2)] - pi/2 
	phi[d-1] = 2*pi*x[d-1] - pi

	stop("Left off here")
	z = numeric(d)
	for (j in 1:d) {
		idx_cur = ifelse(j < d, j)
		idx_prev = seq_len(j-1)
		if (j < d) {
			stop("Left off here")
		}
		z[j] = prod(cos(phi[idx_prev])) * sin(phi[j])
	}
	return(z)
}

