#' Shift/scale transformation
#' 
#' Functions to Transformations between the unit interval \eqn{[0,1]} and
#' the interval \eqn{[a,b]} with \eqn{a \leq b}.
#'
#' @param x A point in \eqn{[a,b]}.
#' @param u A point in \eqn{[0,1]}.
#' @param a A number.
#' @param b A number.
#'
#' @name shift_scale
#' @examples
#' u = seq(0, 1, 0.1)
#' x = inv_shift_scale(u, 1, 10)
#' u_new = shift_scale(x, 1, 10)
#' 
NULL

#' @name shift_scale
#' @export
shift_scale = function(x, a, b)
{
	stopifnot(all(a <= b))
	stopifnot(all(is.finite(a)))
	stopifnot(all(is.finite(b)))
	out = (x - a) / (b - a)
	out[is.nan(out)] = 0.5
	return(out)
}

#' @name shift_scale
#' @export
inv_shift_scale = function(u, a, b)
{
	stopifnot(all(a <= b))
	stopifnot(all(0 <= u & u <= 1))
	stopifnot(all(is.finite(a)))
	stopifnot(all(is.finite(b)))
	(b-a)*u + a
}

