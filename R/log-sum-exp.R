#' Log-Sum-Exp
#'
#' Compute \code{log(sum(exp(x)))} but in a more stable way.
#'
#' @param x A numeric vector
#' @param y A numeric vector
#'
#' @details Computed using the method described by user Ben in StackExchange
#' thread \url{https://stats.stackexchange.com/questions/381936/vectorised-computation-of-logsumexp}.
#' A faster C version (requiring a dependency and possibly compilation) is
#' provided in \link[matrixStats]{logSumExp}.
#'
#' @examples
#' pi = 1:6 / sum(1:6)
#' x = log(2*pi)
#' log(sum(exp(x)))
#' log_sum_exp(x)
#'
#' @name Log-Sum-Exp
NULL

#' @name Log-Sum-Exp
#' @export
log_sum_exp = function(x) {
	k = length(x)
	v = sort(x, decreasing = TRUE)
	s = numeric(k)

	s[1] = v[1]
	for (j in setdiff(seq_len(k), 1)) {
		s[j] = max(v[j], s[j-1]) + log1p(exp(-abs(v[j] - s[j-1])))
	}

	return(s[k])
}

#' @name Log-Sum-Exp
#' @export
log_add2_exp = function(x, y)
{
	stopifnot(length(x) == length(y))
	s = pmin(x,y)
	t = pmax(x,y)
	t + log1p(exp(s - t))
}

#' @name Log-Sum-Exp
#' @export
log_sub2_exp = function(x, y)
{
	stopifnot(length(x) == length(y))
	s = pmin(x,y)
	t = pmax(x,y)
	t + log1p(-exp(s - t))
}

