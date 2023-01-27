#' Sequence
#'
#' @param x A matrix, array, or other object which \eqn{dim} can operate on.
#' @param d A dimension of \eqn{x}
#'
#' @export
seq_along_dim = function(x, d)
{
	seq_len(dim(x)[d])
}
