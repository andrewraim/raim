#' na
#' 
#' Create a vector of NA values.
#'
#' @param length A non-negative integer specifying the desired length.
#' @param type A string matching one of: \code{character}, \code{complex},
#' \code{integer}, or \code{real}
#'
#' @export
na = function(length, type = c("real", "integer", "complex", "character"))
{
	type = match.arg(type)
	
	switch(type,
		real = rep(NA_real_, length),
		integer = rep(NA_integer_, length),
		complex = rep(NA_complex_, length),
		character  = rep(NA_character_, length))
}

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

#' Unit Vector
#' 
#' @param j Position of one
#' @param n Dimension of vector
#' 
#' @return
#' An \eqn{n} dimensional vector with a one in the \eqn{j}th position and zeros
#' in other positions.
#'
#' @export
unitvec = function(j, n)
{
	stopifnot(all(j >= 0 & j <= n))
	x = numeric(n)
	x[j] = 1
	return(x)
}

#' "Not In" Operator
#' 
#' @param x Vector or NULL: the values to be matched. Long vectors are
#' supported.
#' @param table Vector or NULL: the values to be matched against. Long vectors
#' are not supported.
#' 
#' @return
#' The negation of the \code{%in%} operator.
#'
#' @export
`%notin%` = function(x, table)
{
	!(x %in% table)
}

