#' Gumbel Distribution
#' 
#' Functions for the Gumbel distribution
#' 
#' @param n Number of desired draws
#' @param mu Location parameter
#' @param sigma Scale parameter
#' 
#' @return A vector of draws
#' 
#' @name Gumbel
#' @export
rgumbel = function(n, mu = 0, sigma = 1)
{
	u = runif(n)
	-log(-log(u)) * sigma + mu
}
