#' Inverse Gamma distribution
#'
#' @param n Number of observations.
#' @param x Vector of quantiles.
#' @param a Shape parameter.
#' @param b Rate parameter.
#' @param log If \code{TRUE}, return densities and probabilities on the log-scale.
#'
#' @return
#' \code{dinvgamma} gives the density, \code{rinvgamma} generates random
#' deviates.
#' @name InverseGamma
NULL

#' @name InverseGamma
#' @export
rinvgamma = function(n, a, b)
{
	1 / rgamma(n, a, b)
}

#' @name InverseGamma
#' @export
dinvgamma = function(x, a, b, log = FALSE)
{
	logf = dgamma(1/x, a, b, log = TRUE) - 2 * log(x)
	if (log) { return(logf) } else { return(exp(logf))}
}
