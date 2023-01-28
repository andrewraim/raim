#' Randomized Quantile Residuals
#' 
#' Compute randomized quantile residuals (Dunn & Smyth, 1996).
#'
#' @param y Vector of univariate observations
#' @param p_dist A cumulative distribution function. Typically the CDF of a
#' fitted model with with parameters evaluated at  estimates.
#' @param eps A small positive number needed for discrete distributions.
#' Set to zero to avoid using random jitter.
#'
#' @return Vector of residuals
#' 
#' @references
#' Peter K. Dunn & Gordon K. Smyth (1996) Randomized Quantile Residuals,
#' Journal of Computational and Graphical Statistics, 5:3, 236-244,
#' DOI: 10.1080/10618600.1996.10474708.
#' 
#' @examples
#' mu_true = 10
#' y = rpois(200, mu_true)
#' mu_hat = mean(x)
#' p_dist = function(x) { ppois(y, mu_hat) }
#' res = rqres(y, p_dist)
#'
#' @export
rqres = function(y, p_dist, eps = 1e-6)
{
	n = length(y)
	FL = p_dist(y - eps)
	FU = p_dist(y)
	u = runif(n, min = FL, max = FU)
	qnorm(u)
}

