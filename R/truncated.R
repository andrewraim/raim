#' r_truncated
#' 
#' Generate draws from a truncated univariate distribution based on its CDF and
#' quantile function. This is basically copied from
#' \link[LearnBayes]{rtruncated}.
#'
#' @param n 
#' @param lo 
#' @param hi 
#' @param pf 
#' @param qf 
#' @param ... 
#'
#' @export
r_truncated = function (n, lo, hi, pf, qf, ...) {
	qf(pf(lo, ...) + runif(n) * (pf(hi, ...) - pf(lo, ...)), ...)
}

