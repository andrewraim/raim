#' MLE Optimization Confidence Intervals
#' 
#' @param x 
#' @param ... 
#'
#' @name mle_optim_ci
#' @export
print.mle_optim_ci = function(x, ...)
{
	fit_out = x$fit
	dim_theta = length(unlist(fit_out$theta_hat))
	dim_xi = length(unlist(fit_out$xi_hat))

	printf("--- Parameter CIs (level %f) ---\n", x$level)
	idx = seq_len(dim_theta)
	print(x$ci[idx,])

	if (dim_xi > 0) {
		printf("--- Additional CIs (level %f) ---\n", x$level)
		idx = seq_len(dim_xi) + dim_theta
		print(x$ci[idx,])
	}
	
	printf("---\n")
	printf("Degrees of freedom: %d\n", fit_out$df)
	printf("t-quantile: %f\n", x$t_quantile)
}