laplace_approx = function(log_post, mode, Data, optim_control = list(),
	optim.method = "L-BFGS-B")
{
	optim_control$fnscale = -1
	fit = optim(mode, log_post, gr = NULL, Data, hessian = TRUE,
		method = optim.method, control = optim_control)

	mode = fit$par
	H = -solve(fit$hessian)
	p = length(mode)
	int = p/2 * log(2 * pi) + 0.5 * log(det(H)) + log_post(mode, Data)

	list(mode = mode, var = H, int = int, converge = (fit$convergence == 0),
		optim.out = fit)
}