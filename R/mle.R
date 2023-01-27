#' MLE Optimization
#'
#' @param optim_method 
#' @param optim_control 
#'
#' @name mle_optim
NULL

#' @name mle_optim
#' @export
mle_optim_control = function(optim_method = "L-BFGS-B", optim_control = list())
{
	optim_control$fnscale = -1
	out = list(optim_method = optim_method, optim_control = optim_control)
	class(out) = "mle_optim_control"
	return(out)
}

#' @name mle_optim
#' @export
mle_optim = function(init, loglik, n, df = n, control = mle_optim_control())
{
	start_time = Sys.time()
	stopifnot(class(control) == "mle_optim_control")
	qq = length(init)

	if (is.null(names(init))) {
		par_names = sprintf("par%d", seq_len(qq))
	} else {
		par_names = names(init)
	}

	optim_res = optim(par = init, fn = loglik, method = control$optim_method,
		control = control$optim_control, hessian = TRUE)

	par = optim_res$par
	vcov = -solve(optim_res$hessian)
	loglik_hat = optim_res$value
	gr = numDeriv::grad(loglik, x = par)

	names(par) = par_names
	rownames(vcov) = colnames(vcov) = par_names

	elapsed_sec = as.numeric(Sys.time() - start_time, "secs")

	res = list(par = par, vcov = vcov, loglik = loglik_hat, gr = gr,
		optim_res = optim_res, df = df, n = n, qq = qq,
		elapsed_sec = elapsed_sec)
	class(res) = "mle_result"
	return(res)
}

#' @name mle_optim
#' @export
mle_transform = function(object, tx, labels = NULL)
{
	# TBD: do any additional transformations here
	# Do we want tx to return a vector or a list?

	par = object$par
	V_par = object$vcov

	par_tx = tx(par)
	J_tx = numDeriv::jacobian(tx, par)
	V_tx = J_tx %*% V_par %*% t(J_tx)

	if (is.null(labels)) {
		labels = sprintf("tx%d", seq_along(par_tx))
	}
	names(par_tx) = labels
	rownames(V_tx) = colnames(V_tx) = labels

	res = list(par = par_tx, vcov = V_tx)
	class(res) = "mle_transform_result"
	return(res)
}

#' @name mle_optim
#' @export
print.mle_transform_result = function(x, ...)
{
	printf("MLE Optim: Transformed Parameters\n")

	par = x$par
	se = sqrt(diag(x$vcov))

	DF = data.frame(par = par, se = se)
	colnames(DF) = c("Estimate", "SE")
	DF[,1] = round(DF[,1], 4)
	DF[,2] = round(DF[,2], 4)

	print(DF)
}

#' @name mle_optim
#' @export
coef.mle_transform_result = function(object, ...)
{
	object$par
}

#' @name mle_optim
#' @export
vcov.mle_transform_result = function(object, ...)
{
	object$vcov
}


mle_old = function(phi_init, loglik, n, df = n, theta_tx = identity, extra_tx,
	psi_names = NULL, control = mle_control())
{
	start_time = Sys.time()

	# Combine the two lists: theta_tx(phi) and extra_tx(phi)
	# into a vector. If the user provided psi_names, use those for the
	# variable names.
	psi_tx = function(phi) {
		theta = theta_tx(phi)
		psi1 = unlist(theta)
		psi2 = unlist(extra_tx(theta))
		psi = c(psi1, psi2)
		if (!is.null(psi_names)) { names(psi) = psi_names }
		return(psi)
	}

	stopifnot(class(control) == "mle_control")
	optim_method = control$optim_method
	optim_control = control$optim_control

	optim_res = optim(par = phi_init, fn = loglik, method = optim_method,
		control = optim_control, hessian = TRUE)

	phi_hat = optim_res$par
	theta_hat = theta_tx(phi_hat)
	xi_hat = extra_tx(theta_hat)
	psi_hat = psi_tx(phi_hat)

	V_phi = -solve(optim_res$hessian)
	J_tx = jacobian(psi_tx, phi_hat)
	V_psi = J_tx %*% V_phi %*% t(J_tx)
	rownames(V_psi) = colnames(V_psi) = names(psi_hat)

	loglik_hat = optim_res$value
	qq = length(unlist(theta_hat))

	se = sqrt(diag(V_psi))
	t_val = psi_hat / se
	p_val = 2 * pt(abs(t_val), df = df, lower.tail = FALSE)
	gr = J_tx %*% grad(loglik, x = phi_hat)

	estimates = cbind(psi_hat, se, t_val, p_val, gr)
	colnames(estimates) = c("Estimate", "SE", "t-val", "P(|t|>t-val)", "Gradient")

	elapsed_sec = as.numeric(Sys.time() - start.time, 'secs')

	res = list(estimates = estimates, loglik = loglik_hat, vcov = V_psi,
		V_phi = V_phi, optim_res = optim_res, df = df, n = n, qq = qq,
		description = "<Default>", theta_hat = theta_hat,
		xi_hat = xi_hat, phi_init = phi_init, elapsed_sec = elapsed_sec)
	class(res) = "mle_result"
	return(res)
}

#' @name mle_optim
#' @export
confint.mle_result = function(object, parm, level = 0.95, ...)
{
	dim_theta = length(unlist(object$theta_hat))
	dim_xi = length(unlist(object$xi_hat))

	na = rep(NA, dim_theta + dim_xi)
	DF = data.frame(Estimate = na, SE = na, Lower = na, Upper = na)
	rownames(DF) = rownames(object$estimates)

	w = -qt((1-level)/2, df = object$df)
	psi_hat = object$estimates[,1]
	se_psi_hat = object$estimates[,2]
	DF$Lower = psi_hat - w * se_psi_hat
	DF$Upper = psi_hat + w * se_psi_hat
	DF$Estimate = psi_hat
	DF$SE = se_psi_hat

	res = list(ci = DF, df = object$df, t_quantile = w, fit = object, level = level)
	class(res) = "mle_result_ci"
	return(res)
}

#' @name mle_optim
#' @export
print.mle_result_ci = function(x, ...)
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

#' @name mle_optim
#' @export
print.mle_result = function(x, ...)
{
	printf("MLE Optim Fit\n")

	dim_theta = length(unlist(x$theta_hat))
	dim_xi = length(unlist(x$xi_hat))

	par = x$par
	se = sqrt(diag(x$vcov))
	tval = par / se
	pval = 2 * pt(abs(tval), df = df, lower.tail = FALSE)
	gr = x$gr

	DF = data.frame(par = par, se = se, tval = tval, pval = pval, gr = gr)
	colnames(DF) = c("Estimate", "SE", "t-val", "P(|t|>t-val)", "Gradient")
	DF[,1] = round(DF[,1], 4)
	DF[,2] = round(DF[,2], 4)
	DF[,3] = round(DF[,3], 4)
	DF[,4] = my_numerical_format(DF[,4])
	DF[,5] = my_numerical_format(DF[,5])

	print(DF)

	msg = x$optim_res$message
	printf("---\n")
	printf("Elapsed Sec: %0.2f   ", x$elapsed_sec)
	printf("Degrees of freedom: %d\n", x$df)
	printf("LogLik: %0.4f   ", logLik(x))
	printf("AIC: %0.4f   ", AIC(x))
	printf("BIC: %0.4f\n", BIC(x))
	printf("Converged status: %d   ", x$optim_res$convergence)
}

#' @name mle_optim
#' @export
summary.mle_result = function(object, ...)
{
	object
}

#' @name mle_optim
#' @export
coef.mle_result = function(object, ...)
{
	object$par
}

#' @name mle_optim
#' @export
logLik.mle_result = function(object, ...)
{
	object$loglik
}

#' @name mle_optim
#' @export
AIC.mle_result = function(object, ..., k = 2)
{
	-2 * logLik(object) + k*object$qq
}

#' @name mle_optim
#' @export
BIC.mle_result = function(object, ...)
{
	n = object$n
	qq = object$qq
	-2 * logLik(object) + qq*log(n)
}

#' @name mle_optim
#' @export
vcov.mle_result = function(object, ...)
{
	object$vcov
}
