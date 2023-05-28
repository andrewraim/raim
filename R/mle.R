#' MLE Optimization
#'
#' @param init Initial value of \eqn{\phi} for the sampler
#' @param loglik Log-likelihood function of \eqn{\phi}
#' @param n Sample size. Needed for AIC, BIC and some other optional results.
#' Default value \code{NA} results in \code{NA} values for those optional
#' results.
#' @param df Degrees of freedom to use for tests. Default value \code{Inf}
#' assumes a large sample Normal distribution.
#' @param fixed a vector of integers; default is an empty vector. These are
#' interpreted as indices into \code{init}; corresponding elements are held
#' fixed at their \code{init} values during optimization.
#' @param control Object created via \code{mle_optim_control} function.
#' @param optim_method A \code{method} argument to pass to \code{optim}
#' @param optim_control A \code{control} argument to pass to \code{optim}
#' @param tx A transformation of the parameter \eqn{\phi}
#' @param jacobian  
#' @param object Result of \code{mle_optim} or \code{txform}
#' @param x 
#' @param parm 
#' @param level 
#' @param k 
#' @param ... Additional arguments
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
mle_optim = function(init, loglik, n = NA, df = Inf, fixed = integer(0),
	control = mle_optim_control())
{
	start_time = Sys.time()
	stopifnot(class(control) == "mle_optim_control")
	qq = length(init)

	stopifnot(is.numeric(df))
	if (!is.na(n)) {
		stopifnot(is.numeric(n))
	}

	if (is.null(names(init))) {
		par_names = sprintf("par%d", seq_len(qq))
	} else {
		par_names = names(init)
	}

	# Set up any fixed arguments
	stopifnot(all(is.integer(fixed)))
	stopifnot(all(fixed >= 0 & fixed <= qq))
	unfixed = setdiff(seq_len(qq), fixed)
	if (length(fixed) > 0) {
		loglik_internal = function(par) {
			par_ext = init
			par_ext[unfixed] = par
			loglik(par_ext)
		}
	} else {
		loglik_internal = loglik
	}

	optim_res = optim(par = init[unfixed], fn = loglik_internal,
		method = control$optim_method, control = control$optim_control,
		hessian = TRUE)

	par = init
	par[unfixed] = optim_res$par
	names(par) = par_names
	
	gr = rep(NA, qq)
	gr[unfixed] = numDeriv::grad(loglik_internal, x = optim_res$par)

	loglik_hat = optim_res$value

	vcov = matrix(0, qq, qq)
	vcov[unfixed,unfixed] = tryCatch({
		-solve(optim_res$hessian)
	}, error = function(e) {
		msg = sprintf("%s\n%s", "Failure to compute inverse of hessian", e)
		warning(msg)
		matrix(NA, length(unfixed), length(unfixed))
	})
	rownames(vcov) = colnames(vcov) = par_names

	elapsed_sec = as.numeric(Sys.time() - start_time, "secs")

	res = list(par = par, vcov = vcov, loglik = loglik_hat, gr = gr,
		optim_res = optim_res, df = df, n = n, qq = qq,
		elapsed_sec = elapsed_sec, fixed = fixed)
	class(res) = "mle_result"
	return(res)
}

#' @name mle_optim
#' @export
txform.mle_result = function(object, tx, jacobian = NULL, ...)
{
	par = object$par
	V_par = object$vcov

	par_tx = tx(par)
	if (is.null(jacobian)) {
		J_tx = numDeriv::jacobian(tx, par)
	} else {
		stopifnot(nrow(jac_tx) == length(par_tx))
		stopifnot(ncol(jac_tx) == length(par))
		J_tx = jacobian
	}
	V_tx = J_tx %*% V_par %*% t(J_tx)

	labels = names(par_tx)
	if (is.null(labels)) {
		labels = sprintf("tx%d", seq_along(par_tx))
	}
	names(par_tx) = labels
	rownames(V_tx) = colnames(V_tx) = labels

	res = list(par = par_tx, vcov = V_tx)
	class(res) = "txform_mle_result"
	return(res)
}

#' @name mle_optim
#' @export
print.txform_mle_result = function(x, ...)
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

	par = x$par
	se = sqrt(diag(x$vcov))
	tval = par / se
	pval = 2 * pt(abs(tval), df = x$df, lower.tail = FALSE)
	gr = x$gr

	DF = data.frame(par = par, se = se, tval = tval, pval = pval, gr = gr)
	colnames(DF) = c("Estimate", "SE", "t-val", "P(|t|>t-val)", "Gradient")
	DF[,1] = round(DF[,1], 4)
	DF[,2] = round(DF[,2], 4)
	DF[,3] = round(DF[,3], 4)
	DF[,4] = format_numeric(DF[,4])
	DF[,5] = format_numeric(DF[,5])
	
	if (length(x$fixed) > 0) {
		DF$Fixed = "F"
		DF$Fixed[x$fixed] = "T"
	}

	print(DF)

	msg = x$optim_res$message
	printf("---\n")
	printf("Elapsed Sec: %0.2f   ", x$elapsed_sec)
	printf("Converged status: %d\n", x$optim_res$convergence)
	printf("LogLik: %0.4f   ", logLik(x))
	printf("AIC: %0.4f   ", AIC(x))
	printf("BIC: %0.4f\n", BIC(x))
	printf("Specified sample size: %0.0f  ", x$n)
	printf("Degrees of freedom: %0.0f\n", x$df)
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
