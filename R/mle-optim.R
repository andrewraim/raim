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
#' @param tx A transformation of the parameter \eqn{\phi}.
#' @param method Passed to \code{method} argument of \code{optim}.
#' @param gr Passed to \code{gr} argument of \code{optim}.
#' @param lower Passed to \code{lower} argument of \code{optim}.
#' @param upper Passed to \code{upper} argument of \code{optim}.
#' @param hessian Passed to \code{hessian} argument of \code{optim}.
#' @param jacobian Jacobian of tx evaluated at MLE. If argument is \code{NULL},
#' it will be computed numerically.
#' @param object Result of \code{mle_optim} or \code{txform}
#' @param x Result of \code{mle_optim} or \code{txform}.
#' @param k penalty parameter; the default \code{k = 2} gives AIC.
#' @param value a character vector.
#' @param ... Additional arguments
#' 
#' @examples
# Generate data
#' n = 200
#' x = runif(n, 0, 2)
#' X = model.matrix(~ x + I(x^2))
#' 
#' beta_true = c(0.5, 2, -0.25)
#' lambda_true = exp(X %*% beta_true)
#' y = rpois(n, lambda_true)
#' 
#' loglik = function(par) {
#' 	lambda = exp(X %*% par)
#' 	sum(dpois(y, lambda, log = TRUE))
#' }
#' 
#' # Fit Poisson model
#' init = c(0,0,0)
#' control = mle_optim_control()
#' mle_out = mle_optim(init, loglik, control = control)
#' 
#' parnames(mle_out)
#' parnames(mle_out) = c("beta0", "beta1","beta2")
#' print(mle_out)
#' 
#' txform(mle_out, tx = function(par) { lambda = exp(X[1:5,] %*% par) })
#' confint(mle_out)
#' coef(mle_out)
#' vcov(mle_out)
#' logLik(mle_out)
#' AIC(mle_out)
#' BIC(mle_out)
#' parnames(mle_out)
#' 
#' # Same as above, but computing Hessian
#' init = c(0,0,0)
#' control = mle_optim_control(hessian = FALSE)
#' mle_out = mle_optim(init, loglik, control = control)
#' print(mle_out)
#' 
#' parnames(mle_out)
#' parnames(mle_out) = c("beta0", "beta1","beta2")
#' 
#' txform(mle_out, tx = function(par) { lambda = exp(X[1:5,] %*% par) })
#' confint(mle_out)
#' coef(mle_out)
#' vcov(mle_out)
#' logLik(mle_out)
#' AIC(mle_out)
#' BIC(mle_out)
#' 
#' @name mle_optim
NULL

#' @name mle_optim
#' @export
mle_optim_control = function(method = "L-BFGS-B", gr = NULL, lower = -Inf,
	upper = Inf, control = list(), hessian = TRUE)
{
	control$fnscale = -1
	out = list(method = method, gr = gr, lower = lower, upper = upper,
		control = control, hessian = hessian)
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

	optim_res = optim(
		par = init[unfixed],
		fn = loglik_internal,
		gr = control$gr,
		method = control$method,
		lower = control$lower,
		upper = control$upper,
		control = control$control,
		hessian = control$hessian)

	par = init
	par[unfixed] = optim_res$par
	names(par) = par_names

	gr = rep(NA, qq)
	if (is.null(control$gr)) {
		gr[unfixed] = numDeriv::grad(loglik_internal, x = optim_res$par)
	} else {
		gr[unfixed] = control$gr(optim_res$par)
	}

	loglik_hat = optim_res$value

	vcov = matrix(0, qq, qq)
	if (is.null(control$hessian)) {
		vcov[unfixed,unfixed] = NA
	} else {
		vcov[unfixed,unfixed] = tryCatch({
			-solve(optim_res$hessian)
		}, error = function(e) {
			msg = sprintf("%s\n%s", "Failure to compute inverse of hessian", e)
			warning(msg)
			matrix(NA, length(unfixed), length(unfixed))
		})
	}
	rownames(vcov) = colnames(vcov) = par_names

	elapsed_sec = as.numeric(Sys.time() - start_time, "secs")

	res = list(par = par, vcov = vcov, loglik = loglik_hat, gr = gr,
		optim_res = optim_res, df = df, n = n, qq = qq,
		elapsed_sec = elapsed_sec, fixed = fixed)
	class(res) = "mle_optim"
	return(res)
}

#' @name mle_optim
#' @export
txform.mle_optim = function(object, tx, jacobian = NULL, ...)
{
	par = object$par
	V_par = object$vcov

	par_tx = tx(par)
	if (is.null(jacobian)) {
		J_tx = numDeriv::jacobian(tx, par)
	} else {
		stopifnot(nrow(J_tx) == length(par_tx))
		stopifnot(ncol(J_tx) == length(par))
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
	class(res) = "mle_optim_txform"
	return(res)
}

# TBD: this needs to be updated or removed
if (FALSE)
confint.mle_optim = function(object, parm, level = 0.95, ...)
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
	class(res) = "mle_optim_ci"
	return(res)
}

#' @name mle_optim
#' @export
print.mle_optim = function(x, ...)
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
summary.mle_optim = function(object, ...)
{
	object
}

#' @name mle_optim
#' @export
coef.mle_optim = function(object, ...)
{
	object$par
}

#' @name mle_optim
#' @export
logLik.mle_optim = function(object, ...)
{
	object$loglik
}

#' @name mle_optim
#' @export
AIC.mle_optim = function(object, ..., k = 2)
{
	-2 * logLik(object) + k*object$qq
}

#' @name mle_optim
#' @export
BIC.mle_optim = function(object, ...)
{
	n = object$n
	qq = object$qq
	-2 * logLik(object) + qq*log(n)
}

#' @name mle_optim
#' @export
vcov.mle_optim = function(object, ...)
{
	object$vcov
}

#' @name mle_optim
#' @export
parnames.mle_optim = function(object, ...)
{
	names(object$par)
}

#' @name mle_optim
#' @export
`parnames<-.mle_optim` = function(object, value)
{
	object_name = deparse(substitute(object))
	d = length(object$par)
	stopifnot(d == length(value))
	names(object$par) = value
	rownames(object$vcov) = value
	colnames(object$vcov) = value
	assign(object_name, object, envir = parent.frame())
}
