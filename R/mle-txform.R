#' MLE Optimization Transformed Parameters
#' 
#' These methods operate on results of \code{txform} from \code{mle_optim}.
#'
#' @param object Object to pprint
#' @param x Object to operate on
#' @param value Vector of new parameter names
#' @param ... Additional arguments
#' 
#' @name mle_optim_txform
NULL


#' @name mle_optim_txform
#' @export
print.mle_optim_txform = function(x, ...)
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

#' @name mle_optim_txform
#' @export
coef.mle_optim_txform = function(object, ...)
{
	object$par
}

#' @name mle_optim_txform
#' @export
vcov.mle_optim_txform = function(object, ...)
{
	object$vcov
}

#' @name mle_optim_txform
#' @export
parnames.mle_optim_txform = function(object, ...)
{
	names(object$par)
}

#' @name mle_optim_txform
#' @export
`parnames<-.mle_optim_txform` = function(object, value)
{
	object_name = deparse(substitute(object))
	d = length(object$par)
	stopifnot(d == length(value))
	names(object$par) = value
	assign(object_name, object, envir = parent.frame())
}
