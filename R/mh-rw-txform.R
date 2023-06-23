#' Random Walk Metropolis-Hastings Transformed Parameters
#' 
#' These methods operate on results of \code{txform} from \code{mh_rw}.
#'
#' @param x Object to print
#' @param object Object to operate on
#' @param value Vector of new parameter names
#' @param ... Additional arguments
#'
#' @name mh_rw_txform
#' @examples
#' # TBD
NULL

#' @name mh_rw_txform
#' @export
print.mh_rw_txform = function(x, ...)
{
	printf("Metropolis-Hastings Random Walk: Transformed Parameters\n")

	DF = data.frame(
		mean = colMeans(x$par_hist),
		sd = apply(x$par_hist, 2, sd),
		lo = apply(x$par_hist, 2, quantile, prob = 0.025),
		mid = apply(x$par_hist, 2, quantile, prob = 0.5),
		hi = apply(x$par_hist, 2, quantile, prob = 0.975)
	)
	rownames(DF) = colnames(x$par_hist)
	colnames(DF) = c("Mean", "SD", "2.5%", "50%", "97.5%")
	DF = round(DF, 4)
	print(DF)

	printf("---\n")
	printf("Saved Draws: %d\n", nrow(x$par_hist))
}

#' @name mh_rw_txform
#' @export
parnames.mh_rw_txform = function(object, ...)
{
	colnames(object$par_hist)
}

#' @name mh_rw_txform
#' @export
`parnames<-.mh_rw_txform` = function(object, value)
{
	object_name = deparse(substitute(object))
	d = ncol(object$par_hist)
	stopifnot(d == length(value))
	colnames(object$par_hist) = value
	assign(object_name, object, envir = parent.frame())
}
