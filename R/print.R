#' @export
printf = function(msg, ...)
{
	cat(sprintf(msg, ...))
}

#' @export
logger = function(msg, ...)
{
	sys.time = as.character(Sys.time())
	cat(sys.time, "-", sprintf(msg, ...))
}

#' @export
fprintf = function(file, msg, ...)
{
	cat(sprintf(msg, ...), file = file)
}

#' @export
my_numerical_format = function(x, tol = 1e-4, fmt_small = "%0.3E",
	fmt_large = "%0.4f")
{
	idx1 = which(abs(x) < tol)
	idx2 = setdiff(seq_along(x), idx1)
	out = character(length(x))
	out[idx1] = sprintf(fmt_small, x[idx1])
	out[idx2] = sprintf(fmt_large, x[idx2])
	names(out) = names(x)
	return(out)
}

