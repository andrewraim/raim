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
my_numerical_format = function(x, lower = 1e-4)
{
	idx1 = which(abs(x) < lower)
	idx2 = setdiff(1:length(x), idx1)
	y = character(length(x))
	y[idx1] = sprintf("%0.3E", x[idx1])
	y[idx2] = sprintf("%0.4f", x[idx2])
	names(y) = names(x)
	return(y)
}
