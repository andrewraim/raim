#' Printing
#' 
#' @param fmt Formatted string which can be processed by \code{sprintf}
#' @param file A connection, or a character string naming the file to print to
#' @param x A numeric vector
#' @param tol A positive tolerance 
#' @param fmt_small An \code{sprintf} format to display small-magnitude numbers
#' @param fmt_large An \code{sprintf} format to display large-magnitude numbers
#' @param ... Additional arguments
#' 
#' @details
#' For \code{my_numerical_format}n numbers in \code{x} with smaller magnitude
#' than \code{tol} are formatted using \code{fmt_small}; others are formatted
#' using \code{fmt_large}.
#' 
#' @name Print
NULL

#' @name Print
#' @export
printf = function(fmt, ...)
{
	cat(sprintf(fmt, ...))
}

#' @name Print
#' @export
logger = function(fmt, ...)
{
	sys.time = as.character(Sys.time())
	cat(sys.time, "-", sprintf(fmt, ...))
}

#' @name Print
#' @export
fprintf = function(file, fmt, ...)
{
	cat(sprintf(fmt, ...), file = file)
}

#' @name Print
#' @export
my_numerical_format = function(x, tol = 1e-4, fmt_small = "%0.3E",
	fmt_large = "%0.4f")
{
	ifelse(abs(x) < tol, sprintf(fmt_small, x), sprintf(fmt_large, x))
}

