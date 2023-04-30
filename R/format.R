#' Formatting
#' 
#' Functions to format numbers as strings.
#' 
#' @param x A numeric vector.
#' @param lo Lower bound to determine which formatting to use. See details.
#' @param hi Upper bound to determine which formatting to use. See details.
#' @param fmt0 Format string which can be processed by \code{sprintf}. This is
#' used to display numbers which have very small or large magnitudes.
#' @param fmt1 Format string which can be processed by \code{sprintf}. This is
#' used to display numbers which have moderately sized magnitudes.
#' 
#' @details
#' Format numbers conditionally based on their magnitudes. If a number has
#' magnitude between \code{lo} and \code{hi}, it will be formatted using
#' \code{fmt1} - by default this is a decimal format. Otherwise it will be
#' formatted using \code{fmt0}.
#' 
#' Some formatting tasks are better handled by the \code{format} function in
#' base R, such as adding commas to mark thousands.
#' 
#' @examples
#' x = c(0, 1, 2, 3, 1e-10, 1e10)
#' format_numeric(x)
#' format_numeric(x, lo = -Inf)             # Only use fmt0 for big numbers
#' format_numeric(x, lo = -Inf, hi = Inf)   # Use fmt1 for all numbers
#' format_numeric(x, lo = Inf)              # Use fmt0 for all numbers
#' 
#' @name Format
#' @export
format_numeric = function(x, lo = 1e-4, hi = 1e5, fmt0 = "%0.3E", fmt1 = "%0.4f")
{
	ifelse(lo <= abs(x) & abs(x) <= hi,
		sprintf(fmt1, x),
		sprintf(fmt0, x))
}
