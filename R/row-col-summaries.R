#' Title
#'
#' @param x A matrix
#' @param ... Zero or more strings that represent summaries. Strings that
#' represent numbers are interpreted as quantiles. Other strings are
#' interpreted as function names such as `"mean"`; these are evaluated from the
#' current environment. If labels are provided (e.g., `Q3 = "0.50"`), they are
#' used in the output.
#'
#' @returns A list whose elements are vectors corresponding to column-wise
#' summaries. List elements are given in the order they are specified in `...`.
#' They are labeled as follows. If no labels are included in `what`, the values
#' of `what` are used directly. If some or all elements of `what` are labeled,
#' those labels are used in the respective list elements.
#'
#' @examples
#' set.seed(1234)
#'
#' x = matrix(rnorm(500), 100, 5)
#' colsummaries(x)
#' colsummaries(x, "median", "0.025", "0.975")
#' colsummaries(x, Q1 = "min", Q2 = "0.25", Q3 = "median", Q4 = "0.75", Q5 = "max")
#'
#' z = t(x)
#' rowsummaries(z)
#' rowsummaries(z, "median", "0.025", "0.975")
#' rowsummaries(z, Q1 = "min", Q2 = "0.25", Q3 = "median", Q4 = "0.75", Q5 = "max")
#'
#' @name Row-and-column-Summaries
NULL

#' @name Row-and-column-Summaries
#' @export
colsummaries = function(x, ...)
{
	# Capture dot-dot-dot arguments in a character vector
	what = as.character(c(...))
	names(what) = names(c(...))

	if (length(what) == 0) {
		what = c("mean", "sd", lo = "0.05", hi = "0.95")
	}

	L = length(what)
	out = vector(mode = "list", length = L)

	# Set labels: use given label if provided; otherwise, use the value itself
	labels = character(length(what))
	if (is.null(names(what))) { labels = what }
	idx0 = which(nchar(names(what)) == 0)
	idx1 = which(nchar(names(what)) > 0)
	labels[idx0] = what[idx0]
	labels[idx1] = names(what)[idx1]
	names(out) = labels

	# Handle quantiles
	idx = which(!is.na(suppressWarnings(as.numeric(what))))
	for (i in seq_along(idx)) {
		label = labels[idx[i]]
		value = as.numeric(what[idx[i]])
		out[[idx[i]]] = apply(x, 2, quantile, prob = value)
	}

	# Assume all other elements of what correspond to function names
	idx = setdiff(1:L, idx)
	for (i in seq_along(idx)) {
		label = labels[idx[i]]
		fn = get(what[idx[i]], mode = "function")
		out[[idx[i]]] = apply(x, 2, fn)
	}

	return(out)
}

#' @name Row-and-column-Summaries
#' @export
rowsummaries = function(x, ...)
{
	out = colsummaries(t(x), ...)
	return(out)
}

