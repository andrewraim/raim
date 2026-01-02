#' print_function
#'
#' Print the interface of a function in a readable way. Excludes the body of
#' the function.
#' 
#' @param x A function object
#' @param ... Additional options passed to [stringr::str_wrap]
#' 
#' @details If `...` argument is empty, pass `exdent = 4` to [stringr::str_wrap].
#' Otherwise, use whatever is given in `...`.
#' 
#' @return Nothing is returned
#' 
#' @export
print_interface = function(x, ...)
{
	# 0. Convert `...` to a list. If empty, add a default.
    # 1. Get the interface of the function in a printable form; avoid line
    #    breaks.
    # 2. Replace the token 'function ' with the name of the function.
    # 3. Call str_wrap to handle any wrapping if the line is long.
    # 4. Print the wrapped line and replace newline characters with actual
    #    newlines.

	vargs = list(...)
	if (length(vargs) == 0) { vargs = list(exdent = 4) }
	desc = str(x, give.attr = F, width = Inf, strict.width = "wrap") |>
		capture.output()
	vargs$string = sub(
		pattern = "function ",
		replacement = deparse(substitute(x)),
		x = desc,
		fixed = TRUE)
	do.call(stringr::str_wrap, vargs) |> cat(collapse = "\n")
}

