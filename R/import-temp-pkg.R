#' Create and Import a Temporary Package
#' 
#' Create a temporary package from source files so that functions can be
#' accessed from a namespace. This is based on Josh O'Brien's answer from
#' \url{https://stackoverflow.com/questions/15789036/namespaces-without-packages}
#'
#' @param src_files A vector of filenames with source code for the temp package
#' @param pkg_name A name for the temp package
#' @param unlink_on_exit Remove the temporary package when R exits
#'
#' @return Name of the temporary directory with the package
#'  
#' @examples
#' # Create a couple of example source files
#' ff1 = tempfile(pattern = "bar", fileext = ".R")
#' ff2 = tempfile(pattern = "baz", fileext = ".R")
#' cat("bar = function() { print('Hello world') }", file = ff1)
#' cat("baz = function() { print('Goodbye world.') }", file = ff2)
#' import_temp_pkg(src_files = c(ff1, ff2), pkg_name = "foo")
#' unlink(c(ff1,ff2))
#' 
#' @export
import_temp_pkg = function(src_files, pkg_name, unlink_on_exit = TRUE)
{
	require(devtools)
	dd = tempdir()
	if (unlink_on_exit) {
		on.exit(unlink(file.path(dd, pkg_name), recursive = TRUE))
	}
	package.skeleton(name = pkg_name, path = dd, code_files = src_files)
	load_all(file.path(dd, pkg_name))
	return(invisible(dd))
}

