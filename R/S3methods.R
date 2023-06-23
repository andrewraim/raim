#' Transform Estimates
#'
#' Transform results from an estimation method
#'
#' @param object a result from an estimation method
#' @param tx A function of the parameters estimated in \code{object}
#' @param ... additional arguments
#'
#' @details
#' Suppose \code{object} is the result of an estimation method that produces
#' estimates or MCMC draws of model parameters \eqn{\phi} and \code{tx} is a
#' function \eqn{t(\phi) = (t_1(\phi), \ldots, t_k(\phi))} of \eqn{\phi}. The
#' \code{txform} function produces corresponding results for \eqn{t(\phi)},
#'
#' @return
#' Estimates of \code{tx(phi)}
#'
#' @export
txform = function(object, tx, ...)
{
	UseMethod("txform")
}

#' Parameter names
#' 
#' Get or set parameter names.
#' 
#' @param object An object.
#' @param value a character vector.
#' @param ... Additional parameters.
#'
#' @name parnames
#' @export
parnames = function(object, ...)
{
	UseMethod("parnames")
}

#' @name parnames
#' @export
`parnames<-` = function(object, value)
{
	UseMethod("parnames<-")
}
