% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mle-optim.R
\name{mle_optim}
\alias{mle_optim}
\alias{mle_optim_control}
\alias{txform.mle_optim}
\alias{print.mle_optim}
\alias{summary.mle_optim}
\alias{coef.mle_optim}
\alias{logLik.mle_optim}
\alias{AIC.mle_optim}
\alias{BIC.mle_optim}
\alias{vcov.mle_optim}
\alias{parnames.mle_optim}
\alias{parnames<-.mle_optim}
\title{MLE Optimization}
\usage{
mle_optim_control(
  method = "L-BFGS-B",
  gr = NULL,
  lower = -Inf,
  upper = Inf,
  control = list(),
  hessian = TRUE
)

mle_optim(
  init,
  loglik,
  n = NA,
  df = Inf,
  fixed = integer(0),
  control = mle_optim_control()
)

\method{txform}{mle_optim}(object, tx, jacobian = NULL, ...)

\method{print}{mle_optim}(x, ...)

\method{summary}{mle_optim}(object, ...)

\method{coef}{mle_optim}(object, ...)

\method{logLik}{mle_optim}(object, ...)

\method{AIC}{mle_optim}(object, ..., k = 2)

\method{BIC}{mle_optim}(object, ...)

\method{vcov}{mle_optim}(object, ...)

\method{parnames}{mle_optim}(object, ...)

\method{parnames}{mle_optim}(object) <- value
}
\arguments{
\item{method}{Passed to \code{method} argument of \code{optim}.}

\item{gr}{Passed to \code{gr} argument of \code{optim}.}

\item{lower}{Passed to \code{lower} argument of \code{optim}.}

\item{upper}{Passed to \code{upper} argument of \code{optim}.}

\item{control}{Object created via \code{mle_optim_control} function.}

\item{hessian}{Passed to \code{hessian} argument of \code{optim}.}

\item{init}{Initial value of \eqn{\phi} for the sampler}

\item{loglik}{Log-likelihood function of \eqn{\phi}}

\item{n}{Sample size. Needed for AIC, BIC and some other optional results.
Default value \code{NA} results in \code{NA} values for those optional
results.}

\item{df}{Degrees of freedom to use for tests. Default value \code{Inf}
assumes a large sample Normal distribution.}

\item{fixed}{a vector of integers; default is an empty vector. These are
interpreted as indices into \code{init}; corresponding elements are held
fixed at their \code{init} values during optimization.}

\item{object}{Result of \code{mle_optim} or \code{txform}}

\item{tx}{A transformation of the parameter \eqn{\phi}.}

\item{jacobian}{Jacobian of tx evaluated at MLE. If argument is \code{NULL},
it will be computed numerically.}

\item{...}{Additional arguments}

\item{x}{Result of \code{mle_optim} or \code{txform}.}

\item{k}{penalty parameter; the default \code{k = 2} gives AIC.}

\item{value}{a character vector.}
}
\description{
MLE Optimization
}
\examples{
n = 200
x = runif(n, 0, 2)
X = model.matrix(~ x + I(x^2))

beta_true = c(0.5, 2, -0.25)
lambda_true = exp(X \%*\% beta_true)
y = rpois(n, lambda_true)

loglik = function(par) {
	lambda = exp(X \%*\% par)
	sum(dpois(y, lambda, log = TRUE))
}

# Fit Poisson model
init = c(0,0,0)
control = mle_optim_control()
mle_out = mle_optim(init, loglik, control = control)

parnames(mle_out)
parnames(mle_out) = c("beta0", "beta1","beta2")
print(mle_out)

txform(mle_out, tx = function(par) { lambda = exp(X[1:5,] \%*\% par) })
confint(mle_out)
coef(mle_out)
vcov(mle_out)
logLik(mle_out)
AIC(mle_out)
BIC(mle_out)
parnames(mle_out)

# Same as above, but computing Hessian
init = c(0,0,0)
control = mle_optim_control(hessian = FALSE)
mle_out = mle_optim(init, loglik, control = control)
print(mle_out)

parnames(mle_out)
parnames(mle_out) = c("beta0", "beta1","beta2")

txform(mle_out, tx = function(par) { lambda = exp(X[1:5,] \%*\% par) })
confint(mle_out)
coef(mle_out)
vcov(mle_out)
logLik(mle_out)
AIC(mle_out)
BIC(mle_out)

}
