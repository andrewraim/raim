% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/truncated.R
\name{TruncatedUnivariate}
\alias{TruncatedUnivariate}
\alias{d_truncated}
\alias{p_truncated}
\alias{q_truncated}
\alias{r_truncated}
\title{Univariate Distributions Truncated to an Interval}
\usage{
d_truncated(x, lo, hi, df, pf, log = FALSE, ...)

p_truncated(x, lo, hi, pf, log.p = FALSE, ...)

q_truncated(p, lo, hi, pf, qf, log.p = FALSE, ...)

r_truncated(n, lo, hi, pf, qf, ...)
}
\arguments{
\item{x}{Quantile or argument of density.}

\item{lo}{Lower limit.}

\item{hi}{Upper limit.}

\item{df}{Density function for the untruncated distribution.}

\item{pf}{Cumulative distribution function for the untruncated distribution.}

\item{log}{logical; if \code{TRUE}, probabilities \code{p} are given as
\eqn{\log(p)}.}

\item{...}{Additional arguments.}

\item{log.p}{logical; if \code{TRUE}, probabilities \code{p} are given as
\eqn{\log(p)}.}

\item{p}{Probability.}

\item{qf}{Quantile function for the untruncated distribution.}

\item{n}{Desired sample size.}
}
\description{
Functions to truncate a given univariate distribution to an interval of the
form \eqn{(\text{lo}, \text{hi}]}.
}
\details{
This code was initially copied from the \link[LearnBayes]{rtruncated}
function in the LearnBayes package.
}
\examples{
# Take a large sample from truncated standard normal
x = r_truncated(n = 100000, lo = 1, hi = 2, pf = pnorm, qf = qnorm)

# Compare density to histogram of draws
hist(x, probability = TRUE)
curve(raim::d_truncated(x, lo = 1, hi = 2, df = dnorm, pf = pnorm), add = TRUE)

# Compare CDF to empirical CDF of draws
plot(ecdf(x))
curve(raim::p_truncated(x, lo = 1, hi = 2, pf = pnorm), col = "blue", lwd = 2, add = TRUE)

# Compare quantile function to quantiles of draws
pr = seq(0, 1, length.out = 50)
q_emp = quantile(x, probs = pr)
q_thry = raim::q_truncated(pr, lo = 1, hi = 2, pnorm, qf = qnorm)
plot(q_thry, q_emp)
abline(c(0,1), lty = 2, col = "red")

}
