% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mh_rw.R
\name{mh_rw}
\alias{mh_rw}
\title{Random Walk Metropolis-Hastings with Gaussian Proposal}
\arguments{
\item{init}{}

\item{log_post}{}

\item{proposal_var}{}

\item{control}{}

\item{proposal_scale}{}

\item{R}{}

\item{grp}{}

\item{burn}{}

\item{thin}{}

\item{report_period}{}
}
\value{
A list with the following elements
\itemize{
\item{par}{Matrix of draws where the \eqn{q} columns correspond to the
parameters of \code{log_post}}
\item{accept}{Vector of acceptance rates for the groups defined in in
\code{grp}}
}
}
\description{
This code was originally adapted from \code{rwmetrop} function in the
\code{LearnBayes} package
}
\examples{
# TBD
}