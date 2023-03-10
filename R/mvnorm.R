#' Multivariate Normal distribution
#'
#' A k-dimensional Multivariate Normal distribution.
#'
#' @param n Number of observations.
#' @param x An \eqn{n \times k} matrix with n arguments.
#' @param mu mean vector parameter.
#' @param Sigma Covariance matrix parameter.
#' @param Sigma_chol Cholesky factor of covariance matrix parameter.
#' @param Omega Precision matrix parameter.
#' @param log If \code{TRUE}, return densities and probabilities on the log-scale.
#'
#' @return
#' \code{dmvnorm} gives the density using specified covariance matrix,
#' \code{dmvnorm_prec} gives the density using specified precision matrix,
#' \code{rmvnorm} generates random deviates using specified covariance matrix,
#' \code{rmvnorm_prec} generates random deviates using specified precision matrix,
#' \code{r_singular_mvnorm} generates random deviates using specified singular
#' covariance matrix (and mean zero).
#'
#' @details
#' A use case for the function \code{r_singular_mvnorm} would be to start with
#' a singular precision matrix \code{Q}, and use something like
#' \code{Sigma = ginverse(Q)} as the covariance matrix.
#'
#' @name MultivariateNormal
NULL

#' @name MultivariateNormal
#' @export
r_mvnorm = function(n, mu, Sigma)
{
	k = length(mu)
	stopifnot(k == nrow(Sigma) && k == ncol(Sigma))
	Z = matrix(rnorm(n*k), k, n)
	A = t(chol(Sigma))
	A %*% Z + mu
}

#' @name MultivariateNormal
#' @export
r_mvnorm_chol = function(n, mu, Sigma_chol)
{
	k = length(mu)
	stopifnot(k == nrow(Sigma_chol) && k == ncol(Sigma_chol))
	Z = matrix(rnorm(n*k), k, n)
	Sigma_chol %*% Z + mu
}

#' @name MultivariateNormal
#' @export
r_mvnorm_prec = function(n, mu, Omega)
{
	k = length(mu)
	stopifnot(k == nrow(Omega) && k == ncol(Omega))
	Z = matrix(rnorm(n*k), k, n)

	# Compute A such that Omega = A %*% t(A)
	if (isDiagonal(Omega)) {
		idx = cbind(seq_len(k), seq_len(k))
		A = Diagonal(x = Omega[idx]^(1/2))
	} else if (is(Omega, 'sparseMatrix')) {
		A = chol(Omega)
	} else {
		A = chol(Omega)
	}

	solve(A, Z) + mu
}

#' @name MultivariateNormal
#' @export
r_singular_mvnorm = function(n, Sigma)
{
	eig = eigen(Sigma)
	eigvals = eig$values
	eigvecs = eig$vectors
	k = nrow(eigvecs)
	r = ncol(eigvecs)
	Z = matrix(rnorm(n*k), k, n)
	eigvecs %*% (sqrt(eigvals) * Z)
}

#' @name MultivariateNormal
#' @export
d_mvnorm = function(x, mu, Sigma, log = FALSE)
{
	n = nrow(x)
	k = length(mu)
	stopifnot(k == nrow(Sigma) && k == ncol(Sigma) && k == ncol(x))

	# Use QR decomposition to efficiently compute determinant and inverse
	xc = x - t(mu) %x% matrix(1,n,1)
	qr_out = qr(Sigma)
	Omega_xc = qr.solve(qr_out, t(xc))
	logdetA = sum(log(abs(diag(qr.R(qr_out)))))
	logf = -k/2*log(2*pi) - logdetA / 2 - rowSums(xc * t(Omega_xc)) / 2

	if (log) { return(logf) } else { return(exp(logf))}
}

#' @name MultivariateNormal
#' @export
d_mvnorm_prec = function(x, mu, Omega, log = FALSE)
{
	n = nrow(x)
	k = length(mu)
	stopifnot(k == nrow(Sigma) && k == ncol(Sigma) && k == ncol(x))

	xc = x - t(mu) %x% matrix(1,n,1)
	Omega_xc = Omega %*% t(xc)
	logdetA = -as.numeric(determinant(Omega)$modulus)
	logf = -k/2*log(2*pi) - logdetA / 2 - rowSums(xc * t(Omega_xc)) / 2

	if (log) { return(logf) } else { return(exp(logf))}
}

