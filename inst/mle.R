# ----- Test MLE functions with linear regression -----
n = 800
x = rnorm(n)
X = cbind(1, x)
d = ncol(X)
beta_true = c(0.5, 1)
sigma_true = 0.25
y = rnorm(n, X %*% beta_true, sigma_true)

loglik = function(par) {
	beta = par[seq_len(d)]
	sigma2 = exp(par[d+1])
	sum(dnorm(y, X %*% beta, sqrt(sigma2), log = TRUE))
}

init = c(0,0,0)
names(init) = c(sprintf("beta%d", 1:d), "pre_sigma2")

out = mle_optim(init, loglik)
print(out)

tx = function(par) {
	beta = par[seq_len(d)]
	sigma2 = exp(par[d+1])
	c(beta[1] - beta[2], sqrt(sigma2))
}
tx_out = mle_transform(out, tx, labels = c("beta_diff", "sigma"))
print(tx_out)
coef(tx_out)
vcov(tx_out)


# ----- Test MLE functions with Poisson regression -----
n = 800
x = rnorm(n)
X = cbind(1, x)
d = ncol(X)
beta_true = c(2, 1)
y = rpois(n, exp(X %*% beta_true))

loglik = function(par) {
	beta = par[seq_len(d)]
	sum(dpois(y, exp(X %*% beta), log = TRUE))
}

init = c(0,0)
names(init) = sprintf("beta%d", 1:d)
loglik(init)

out = mle_optim(init, loglik, n)
print(out)

tx = function(par) {
	beta = par[seq_len(d)]
	out = c(beta[1] - beta[2], beta[1] + beta[2], beta[1] * beta[2], exp(beta[1]))
	names(out) = c("beta_diff", "beta_sum", "beta_prod", "exp(beta1)")
	return(out)
}
tx_out = mle_transform(out, tx)
print(tx_out)
coef(tx_out)
vcov(tx_out)
