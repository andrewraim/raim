# ----- Try normal as a quick test -----
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

out = mle_optim(init, loglik, n)
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
