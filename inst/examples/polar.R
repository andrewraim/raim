library(ggplot2)
library(mvtnorm)
library(viridisLite)
library(ggforce)

set.seed(1234)

N = 50000
u = runif(N)

a = 1
b = 1.5

r2 = raim::r_truncated(N, lo = a^2, hi = b^2,
	pf = function(x) { pchisq(x, df = 2) },
	qf = function(x) { qchisq(x, df = 2) }
)
pts = sqrt(r2) * t(sapply(u, inv_polar))

# We should be able to compute the probability of this region using only the r2
# variable in the transformation.
x = rmvnorm(N, mean = c(0,0), sigma = diag(1,2,2))
idx = which(a^2 <= rowSums(x^2) & rowSums(x^2) <= b^2)
length(idx) / N
pchisq(b^2, df = 2) - pchisq(a^2, df = 2)

# Plot the truncated density
dat_plot = as.data.frame(pts)
ggplot() +
	stat_density_2d(data = dat_plot,
		aes(x = V1, y = V2, fill = after_stat(density)),
		geom = "raster", contour = FALSE) +
	scale_fill_viridis_c() +
	scale_x_continuous(expand = c(0, 0)) +
	scale_y_continuous(expand = c(0, 0)) +
	geom_circle(data = NULL, aes(x0 = 0, y0 = 0, r = a), col = "white", lty = 2) +
	geom_circle(data = NULL, aes(x0 = 0, y0 = 0, r = b), col = "white", lty = 2) +
	theme_bw() +
	theme(legend.position='none')

# Bivariate normal distribution
x = seq(min(dat_plot$V1), max(dat_plot$V1), 0.02) 
y = seq(min(dat_plot$V2), max(dat_plot$V2), 0.02)
mu = c(0, 0)
Sigma = diag(1, 2, 2)
f = function(x, y) dmvnorm(cbind(x, y), mu, Sigma)
z = outer(x, y, f)
image(x, y, z, col = viridis(25))

