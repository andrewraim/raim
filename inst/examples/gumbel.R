library(raim)

x = r_gumbel(n = 10000)

plot(density(x))
curve(d_gumbel(x), lty = 2, add = TRUE)
