Rcpp::sourceCpp("inst/examples/rcpp.cpp")

x = rnorm(50)
ii = test_which(x)

which(x > 0)
ii
ii + 1

test_logger()

x = log(c(0.5,1,2,3))
log(sum(exp(x)))
raim::log_sum_exp(x)
test_logsumexp(x)

x = log(c(0.75,1,2,3))
y = log(c(0.5,0.9,2,3))
log(exp(x) + exp(y))
log(exp(x) - exp(y))
test_log_add2_exp(x, y)
test_log_sub2_exp(x, y)
