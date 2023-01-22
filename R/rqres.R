#  Set eps to zero to avoid using random jitter
rqres <- function(y, F, eps = 1e-6)
{
	n <- length(y)
	FL <- F(y - eps)
	FU <- F(y)
	u <- runif(n, min = FL, max = FU)
	qres <- qnorm(u)
	return(qres)
}
