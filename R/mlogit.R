# Transform from probability simplex S^J to R^(J-1)
mlogit = function(p, ref = 1)
{
	log(p[-ref] / p[ref])
}

# Transform from R^(J-1) to probability simplex S^J
inv.mlogit = function(x, ref = 1)
{
	p = numeric(length(x) + 1)
	z = exp(x)
	p[-ref] = z / (1 + sum(z))
	p[ref] = 1 / (1 + sum(z))
	return(p)
}

