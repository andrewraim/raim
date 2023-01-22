# This sampler and code was adapted from rwmetrop function in LearnBayes package

mh_rw_control = function(proposal_var, proposal_scale = 1, R = 1000,
	grp = NULL, burn = 0, thin = 1, report_period = R + 1)
{
	qq = nrow(proposal_var)
	stopifnot(R > burn)
	if (is.null(grp)) { grp = rep(1, qq) }

	list(proposal_var = proposal_var, proposal_scale = proposal_scale, R = R,
		grp = grp, burn = burn, thin = thin, report_period = report_period)
}

mh_rw = function(init, logpost, control)
{
	qq = length(init)

	stopifnot(class(control) == "mh_rw_control")
	R = control$R
	burn = control$burn
	thin = control$thin
	proposal_var = control$proposal_var
	proposal_scale = control$proposal_scale

	stopifnot(length(init) == nrow(proposal_var))
	stopifnot(length(init) == ncol(proposal_var))

	R_keep = ceiling((R - burn) / thin)
	idx_keep = 0
	par_hist = matrix(NA, R_keep, qq)

	b = init
	logfb = logpost(b)
	V_proposal_chol = chol(proposal$var)

	grp_list = list()
	G = length(unique(grp))
	for (g in 1:G) {
		grp_list[[g]] = which(grp == unique(grp)[g])
	}
	accept_grp = rep(0, G)

	for (r in 1:R) {
		bc = (proposal$scale * t(V_proposal_chol)) %*% rnorm(qq)
		for (g in 1:G) {
			idx_grp = grp_list[[g]]
			b_ = b
			b_[idx_grp] = b_[idx_grp] + bc[idx_grp]

			log_alpha = logpost(b_, Data) - logfb
			if (!is.na(log_alpha)) {
				if (log(runif(1)) < log_alpha) {
					b[idx_grp] = b_[idx_grp]
					accept_grp[g] = accept_grp[g] + 1
					logfb = logpost(b, Data)
				}
			} else {
				warning("log_alpha = NA")
			}
		}

		if (r > burn && r %% thin == 0) {
			idx_keep = idx_keep + 1
			par_hist[idx_sample,] = b
		}

		if (r %% report_period == 0) {
			acc = paste(sprintf("%0.02f", accept_grp / r * 100), collapse = ", ")
			logger("After %d rep, accept%% {%s}\n", r, acc)
		}
	}

	list(par = par_hist, accept = accept_grp / R)
}

laplace = function(logpost, mode, Data, optim_control = list(),
	optim.method = "L-BFGS-B")
{
	optim_control$fnscale = -1
	fit = optim(mode, logpost, gr = NULL, Data, hessian = TRUE,
		method = optim.method, control = optim_control)

	mode = fit$par
	H = -solve(fit$hessian)
	p = length(mode)
	int = p/2 * log(2 * pi) + 0.5 * log(det(H)) + logpost(mode, Data)

	list(mode = mode, var = H, int = int, converge = (fit$convergence == 0),
		optim.out = fit)
}

