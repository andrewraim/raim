#' Random Walk Metropolis-Hastings with Gaussian Proposal
#' 
#' This code was originally adapted from \code{rwmetrop} function in the
#' \code{LearnBayes} package.
#'
#' @param init 
#' @param log_post 
#' @param proposal_var 
#' @param control 
#' @param proposal_scale 
#' @param R 
#' @param grp 
#' @param burn 
#' @param thin 
#' @param report_period 
#'
#' @returns A list with the following elements
#' \itemize{
#' \item{par}{Matrix of draws where the \eqn{q} columns correspond to the
#' parameters of \code{log_post}}
#' \item{accept}{Vector of acceptance rates for the groups defined in in
#' \code{grp}}
#' }
#'
#' @name mh_rw
#' @examples
#' # TBD
NULL

#' @name mh_rw
#' @export
mh_rw_control = function(proposal_scale = 1, R = 1000,
	grp = NULL, burn = 0, thin = 1, report_period = R + 1)
{
	q = nrow(proposal_var)
	stopifnot(R > burn)

	out = list(proposal_scale = proposal_scale, R = R, grp = grp, burn = burn,
		thin = thin, report_period = report_period)
	class(out) = "mh_rw_control"
	return(out)
}

#' @name mh_rw
#' @export
mh_rw = function(init, log_post, proposal_var, control = mh_rw_control())
{
	start_time = Sys.time()

	q = length(init)
	stopifnot(q == nrow(proposal_var))
	stopifnot(q == ncol(proposal_var))

	stopifnot(class(control) == "mh_rw_control")
	R = control$R
	burn = control$burn
	thin = control$thin
	proposal_scale = control$proposal_scale
	grp = control$grp
	report_period = control$report_period

	# Default grouping is to sample all q variables with a single proposed draw
	if (is.null(grp)) {
		grp = rep(1, q)
	}
	
	# Make a list of indices for each group
	grp_list = list()
	G = length(unique(grp))
	for (g in 1:G) {
		grp_list[[g]] = which(grp == unique(grp)[g])
	}
	accept_grp = rep(0, G)

	idx_keep = 0
	R_keep = ceiling((R - burn) / thin)
	par_hist = matrix(NA, R_keep, q)
	lp_hist = numeric(R_keep)

	par_names = names(init)
	if (is.null(par_names)) {
		par_names = sprintf("par%d", seq_len(q))
	}
	colnames(par_hist) = par_names

	b = init
	logpb = log_post(b)
	stopifnot(length(logpb) == 1)
	V_chol = t(chol(proposal_scale * proposal_var))

	for (r in 1:R) {
		# Draw the jump sizes and uniforms for all groups
		bc = rmvnorm_chol(1, numeric(q), V_chol)
		u = runif(G)

		for (g in 1:G) {
			# b_ is the proposed draw for the g-th group
			idx_grp = grp_list[[g]]
			b_ = b
			b_[idx_grp] = b_[idx_grp] + bc[idx_grp]

			log_alpha = log_post(b_) - logpb
			if (is.na(log_alpha)) {
				warning("log_alpha = NA")
			} else if (log(u[g]) < log_alpha) {
				b[idx_grp] = b_[idx_grp]
				accept_grp[g] = accept_grp[g] + 1
				logpb = log_post(b)
			}
		}

		if (r > burn && r %% thin == 0) {
			idx_keep = idx_keep + 1
			par_hist[idx_keep,] = b
			lp_hist[idx_keep] = logpb
		}

		if (r %% report_period == 0) {
			acc = paste(sprintf("%0.02f", accept_grp / r * 100), collapse = ", ")
			logger("After %d rep, accept%% {%s}\n", r, acc)
		}
	}

	elapsed_sec = as.numeric(Sys.time() - start_time, "secs")

	out = list(par_hist = par_hist, lp_hist = lp_hist, accept = accept_grp / R,
		grp = grp, elapsed_sec = elapsed_sec)
	class(out) = "mh_rw_result"
	return(out)
}

#' @name mh_rw
#' @export
print.mh_rw_result = function(x, ...)
{
	printf("Metropolis-Hastings Random Walk\n")

	printf("--- Parameters ---\n")
	par_mcmc = x$par_hist

	DF = data.frame(
		mean = colMeans(par_mcmc),
		sd = apply(par_mcmc, 2, sd),
		lo = apply(par_mcmc, 2, quantile, prob = 0.025),
		hi = apply(par_mcmc, 2, quantile, prob = 0.975),
		grp = x$grp,
		accept = x$accept[x$grp] * 100
	)
	rownames(DF) = colnames(par_mcmc)
	colnames(DF) = c("Mean", "SD", "2.5%", "97.5%", "Group", "Accept%")
	DF[,1] = round(DF[,1], 4)
	DF[,2] = round(DF[,2], 4)
	DF[,3] = round(DF[,3], 4)
	DF[,4] = round(DF[,3], 4)
	print(DF)
	
	printf("--- Log-Posterior ---\n")
	lp_mcmc = x$lp_hist

	DF = data.frame(
		mean = mean(lp_mcmc),
		sd = sd(lp_mcmc),
		lo = quantile(lp_mcmc, prob = 0.025),
		hi = quantile(lp_mcmc, prob = 0.975)
	)
	rownames(DF) = "lp"
	colnames(DF) = c("Mean", "SD", "2.5%", "97.5%")
	DF[,1] = round(DF[,1], 4)
	DF[,2] = round(DF[,2], 4)
	DF[,3] = round(DF[,3], 4)
	DF[,4] = round(DF[,3], 4)
	print(DF)

	printf("---\n")
	printf("Elapsed Sec: %0.2f   ", x$elapsed_sec)
}

#' @name mh_rw
#' @export
mh_rw_transform = function(object, tx)
{
	stopifnot(class(object) == "mh_rw_result")

	par_mcmc = object$par_hist
	tx_mcmc = apply(par_mcmc, 1, tx)
	stop("Keep working here!! Also make a print function for this!!")

	out = list(par = par_tx, vcov = V_tx)
	class(out) = "mh_rw_transform_result"
	return(out)
}
