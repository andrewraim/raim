#' Random Walk Metropolis-Hastings with Gaussian Proposal
#' 
#' This code was originally adapted from \code{rwmetrop} function in the
#' \code{LearnBayes} package.
#'
#' @param init Initial value of \eqn{\phi} for the sampler
#' @param log_post Log-posterior function of \eqn{\phi} with target distribution
#' @param proposal_var Variance matrix of proposal distribution
#' @param control An object created via \code{mh_rw_control}
#' @param proposal_scale A scalar multiple applied to \code{proposal_var}
#' @param grp Vector of integers specifying grouping of elements of \eqn{\phi}.
#' See details.
#' @param R Length of requested chain
#' @param burn Number of initial draws to burn / discard
#' @param thin Thinning factor; the period for saving draws
#' @param report_period Period for reporting MCMC progress
#' @param tx A transformation of the parameter \eqn{\phi}
#' @param ... Additional arguments
#' @param x Object to print
#' @param object Result of \code{mh_rw} or \code{txform}
#'
#' @returns A list with the following elements
#' \itemize{
#' \item{par}{Matrix of draws where the \eqn{q} columns correspond to the
#' parameters of \code{log_post}}
#' \item{accept}{Vector of acceptance rates for the groups defined in in
#' \code{grp}}
#' }
#' 
#' @details 
#' TBD: describe \code{grp}, proposal, \code{log_post}.
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
	stopifnot(length(grp) == q)
	
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
		bc = r_mvnorm_chol(1, numeric(q), V_chol)
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

		if (r %% report_period == 0 && G > 1) {
			acc = paste(sprintf("%0.02f", accept_grp / r * 100), collapse = ", ")
			logger("After %d rep, accept%% {%s}\n", r, acc)
		} else if (r %% report_period == 0 && G == 1) {
			logger("After %d rep, accept%% %0.02f\n", r, accept_grp / r * 100)
		}
	}

	elapsed_sec = as.numeric(Sys.time() - start_time, "secs")

	out = list(par_hist = par_hist, lp_hist = lp_hist, accept = accept_grp / R,
		grp = grp, elapsed_sec = elapsed_sec, R = R, burn = burn, thin = thin)
	class(out) = "mh_rw"
	return(out)
}

#' @name mh_rw
#' @export
summary.mh_rw = function(object, level = 0.05, ...)
{
	par_mcmc = object$par_hist
	lp_mcmc = object$lp_hist
	probs = c(level / 2, 0.5, 1 - level / 2)

	par_df = data.frame(
		mean = colMeans(par_mcmc),
		sd = apply(par_mcmc, 2, sd),
		lo = apply(par_mcmc, 2, quantile, prob = probs[1]),
		mid = apply(par_mcmc, 2, quantile, prob = probs[2]),
		hi = apply(par_mcmc, 2, quantile, prob = probs[3])
	)
	rownames(par_df) = colnames(par_mcmc)
	colnames(par_df) = c("Mean", "SD", sprintf("%g%%", probs * 100))

	lp_df = data.frame(
		mean = mean(lp_mcmc),
		sd = sd(lp_mcmc),
		lo = quantile(lp_mcmc, prob = probs[1]),
		mid = quantile(lp_mcmc, prob = probs[2]),
		hi = quantile(lp_mcmc, prob = probs[3])
	)
	rownames(lp_df) = "lp"
	colnames(lp_df) = c("Mean", "SD", sprintf("%g%%", probs * 100))

	list(par_df = par_df, lp_df = lp_df)
}


#' @name mh_rw
#' @export
print.mh_rw = function(x, ...)
{
	ss = summary(x)
	par_mcmc = x$par_hist
	lp_mcmc = x$lp_hist

	printf("Metropolis-Hastings Random Walk\n")

	printf("--- Parameters ---\n")

	DF = round(ss$par_df, 4)
	G = length(table(x$grp))
	if (G > 1) {
		# If more than one group was defined, report group memberships and
		# acceptance rates as columns in the table.
		DF$`Group` = x$grp
		DF$`Accept%` = x$accept[x$grp] * 100
	}
	print(DF)
	
	printf("--- Log-Posterior ---\n")
	DF = round(ss$lp_df, 4)
	print(DF)
	printf("---\n")

	printf("Chain Length: %d  Burn: %d  Thin: %d  Saved Draws: %d\n", x$R,
		x$burn, x$thin, nrow(par_mcmc))

	printf("Elapsed Sec: %0.2f", x$elapsed_sec)
	if (G == 1) {
		# If one group was defined, report global acceptance rate here.
		printf("  Accept%%: %0.2f", x$accept * 100)
	}
	printf("\n")
}

#' @name mh_rw
#' @export
parnames.mh_rw = function(object, ...)
{
	colnames(object$par_hist)
}

#' @name mh_rw
#' @export
`parnames<-.mh_rw` = function(object, value)
{
	object_name = deparse(substitute(object))
	d = ncol(object$par_hist)
	stopifnot(d == length(value))
	colnames(object$par_hist) = value
	assign(object_name, object, envir = parent.frame())
}

#' @name mh_rw
#' @export
txform.mh_rw = function(object, tx, ...)
{
	par_mcmc = object$par_hist
	R = nrow(par_mcmc)
	q = ncol(par_mcmc)
	tx_mcmc_list = apply(par_mcmc, 1, tx, simplify = FALSE)
	tx_mcmc = matrix(unlist(tx_mcmc_list), nrow = R, byrow = TRUE)
	colnames(tx_mcmc) = sprintf("txpar%d", seq_along_dim(tx_mcmc, 2))
	out = list(par_hist = tx_mcmc)
	class(out) = "mh_rw_txform"
	return(out)
}
