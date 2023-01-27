#' Random Walk Metropolis-Hastings with Gaussian Proposal
#' 
#' This code was originally adapted from \code{rwmetrop} function in the
#' \code{LearnBayes} package
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

#' @export
mh_rw = function(init, log_post, proposal_var, control = mh_rw_control())
{
	q = length(init)
	stopifnot(q == nrow(proposal_var))
	stopifnot(q == ncol(proposal_var))

	stopifnot(class(control) == "mh_rw_control")
	R = control$R
	burn = control$burn
	thin = control$thin
	proposal_scale = control$proposal_scale
	grp = control$grp

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

	R_keep = ceiling((R - burn) / thin)
	idx_keep = 0
	par_hist = matrix(NA, R_keep, q)

	b = init
	logfb = log_post(b)
	V_chol = proposal$scale * t(chol(proposal$var))

	for (r in 1:R) {
		bc = rmvnorm_chol(1, numeric(q), V_chol)

		for (g in 1:G) {
			idx_grp = grp_list[[g]]
			b_ = b
			b_[idx_grp] = b_[idx_grp] + bc[idx_grp]

			log_alpha = log_post(b_, Data) - logfb
			if (!is.na(log_alpha)) {
				if (log(runif(1)) < log_alpha) {
					b[idx_grp] = b_[idx_grp]
					accept_grp[g] = accept_grp[g] + 1
					logfb = log_post(b, Data)
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
