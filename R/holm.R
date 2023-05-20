#' Holm's Procedure for Multiple Comparisons
#'
#' @param p vector of \eqn{m} p-values.
#' @param alpha Family-wise error rate.
#'
#' @return Vector of indices \eqn{\mathcal{I}} such that
#' \eqn{H_{0i}: i \in \mathcal{I}} can be rejected. Result is empty if none can
#' be rejected.
#' 
#' @examples
#' p = c(0.101, 0.05, 0.03)
#' holm(p, 0.15)
#' holm(p, 0.10)
#' holm(p, 0.05)
#' 
#' @references
#' Holm, S. (1979). A Simple Sequentially Rejective Multiple Test Procedure.
#' Scandinavian Journal of Statistics, 6(2), 65-70.
#' 
#' @export
holm = function(p, alpha)
{
	m = length(p)
	idx_p = order(p)
	alpha_adj = alpha / (m + 1 - 1:m)

    k = m + 1
	idx_rej = which(p[idx_p] > alpha_adj)
	if (length(idx_rej) > 0) {
		k = min(idx_rej)
	}

	idx_p[seq_len(k-1)]
}
