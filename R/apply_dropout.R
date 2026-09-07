#' Adjust a raw sample size for expected dropout
#'
#' Studies with follow-up, non-response, or attrition need to enrol
#' more participants than the raw formula suggests, so that enough
#' complete observations remain at the end. This function applies the
#' statistically correct inflation formula:
#'
#'   n* = n / (1 - dropout_rate)
#'
#' Note: Bhardwaj et al. (2024), the primary reference for this
#' package, states this same formula in the text, but their worked
#' numeric examples actually use the simpler approximation
#' n * (1 + dropout_rate), which gives a slightly smaller (less
#' conservative) sample size. This package follows the formula as
#' stated in the text, not the arithmetic in the worked examples.
#' See the package Validation vignette for a side-by-side comparison.
#'
#' @param n Numeric. The raw (uninflated) sample size, before
#'   accounting for dropout.
#' @param dropout_rate Numeric between 0 and 1 (not inclusive of 1).
#'   Expected proportion of participants lost to follow-up or
#'   non-response. Defaults to 0.20 (20\%), the conventional default
#'   used throughout the sample size literature.
#'
#' @return A single integer: the dropout-adjusted sample size,
#'   rounded up to the nearest whole participant.
#'
#' @examples
#' apply_dropout(323, dropout_rate = 0.10)
#'
#' @export
apply_dropout <- function(n, dropout_rate = 0.20) {

  # Fail loudly on bad input rather than silently returning a
  # meaningless number -- this function feeds directly into every
  # other calculation in the package, so mistakes here would be
  # invisible everywhere else.
  if (!is.numeric(n) || length(n) != 1 || n <= 0) {
    stop("n must be a single positive number.")
  }
  if (!is.numeric(dropout_rate) || length(dropout_rate) != 1 ||
      dropout_rate < 0 || dropout_rate >= 1) {
    stop("dropout_rate must be a single number between 0 and 1 (e.g. 0.20 for 20%).")
  }

  n_adjusted <- n / (1 - dropout_rate)

  # Sample sizes are always rounded UP: a fractional participant
  # cannot be enrolled, and rounding down would leave the study
  # slightly under-powered.
  ceiling(n_adjusted)
}
