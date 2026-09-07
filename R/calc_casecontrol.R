#' Sample size for a case-control study
#'
#' Estimates the number of cases and controls needed to detect an
#' association between an exposure and an outcome, following
#' Bhardwaj et al. (2024) and using epiR::epi.sscc() as the
#' validated computational backend.
#'
#' Bhardwaj et al. (2024) express the exposure/outcome association
#' in terms of the proportion exposed among cases and among controls
#' (p1 and p0). epiR's epi.sscc() instead takes an odds ratio (OR).
#' This function converts p1/p0 to an OR internally, so the user can
#' keep thinking in the same terms as the source paper.
#'
#' @param p_exposed_controls Numeric between 0 and 1. Proportion
#'   exposed among controls (p0 in the paper's notation).
#' @param p_exposed_cases Numeric between 0 and 1. Proportion exposed
#'   among cases (p1 in the paper's notation).
#' @param control_case_ratio Numeric >= 1. Number of controls per
#'   case. Defaults to 1 (equal numbers of cases and controls).
#' @param power Numeric between 0 and 1. Desired study power.
#'   Defaults to 0.80 (80\%), matching the paper's convention.
#' @param sig_level Numeric between 0 and 1. Significance level
#'   (alpha). Defaults to 0.05.
#' @param dropout_rate Numeric between 0 and 1. Defaults to 0.20.
#'
#' @return A list with n_raw (total, before dropout adjustment),
#'   n_final (total, after dropout adjustment), the derived odds
#'   ratio, and the inputs used.
#'
#' @examples
#' # Reproduces the worked lymphoma example in Bhardwaj et al. (2024):
#' # 25\% exposed in controls, 40\% exposed in cases, equal groups,
#' # 80\% power, 10\% dropout
#' calc_casecontrol(p_exposed_controls = 0.25, p_exposed_cases = 0.40,
#'                   dropout_rate = 0.10)
#'
#' @export
calc_casecontrol <- function(p_exposed_controls,
                             p_exposed_cases,
                             control_case_ratio = 1,
                             power = 0.80,
                             sig_level = 0.05,
                             dropout_rate = 0.20) {

  if (!is.numeric(p_exposed_controls) || p_exposed_controls <= 0 || p_exposed_controls >= 1) {
    stop("p_exposed_controls must be a number between 0 and 1 (exclusive).")
  }
  if (!is.numeric(p_exposed_cases) || p_exposed_cases <= 0 || p_exposed_cases >= 1) {
    stop("p_exposed_cases must be a number between 0 and 1 (exclusive).")
  }
  if (!is.numeric(control_case_ratio) || control_case_ratio < 1) {
    stop("control_case_ratio must be a number >= 1.")
  }

  # Convert the two proportions to an odds ratio, since that is what
  # epi.sscc() expects. Odds = p / (1 - p).
  odds_cases    <- p_exposed_cases    / (1 - p_exposed_cases)
  odds_controls <- p_exposed_controls / (1 - p_exposed_controls)
  or_derived    <- odds_cases / odds_controls

  # sig_level is a two-sided alpha in epi.sscc() (sided.test = 2),
  # matching the convention used throughout Bhardwaj et al. (2024).
  result <- epiR::epi.sscc(
    N           = NA,
    OR          = or_derived,
    p1          = NA,        # only needed for the Fleiss correction, which we don't use
    p0          = p_exposed_controls,
    n           = NA,        # NA here means "solve for n"
    power       = power,
    r           = control_case_ratio,
    phi.coef    = 0,
    design      = 1,
    sided.test  = 2,
    nfractional = FALSE,
    conf.level  = 1 - sig_level,
    method      = "unmatched",
    fleiss      = FALSE
  )

  n_raw   <- result$n.total
  n_final <- apply_dropout(n_raw, dropout_rate)

  list(
    design        = "Case-control",
    outcome_type  = "binary",
    inputs        = list(p_exposed_controls = p_exposed_controls,
                         p_exposed_cases = p_exposed_cases,
                         control_case_ratio = control_case_ratio,
                         power = power, sig_level = sig_level,
                         dropout_rate = dropout_rate),
    or_derived    = or_derived,
    n_raw         = n_raw,
    n_final       = n_final
  )
}
