#' Sample size for a superiority trial (binary outcome)
#'
#' Estimates the sample size needed to demonstrate that a new
#' treatment is better than a standard treatment, following
#' Bhardwaj et al. (2024) and using epiR::epi.sssupb() as the
#' validated computational backend.
#'
#' @param p_standard Numeric between 0 and 1. Outcome rate in the
#'   standard/control treatment group.
#' @param p_new Numeric between 0 and 1. Outcome rate in the new
#'   treatment group.
#' @param delta Numeric >= 0. Superiority margin -- the minimum
#'   difference researchers want to be able to detect.
#' @param power Numeric between 0 and 1. Desired study power.
#'   Defaults to 0.80.
#' @param sig_level Numeric between 0 and 1. Significance level
#'   (alpha). Defaults to 0.05.
#' @param sided_test Either 1 or 2. Bhardwaj et al. (2024) use a
#'   one-sided test for superiority trials (the traditional
#'   convention). However, epiR's own documentation notes that
#'   "regulatory agencies and most clinical trial guidelines
#'   recommend two-sided tests for superiority trials" -- this is a
#'   genuine, unresolved difference of opinion in the literature, not
#'   a bug. Defaults to 2 (two-sided), matching current regulatory
#'   guidance; set to 1 to reproduce the paper's own worked example.
#' @param treat_control_ratio Numeric >= 1. Number in the treatment
#'   group per subject in the control group. Defaults to 1.
#' @param dropout_rate Numeric between 0 and 1. Defaults to 0.20.
#'
#' @return A list with n_raw, n_final, and the inputs used.
#'
#' @examples
#' # Reproduces the worked cancer survival example in
#' # Bhardwaj et al. (2024) -- use sided_test = 1 to match their
#' # one-sided convention exactly
#' calc_trial_superiority(p_standard = 0.45, p_new = 0.61,
#'                         delta = 0.10, sided_test = 1)
#'
#' @export
calc_trial_superiority <- function(p_standard,
                                   p_new,
                                   delta,
                                   power = 0.80,
                                   sig_level = 0.05,
                                   sided_test = 2,
                                   treat_control_ratio = 1,
                                   dropout_rate = 0.20) {

  .check_trial_inputs(p_standard, p_new, delta, treat_control_ratio)
  if (!sided_test %in% c(1, 2)) {
    stop("sided_test must be either 1 or 2.")
  }

  result <- epiR::epi.sssupb(
    treat       = p_new,
    control     = p_standard,
    delta       = delta,
    n           = NA,
    power       = power,
    r           = treat_control_ratio,
    sided.test  = sided_test,
    nfractional = FALSE,
    alpha       = sig_level
  )

  n_raw   <- result$n.total
  n_final <- apply_dropout(n_raw, dropout_rate)

  list(
    design        = "Clinical trial (superiority, binary outcome)",
    outcome_type  = "binary",
    inputs        = list(p_standard = p_standard, p_new = p_new,
                         delta = delta, power = power,
                         sig_level = sig_level, sided_test = sided_test,
                         treat_control_ratio = treat_control_ratio,
                         dropout_rate = dropout_rate),
    n_raw         = n_raw,
    n_final       = n_final
  )
}


#' Sample size for a non-inferiority trial (binary outcome)
#'
#' Estimates the sample size needed to demonstrate that a new
#' treatment is not unacceptably worse than a standard treatment,
#' using epiR::epi.ssninfb() as the validated computational backend.
#'
#' @inheritParams calc_trial_superiority
#' @param delta Numeric >= 0. Non-inferiority margin -- the maximum
#'   acceptable drop in outcome rate for the new treatment to still
#'   be considered non-inferior. This is the parameter most often
#'   mis-specified, so double-check it reflects a clinically (not
#'   just statistically) meaningful difference.
#'
#' @return A list with n_raw, n_final, and the inputs used.
#'
#' @examples
#' calc_trial_noninferiority(p_standard = 0.45, p_new = 0.45,
#'                            delta = 0.10)
#'
#' @export
calc_trial_noninferiority <- function(p_standard,
                                      p_new,
                                      delta,
                                      power = 0.80,
                                      sig_level = 0.05,
                                      treat_control_ratio = 1,
                                      dropout_rate = 0.20) {

  .check_trial_inputs(p_standard, p_new, delta, treat_control_ratio)
  result <- epiR::epi.ssninfb(
    treat       = p_new,
    control     = p_standard,
    delta       = delta,
    n           = NA,
    power       = power,
    r           = treat_control_ratio,
    nfractional = FALSE,
    alpha       = sig_level
  )

  n_raw   <- result$n.total
  n_final <- apply_dropout(n_raw, dropout_rate)

  list(
    design        = "Clinical trial (non-inferiority, binary outcome)",
    outcome_type  = "binary",
    inputs        = list(p_standard = p_standard, p_new = p_new,
                         delta = delta, power = power,
                         sig_level = sig_level,
                         treat_control_ratio = treat_control_ratio,
                         dropout_rate = dropout_rate),
    n_raw         = n_raw,
    n_final       = n_final
  )
}


#' Sample size for an equivalence trial (binary outcome)
#'
#' Estimates the sample size needed to demonstrate that two
#' treatments are, for practical purposes, equally effective, using
#' epiR::epi.ssequb() as the validated computational backend.
#'
#' @inheritParams calc_trial_superiority
#' @param delta Numeric >= 0. Equivalence limit -- the maximum
#'   difference in either direction still considered "equivalent".
#'
#' @return A list with n_raw, n_final, and the inputs used.
#'
#' @examples
#' calc_trial_equivalence(p_standard = 0.45, p_new = 0.45,
#'                         delta = 0.10)
#'
#' @export
calc_trial_equivalence <- function(p_standard,
                                   p_new,
                                   delta,
                                   power = 0.80,
                                   sig_level = 0.05,
                                   treat_control_ratio = 1,
                                   dropout_rate = 0.20) {

  .check_trial_inputs(p_standard, p_new, delta, treat_control_ratio)

  result <- epiR::epi.ssequb(
    treat       = p_new,
    control     = p_standard,
    delta       = delta,
    n           = NA,
    power       = power,
    r           = treat_control_ratio,
    nfractional = FALSE,
    alpha       = sig_level
  )

  n_raw   <- result$n.total
  n_final <- apply_dropout(n_raw, dropout_rate)

  list(
    design        = "Clinical trial (equivalence, binary outcome)",
    outcome_type  = "binary",
    inputs        = list(p_standard = p_standard, p_new = p_new,
                         delta = delta, power = power,
                         sig_level = sig_level,
                         treat_control_ratio = treat_control_ratio,
                         dropout_rate = dropout_rate),
    n_raw         = n_raw,
    n_final       = n_final
  )
}


# Internal helper: shared input validation for all three trial
# functions above. Not exported -- this is package-internal only,
# so it doesn't need full roxygen documentation, just a short note.
.check_trial_inputs <- function(p_standard, p_new, delta, treat_control_ratio) {
  if (!is.numeric(p_standard) || p_standard <= 0 || p_standard >= 1) {
    stop("p_standard must be a number between 0 and 1 (exclusive).")
  }
  if (!is.numeric(p_new) || p_new <= 0 || p_new >= 1) {
    stop("p_new must be a number between 0 and 1 (exclusive).")
  }
  if (!is.numeric(delta) || delta < 0) {
    stop("delta must be a non-negative number.")
  }
  if (!is.numeric(treat_control_ratio) || treat_control_ratio < 1) {
    stop("treat_control_ratio must be a number >= 1.")
  }
}
