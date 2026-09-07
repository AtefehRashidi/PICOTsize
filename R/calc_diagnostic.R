#' Sample size to estimate the sensitivity and specificity of a
#' diagnostic test
#'
#' Estimates the sample size needed to evaluate a diagnostic test's
#' accuracy, using epiR::epi.ssdxsesp() as the validated
#' computational backend. That function implements the method of
#' Buderer (1996) and Hajian-Tilaki (2014), which calculates the
#' required n for sensitivity and for specificity separately, then
#' takes the LARGER of the two as the total study sample size --
#' because the same group of subjects is used to estimate both
#' simultaneously, not two separate samples.
#'
#' Note this differs from Bhardwaj et al. (2024), who calculate the
#' two requirements separately and add them together. Summing
#' effectively assumes two independent studies, which is not how
#' diagnostic accuracy studies are actually run. This package
#' follows the epiR/Buderer approach (the larger of the two), as
#' the methodologically correct one. See the package Validation
#' vignette for a side-by-side comparison.
#'
#' @param expected_sensitivity Numeric between 0 and 1. Prior
#'   estimate of the test's sensitivity.
#' @param expected_specificity Numeric between 0 and 1. Prior
#'   estimate of the test's specificity.
#' @param prevalence Numeric between 0 and 1. Expected prevalence of
#'   the disease/condition in the study population.
#' @param precision Numeric. Acceptable margin of error for the
#'   estimate (absolute, unless error_type = "relative").
#' @param error_type Character. Either "absolute" or "relative".
#'   Defaults to "absolute", matching the worked example in
#'   Bhardwaj et al. (2024).
#' @param conf_level Numeric. Confidence level, e.g. 0.95 for 95\%.
#' @param dropout_rate Numeric between 0 and 1. Defaults to 0, since
#'   Bhardwaj et al. (2024) do not apply a dropout adjustment to
#'   their diagnostic test example. Set to a positive value if your
#'   study design calls for one.
#'
#' @return A list with n_sensitivity, n_specificity, n_raw
#'   (the larger of the two -- the actual required enrolment),
#'   n_final (dropout-adjusted), and the inputs used.
#'
#' @examples
#' # Reproduces the worked hypertension test example in
#' # Bhardwaj et al. (2024): sensitivity 80\%, specificity 90\%,
#' # prevalence 20\%, 5\% absolute margin of error
#' calc_diagnostic_accuracy(expected_sensitivity = 0.80,
#'                           expected_specificity = 0.90,
#'                           prevalence = 0.20, precision = 0.05)
#'
#' @export
calc_diagnostic_accuracy <- function(expected_sensitivity,
                                     expected_specificity,
                                     prevalence,
                                     precision,
                                     error_type = "absolute",
                                     conf_level = 0.95,
                                     dropout_rate = 0) {

  if (!is.numeric(expected_sensitivity) || expected_sensitivity <= 0 || expected_sensitivity >= 1) {
    stop("expected_sensitivity must be a number between 0 and 1 (exclusive).")
  }
  if (!is.numeric(expected_specificity) || expected_specificity <= 0 || expected_specificity >= 1) {
    stop("expected_specificity must be a number between 0 and 1 (exclusive).")
  }
  if (!is.numeric(prevalence) || prevalence <= 0 || prevalence >= 1) {
    stop("prevalence must be a number between 0 and 1 (exclusive).")
  }
  if (!error_type %in% c("absolute", "relative")) {
    stop('error_type must be either "absolute" or "relative".')
  }

  result <- epiR::epi.ssdxsesp(
    se          = expected_sensitivity,
    sp          = expected_specificity,
    Py          = prevalence,
    epsilon     = precision,
    error       = error_type,
    nfractional = FALSE,
    conf.level  = conf_level
  )

  # epi.ssdxsesp() returns a one-row data frame; pull the columns we need.
  n_sensitivity <- result$se.n
  n_specificity <- result$sp.n
  n_raw         <- result$total.n   # the larger of se.n and sp.n

  n_final <- if (dropout_rate > 0) apply_dropout(n_raw, dropout_rate) else n_raw
  list(
    design            = "Diagnostic test accuracy",
    outcome_type      = "diagnostic",
    inputs            = list(expected_sensitivity = expected_sensitivity,
                             expected_specificity = expected_specificity,
                             prevalence = prevalence, precision = precision,
                             error_type = error_type, conf_level = conf_level,
                             dropout_rate = dropout_rate),
    n_sensitivity     = n_sensitivity,
    n_specificity     = n_specificity,
    n_raw             = n_raw,
    n_final           = n_final
  )
}
