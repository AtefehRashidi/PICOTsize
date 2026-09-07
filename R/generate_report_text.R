#' Generate a Methods-section paragraph from a sample size result
#'
#' Takes the list returned by any of the calc_* functions in this
#' package and produces a ready-to-use paragraph describing the
#' sample size calculation, written in the style of a Methods
#' section, following Bhardwaj et al. (2024)'s own reporting
#' conventions.
#'
#' @param result A list, as returned by calc_crosssectional_binary(),
#'   calc_crosssectional_continuous(), calc_casecontrol(),
#'   calc_cohort(), calc_trial_superiority(),
#'   calc_trial_noninferiority(), calc_trial_equivalence(), or
#'   calc_diagnostic_accuracy().
#'
#' @return A single character string containing the report paragraph.
#'
#' @examples
#' result <- calc_crosssectional_binary(p = 0.30, precision = 0.05,
#'                                       dropout_rate = 0.10)
#' generate_report_text(result)
#'
#' @export
generate_report_text <- function(result) {

  if (!is.list(result) || is.null(result$design)) {
    stop("result must be a list returned by one of this package's calc_* functions.")
  }

  # Dispatch to a design-specific sentence builder. Using the
  # design field (already a human-readable string) keeps this
  # simple -- no need for a separate design code/enum.
  body <- switch(
    result$design,
    "Cross-sectional (binary outcome)"                   = .report_crosssectional_binary(result),
    "Cross-sectional (continuous outcome)"                = .report_crosssectional_continuous(result),
    "Case-control"                                        = .report_casecontrol(result),
    "Cohort"                                               = .report_cohort(result),
    "Clinical trial (superiority, binary outcome)"        = .report_trial(result, "superiority"),
    "Clinical trial (non-inferiority, binary outcome)"    = .report_trial(result, "non-inferiority"),
    "Clinical trial (equivalence, binary outcome)"        = .report_trial(result, "equivalence"),
    "Diagnostic test accuracy"                             = .report_diagnostic(result),
    stop("Unrecognised design: ", result$design)
  )

  body
}


# ---- Internal sentence builders, one per design ---------------------
# Each of these is package-internal (not exported) and returns a
# single paragraph as a character string. Splitting them out keeps
# generate_report_text() itself short and easy to scan.

.report_crosssectional_binary <- function(result) {
  i <- result$inputs
  sprintf(
    paste(
      "Assuming a two-sided significance level of %.0f%% and a confidence",
      "level of %.0f%%, with an expected proportion of %.2f and an",
      "%s margin of error of %.2f, the raw sample size was calculated",
      "as %d. After adjusting for an expected %.0f%% dropout rate, the",
      "final required sample size was %d participants."
    ),
    5, i$conf_level * 100, i$p, i$error_type, i$precision,
    result$n_raw, i$dropout_rate * 100, result$n_final
  )
}

.report_crosssectional_continuous <- function(result) {
  i <- result$inputs
  sprintf(
    paste(
      "Assuming a confidence level of %.0f%%, with an expected mean of",
      "%.2f, an expected standard deviation of %.2f, and an %s margin",
      "of error of %.2f, the raw sample size was calculated as %d.",
      "After adjusting for an expected %.0f%% dropout rate, the final",
      "required sample size was %d participants."
    ),
    i$conf_level * 100, i$mean, i$sd, i$error_type, i$precision,
    result$n_raw, i$dropout_rate * 100, result$n_final
  )
}

.report_casecontrol <- function(result) {
  i <- result$inputs
  sprintf(
    paste(
      "Assuming an expected exposure prevalence of %.0f%% among",
      "controls and %.0f%% among cases (equivalent to an odds ratio",
      "of %.2f), a control-to-case ratio of %d, %.0f%% power, and a",
      "significance level of %.2f, the raw sample size was calculated",
      "as %d. After adjusting for an expected %.0f%% dropout rate, the",
      "final required sample size was %d participants."
    ),
    i$p_exposed_controls * 100, i$p_exposed_cases * 100, result$or_derived,
    i$control_case_ratio, i$power * 100, i$sig_level,
    result$n_raw, i$dropout_rate * 100, result$n_final
  )
}

.report_cohort <- function(result) {
  i <- result$inputs
  sprintf(
    paste(
      "Assuming an expected incidence of %.0f%% in the unexposed group",
      "and %.0f%% in the exposed group, an exposed-to-unexposed ratio",
      "of %d, %.0f%% power, and a significance level of %.2f, the raw",
      "sample size was calculated as %d. After adjusting for an",
      "expected %.0f%% dropout rate, the final required sample size",
      "was %d participants."
    ),
    i$incidence_unexposed * 100, i$incidence_exposed * 100,
    i$exposed_unexposed_ratio, i$power * 100, i$sig_level,
    result$n_raw, i$dropout_rate * 100, result$n_final
  )
}

.report_trial <- function(result, trial_type) {
  i <- result$inputs
  sided_note <- if (!is.null(i$sided_test)) {
    sprintf(" using a %s test", if (i$sided_test == 1) "one-sided" else "two-sided")
  } else {
    ""
  }
  sprintf(
    paste(
      "For a %s trial comparing a standard treatment (outcome rate",
      "%.0f%%) with a new treatment (outcome rate %.0f%%), assuming a",
      "margin of %.2f, %.0f%% power, and a significance level of",
      "%.2f%s, the raw sample size was calculated as %d. After",
      "adjusting for an expected %.0f%% dropout rate, the final",
      "required sample size was %d participants."
    ),
    trial_type, i$p_standard * 100, i$p_new * 100, i$delta,
    i$power * 100, i$sig_level, sided_note,
    result$n_raw, i$dropout_rate * 100, result$n_final
  )
}

.report_diagnostic <- function(result) {
  i <- result$inputs
  sprintf(
    paste(
      "Assuming an expected sensitivity of %.0f%% and specificity of",
      "%.0f%%, a disease prevalence of %.0f%%, and an %s margin of",
      "error of %.2f, the required sample size was %d for sensitivity",
      "and %d for specificity. Following Buderer (1996), the larger",
      "of the two (%d) was taken as the total required sample size,",
      "as both are estimated from the same study subjects."
    ),
    i$expected_sensitivity * 100, i$expected_specificity * 100,
    i$prevalence * 100, i$error_type, i$precision,
    result$n_sensitivity, result$n_specificity, result$n_raw
  )
}
