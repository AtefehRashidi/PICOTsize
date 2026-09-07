#' Sample size for a cross-sectional study with a binary outcome
#'
#' Estimates the sample size needed to estimate a proportion or
#' prevalence in a population, following Bhardwaj et al. (2024) and
#' using epiR::epi.sssimpleestb() as the validated computational
#' backend.
#'
#' @param p Numeric between 0 and 1. Expected proportion/prevalence
#'   in the population (from prior studies or a pilot study).
#' @param precision Numeric. Acceptable margin of error (absolute,
#'   in the same 0-1 scale as p, unless error_type = "relative").
#' @param error_type Character. Either "absolute" or "relative".
#'   Defaults to "absolute", matching the worked examples in
#'   Bhardwaj et al. (2024).
#' @param conf_level Numeric. Confidence level, e.g. 0.95 for 95\%.
#' @param dropout_rate Numeric between 0 and 1. Expected dropout /
#'   non-response rate. Defaults to 0.20. Set to 0 to skip dropout
#'   adjustment entirely.
#'
#' @return A list with the raw sample size (n_raw), the
#'   dropout-adjusted sample size (n_final), and the inputs used,
#'   so the result can be fed directly into report generation.
#'
#' @examples
#' # Reproduces the worked example in Bhardwaj et al. (2024):
#' # prevalence 30\%, 5\% absolute precision, 95\% CI, 10\% dropout
#' calc_crosssectional_binary(p = 0.30, precision = 0.05,
#'                             dropout_rate = 0.10)
#'
#' @export
calc_crosssectional_binary <- function(p,
                                       precision,
                                       error_type = "absolute",
                                       conf_level = 0.95,
                                       dropout_rate = 0.20) {

  if (!is.numeric(p) || p <= 0 || p >= 1) {
    stop("p must be a number between 0 and 1 (exclusive).")
  }
  if (!error_type %in% c("absolute", "relative")) {
    stop('error_type must be either "absolute" or "relative".')
  }

  # N = NA tells epiR to assume an unlimited population (no finite
  # population correction) -- this is the documented way to do it,
  # not just a very large number.
  n_raw <- epiR::epi.sssimpleestb(
    N           = NA,
    Py          = p,
    epsilon     = precision,
    error       = error_type,
    se          = 1,
    sp          = 1,
    nfractional = FALSE,
    conf.level  = conf_level
  )

  n_final <- apply_dropout(n_raw, dropout_rate)

  list(
    design        = "Cross-sectional (binary outcome)",
    outcome_type  = "binary",
    inputs        = list(p = p, precision = precision,
                         error_type = error_type,
                         conf_level = conf_level,
                         dropout_rate = dropout_rate),
    n_raw         = n_raw,
    n_final       = n_final
  )
}


#' Sample size for a cross-sectional study with a continuous outcome
#'
#' Estimates the sample size needed to estimate a population mean,
#' following Bhardwaj et al. (2024) and using
#' epiR::epi.sssimpleestc() as the validated computational backend.
#'
#' @param mean Numeric. Expected mean of the outcome (from prior
#'   studies or a pilot study). Required by the epiR backend even
#'   when error_type = "absolute".
#' @param sd Numeric. Expected standard deviation of the outcome.
#' @param precision Numeric. Acceptable margin of error. In the same
#'   units as sd if error_type = "absolute", or as a fraction of
#'   mean if error_type = "relative".
#' @param error_type Character. Either "absolute" or "relative".
#'   Defaults to "absolute", matching the worked examples in
#'   Bhardwaj et al. (2024). Note this differs from epiR's own
#'   default ("relative") -- we override it here.
#' @param conf_level Numeric. Confidence level, e.g. 0.95 for 95\%.
#' @param dropout_rate Numeric between 0 and 1. Defaults to 0.20.
#'
#' @return A list with the same structure as
#'   [calc_crosssectional_binary()].
#'
#' @examples
#' # Reproduces the worked SBP example in Bhardwaj et al. (2024):
#' # mean SBP unspecified in the paper's formula, SD = 3 mmHg,
#' # precision = 0.5 mmHg, 95\% CI, 10\% dropout
#' calc_crosssectional_continuous(mean = 120, sd = 3, precision = 0.5,
#'                                 dropout_rate = 0.10)
#'
#' @export
calc_crosssectional_continuous <- function(mean,
                                           sd,
                                           precision,
                                           error_type = "absolute",
                                           conf_level = 0.95,
                                           dropout_rate = 0.20) {

  if (!is.numeric(mean)) {
    stop("mean must be provided as a number (required by the epiR backend).")
  }
  if (!is.numeric(sd) || sd <= 0) {
    stop("sd must be a positive number.")
  }
  if (!is.numeric(precision) || precision <= 0) {
    stop("precision must be a positive number.")
  }
  if (!error_type %in% c("absolute", "relative")) {
    stop('error_type must be either "absolute" or "relative".')
  }

  n_raw <- epiR::epi.sssimpleestc(
    N           = NA,
    xbar        = mean,
    sigma       = sd,
    epsilon     = precision,
    error       = error_type,
    nfractional = FALSE,
    conf.level  = conf_level
  )

  n_final <- apply_dropout(n_raw, dropout_rate)

  list(
    design        = "Cross-sectional (continuous outcome)",
    outcome_type  = "continuous",
    inputs        = list(mean = mean, sd = sd, precision = precision,
                         error_type = error_type,
                         conf_level = conf_level,
                         dropout_rate = dropout_rate),
    n_raw         = n_raw,
    n_final       = n_final
  )
}
