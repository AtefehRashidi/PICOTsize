#' Sample size for a cohort study
#'
#' Estimates the number of exposed and unexposed subjects needed to
#' detect a difference in incidence between two groups, following
#' Bhardwaj et al. (2024) and using epiR::epi.sscohortc() as the
#' validated computational backend.
#'
#' @param incidence_unexposed Numeric between 0 and 1. Expected
#'   incidence of the outcome in the unexposed group (p0 in the
#'   paper's notation).
#' @param incidence_exposed Numeric between 0 and 1. Expected
#'   incidence of the outcome in the exposed group (p1 in the
#'   paper's notation).
#' @param exposed_unexposed_ratio Numeric >= 1. Number of exposed
#'   subjects per unexposed subject. Defaults to 1 (equal groups).
#' @param power Numeric between 0 and 1. Desired study power.
#'   Defaults to 0.80 (80\%).
#' @param sig_level Numeric between 0 and 1. Significance level
#'   (alpha). Defaults to 0.05.
#' @param dropout_rate Numeric between 0 and 1. Defaults to 0.20.
#'
#' @return A list with n_raw (total, before dropout adjustment),
#'   n_final (total, after dropout adjustment), and the inputs
#'   used.
#'
#' @examples
#' # Reproduces the worked air pollution / asthma example in
#' # Bhardwaj et al. (2024): 20\% incidence unexposed, 30\% incidence
#' # exposed, equal groups, 80\% power, 10\% dropout
#' calc_cohort(incidence_unexposed = 0.20, incidence_exposed = 0.30,
#'             dropout_rate = 0.10)
#'
#' @export
calc_cohort <- function(incidence_unexposed,
                        incidence_exposed,
                        exposed_unexposed_ratio = 1,
                        power = 0.80,
                        sig_level = 0.05,
                        dropout_rate = 0.20) {

  if (!is.numeric(incidence_unexposed) || incidence_unexposed <= 0 || incidence_unexposed >= 1) {
    stop("incidence_unexposed must be a number between 0 and 1 (exclusive).")
  }
  if (!is.numeric(incidence_exposed) || incidence_exposed <= 0 || incidence_exposed >= 1) {
    stop("incidence_exposed must be a number between 0 and 1 (exclusive).")
  }
  if (!is.numeric(exposed_unexposed_ratio) || exposed_unexposed_ratio < 1) {
    stop("exposed_unexposed_ratio must be a number >= 1.")
  }

  result <- epiR::epi.sscohortc(
    N                 = NA,
    irexp1            = incidence_exposed,
    irexp0            = incidence_unexposed,
    pexp              = NA,     # only needed when population-level exposure prevalence matters; not used here
    n                 = NA,     # NA means "solve for n"
    power             = power,
    r                 = exposed_unexposed_ratio,
    design             = 1,
    sided.test        = 2,
    finite.correction = FALSE,
    nfractional       = FALSE,
    conf.level        = 1 - sig_level
  )

  n_raw   <- result$n.total
  n_final <- apply_dropout(n_raw, dropout_rate)

  list(
    design        = "Cohort",
    outcome_type  = "binary",
    inputs        = list(incidence_unexposed = incidence_unexposed,
                         incidence_exposed = incidence_exposed,
                         exposed_unexposed_ratio = exposed_unexposed_ratio,
                         power = power, sig_level = sig_level,
                         dropout_rate = dropout_rate),
    n_raw         = n_raw,
    n_final       = n_final
  )
}
