#' Build the dynamic parameter form for a given study design
#'
#' Returns the input widgets appropriate to design_key. Field
#' inputIds are shared across designs where the underlying meaning is
#' analogous (e.g. calc_p1 is "prevalence" for a cross-sectional
#' study but "outcome rate in controls" for a case-control study) --
#' this keeps server.R simple, since it can always read from the same
#' small set of inputIds regardless of which form is showing.
#'
#' @param design_key Character. One of the design keys produced by
#'   current_design() in server.R.
#' @return A shiny tag list of input widgets.
#' @keywords internal
build_calculator_form <- function(design_key) {

  # A dropout slider is used by every design, so it's built once here
  # and reused at the bottom of each form.
  dropout_input <- function(default = 20) {
    shiny::sliderInput("calc_dropout", "Expected dropout rate (%)",
                       min = 0, max = 50, value = default, step = 1)
  }

  conf_level_input <- function() {
    shiny::selectInput("calc_conflevel", "Confidence level",
                       choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                       selected = 0.95)
  }

  sig_level_input <- function() {
    shiny::selectInput("calc_siglevel", "Significance level (alpha)",
                       choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                       selected = 0.05)
  }

  error_type_input <- function() {
    shiny::radioButtons("calc_errortype", "Margin of error type",
                        choices = c("Absolute" = "absolute", "Relative" = "relative"),
                        selected = "absolute", inline = TRUE)
  }

  power_input <- function() {
    shiny::sliderInput("calc_power", "Power (%)",
                       min = 70, max = 99, value = 80, step = 1)
  }

  ratio_input <- function(label) {
    shiny::sliderInput("calc_ratio", label, min = 1, max = 5, value = 1, step = 1)
  }

  shiny::tagList(
    switch(
      design_key,

      "cross_binary" = shiny::tagList(
        shiny::sliderInput("calc_p1", "Expected proportion / prevalence",
                           min = 0.01, max = 0.99, value = 0.30, step = 0.01),
        shiny::sliderInput("calc_precision", "Precision (margin of error)",
                           min = 0.01, max = 0.20, value = 0.05, step = 0.01),
        error_type_input(),
        conf_level_input(),
        dropout_input()
      ),

      "cross_continuous" = shiny::tagList(
        shiny::numericInput("calc_mean", "Expected mean", value = 100),
        shiny::numericInput("calc_sd", "Expected standard deviation", value = 10, min = 0.01),
        shiny::numericInput("calc_precision", "Precision (margin of error)", value = 1, min = 0.01),
        error_type_input(),
        conf_level_input(),
        dropout_input()
      ),

      "casecontrol" = shiny::tagList(
        shiny::sliderInput("calc_p1", "Proportion exposed among controls",
                           min = 0.01, max = 0.99, value = 0.25, step = 0.01),
        shiny::sliderInput("calc_p2", "Proportion exposed among cases",
                           min = 0.01, max = 0.99, value = 0.40, step = 0.01),
        ratio_input("Control : case ratio"),
        power_input(),
        sig_level_input(),
        dropout_input()
      ),

      "cohort" = shiny::tagList(
        shiny::sliderInput("calc_p1", "Incidence in unexposed group",
                           min = 0.01, max = 0.99, value = 0.20, step = 0.01),
        shiny::sliderInput("calc_p2", "Incidence in exposed group",
                           min = 0.01, max = 0.99, value = 0.30, step = 0.01),
        ratio_input("Exposed : unexposed ratio"),
        power_input(),
        sig_level_input(),
        dropout_input()
      ),

      "trial_superiority" = shiny::tagList(
        shiny::sliderInput("calc_p1", "Outcome rate, standard treatment",
                           min = 0.01, max = 0.99, value = 0.45, step = 0.01),
        shiny::sliderInput("calc_p2", "Outcome rate, new treatment",
                           min = 0.01, max = 0.99, value = 0.61, step = 0.01),
        shiny::numericInput("calc_delta", "Superiority margin", value = 0.10, min = 0),
        shiny::radioButtons("calc_sided", "Test type",
                            choices = c("One-sided (matches paper's convention)" = 1,
                                        "Two-sided (current regulatory guidance)" = 2),
                            selected = 2),
        power_input(),
        sig_level_input(),
        dropout_input()
      ),

      "trial_noninferiority" = shiny::tagList(
        shiny::sliderInput("calc_p1", "Outcome rate, standard treatment",
                           min = 0.01, max = 0.99, value = 0.45, step = 0.01),
        shiny::sliderInput("calc_p2", "Outcome rate, new treatment",
                           min = 0.01, max = 0.99, value = 0.45, step = 0.01),
        shiny::numericInput("calc_delta", "Non-inferiority margin", value = 0.10, min = 0),
        power_input(),
        sig_level_input(),
        dropout_input()
      ),

      "trial_equivalence" = shiny::tagList(
        shiny::sliderInput("calc_p1", "Outcome rate, standard treatment",
                           min = 0.01, max = 0.99, value = 0.45, step = 0.01),
        shiny::sliderInput("calc_p2", "Outcome rate, new treatment",
                           min = 0.01, max = 0.99, value = 0.45, step = 0.01),
        shiny::numericInput("calc_delta", "Equivalence limit", value = 0.10, min = 0),
        power_input(),
        sig_level_input(),
        dropout_input()
      ),

      "diagnostic" = shiny::tagList(
        shiny::sliderInput("calc_p1", "Expected sensitivity",
                           min = 0.01, max = 0.99, value = 0.80, step = 0.01),
        shiny::sliderInput("calc_p2", "Expected specificity",
                           min = 0.01, max = 0.99, value = 0.90, step = 0.01),
        shiny::sliderInput("calc_prevalence", "Disease prevalence",
                           min = 0.01, max = 0.99, value = 0.20, step = 0.01),
        shiny::sliderInput("calc_precision", "Precision (margin of error)",
                           min = 0.01, max = 0.20, value = 0.05, step = 0.01),
        error_type_input(),
        conf_level_input(),
        dropout_input(default = 0)
      ),

      shiny::p("Please complete the Wizard first.")
    )
  )
}


#' The list of parameters that can be varied on the sensitivity plot,
#' for a given design
#'
#' @param design_key Character. One of the design keys produced by
#'   current_design() in server.R.
#' @return A named character vector suitable for selectInput(choices = ...).
#'   Values are generic field names used internally by server.R (not
#'   the form's inputIds), so the plot can vary one field in isolation.
#' Parameters available for the sensitivity plot, by design
#' @keywords internal
plot_parameter_choices <- function(design_key) {
  switch(
    design_key,
    "cross_binary"          = c("Proportion" = "p1", "Precision" = "precision"),
    "cross_continuous"      = c("Standard deviation" = "sd", "Precision" = "precision"),
    "casecontrol"           = c("Power" = "power", "Control:case ratio" = "ratio"),
    "cohort"                = c("Power" = "power", "Exposed:unexposed ratio" = "ratio"),
    "trial_superiority"     = c("Power" = "power", "Superiority margin" = "delta"),
    "trial_noninferiority"  = c("Power" = "power", "Non-inferiority margin" = "delta"),
    "trial_equivalence"     = c("Power" = "power", "Equivalence limit" = "delta"),
    "diagnostic"            = c("Precision" = "precision", "Prevalence" = "prevalence"),
    character(0)
  )
}
