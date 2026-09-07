#' UI for the PICOT Wizard tab
#'
#' Builds the step-by-step questionnaire that walks the user through
#' the PICOT framework and determines which study design (and
#' therefore which calculator form) applies. This is a UI-building
#' function, not a static object, so it can be called from ui.R.
#'
#' @return A shiny tag list, ready to be placed inside a nav_panel().
#' @keywords internal
ui_wizard <- function() {
  bslib::layout_column_wrap(
    width = "400px",

    # ---- P: Population -------------------------------------------------
    bslib::card(
      bslib::card_header("P-Population"),
      shiny::textInput(
        inputId = "wizard_population",
        label   = "Briefly describe the study population",
        placeholder = "e.g. children aged 1 to 5 years in an urban community"
      )
    ),

    # ---- I/C: determines the study design -------------------------------
    bslib::card(
      bslib::card_header("I / C-Intervention & Comparison"),
      shiny::radioButtons(
        inputId = "wizard_comparison",
        label   = "Are you comparing two groups, or estimating a single value in the population?",
        choices = c(
          "Estimating a single value (e.g. prevalence, mean)" = "single",
          "Comparing two groups"                              = "compare"
        ),
        selected = character(0)
      ),

      # Only shown once "compare" is selected -- see server.R for the
      # logic that reveals this conditional panel.
      shiny::conditionalPanel(
        condition = "input.wizard_comparison == 'compare'",
        shiny::radioButtons(
          inputId = "wizard_design",
          label   = "How is the comparison made?",
          choices = c(
            "Researcher assigns the intervention (randomised controlled trial)" = "trial",
            "Following exposed and unexposed groups forward in time (cohort)"   = "cohort",
            "Starting from the outcome and looking back (case-control)"          = "casecontrol",
            "Comparing a diagnostic test against a gold standard"                = "diagnostic"
          ),
          selected = character(0)
        )
      ),

      # Only shown once "trial" is selected within the comparison branch.
      shiny::conditionalPanel(
        condition = "input.wizard_comparison == 'compare' && input.wizard_design == 'trial'",
        shiny::radioButtons(
          inputId = "wizard_trial_type",
          label   = "What is the goal of the trial?",
          choices = c(
            "Show the new treatment is better (superiority)"          = "superiority",
            "Show the new treatment is not unacceptably worse (non-inferiority)" = "noninferiority",
            "Show the two treatments are practically equivalent (equivalence)"   = "equivalence"
          ),
          selected = character(0)
        )
      )
    ),

    # ---- O: Outcome type -------------------------------------------------
    # Not relevant for the diagnostic-test design, so it's hidden in
    # that case (handled by the condition below).
    bslib::card(
      bslib::card_header("O-Outcome"),
      shiny::conditionalPanel(
        condition = "input.wizard_design != 'diagnostic'",
        shiny::radioButtons(
          inputId = "wizard_outcome_type",
          label   = "What type of outcome variable will be used?",
          choices = c(
            "Binary (e.g. proportion, event yes/no)" = "binary",
            "Continuous (e.g. mean, measurement)"     = "continuous"
          ),
          selected = character(0)
        )
      )
    ),

    # ---- Confirmation ------------------------------------------------
    bslib::card(
      shiny::actionButton(
        inputId = "wizard_confirm",
        label   = "Go to Calculator",
        class   = "btn-primary"
      ),
      shiny::uiOutput("wizard_summary")   # server.R fills this in with a plain-language summary
    )
  )
}
