#' UI for the Validation tab
#'
#' A static comparison table showing this package's output against
#' the worked numeric examples in Bhardwaj et al. (2024), along with
#' a short explanation of every case where the two differ and why.
#'
#' @return A shiny tag list, ready to be placed inside a nav_panel().
#' @keywords internal
ui_validation <- function() {
  shiny::tagList(
    shiny::p(
      "The table below reproduces every worked numeric example in ",
      shiny::em("Bhardwaj et al. (2024), J Family Med Prim Care"),
      ", using this package, and compares the two results."
    ),

    shiny::tableOutput("validation_table"),

    shiny::h4("Why some numbers differ"),
    shiny::tags$ol(
      shiny::tags$li(
        shiny::strong("Dropout formula: "),
        "the paper's worked examples use the approximation n \u00d7 (1 + dropout), ",
        "while this package uses the statistically correct n / (1 - dropout), ",
        "as stated (but not actually used) in the paper's own text."
      ),
      shiny::tags$li(
        shiny::strong("Rounding: "),
        "the paper truncates some decimals (e.g. 138.29 \u2192 138) instead of ",
        "rounding up. This package always rounds up, the conventional and more ",
        "conservative rule for sample size."
      ),
      shiny::tags$li(
        shiny::strong("Approximation method: "),
        "for case-control, cohort, and trial designs, the paper uses a simple ",
        "normal approximation formula, while this package uses epiR's more ",
        "precise implementations (Dupont 1988; Woodward 2014). Differences of ",
        "a few subjects are expected and are not errors."
      ),
      shiny::tags$li(
        shiny::strong("Trial sidedness: "),
        "the paper treats superiority trials as one-sided; current regulatory ",
        "guidance (and epiR's documentation) favours two-sided testing. This ",
        "package defaults to two-sided but lets the user choose either."
      ),
      shiny::tags$li(
        shiny::strong("Diagnostic test sample size: "),
        "the paper adds the sensitivity and specificity sample sizes together. ",
        "This package takes the larger of the two, following Buderer (1996) and ",
        "Hajian-Tilaki (2014): both are estimated from the same study subjects, ",
        "so summing over-counts the required enrolment."
      )
    )
  )
}


#' The static validation dataset used by the table above
#'
#' Kept as its own small function (rather than inline in server.R) so
#' it can also be reused by tests, if needed later.
#'
#' @return A data.frame.
#' Static validation comparsion data
#' @keywords internal
validation_dataset <- function() {
  data.frame(
    Design = c(
      "Cross-sectional (binary)", "Cross-sectional (continuous)",
      "Case-control", "Cohort",
      "Trial (superiority, one-sided)", "Trial (non-inferiority)",
      "Trial (equivalence)", "Diagnostic accuracy"
    ),
    `Paper's raw n`   = c(323, 138, 153, 293, 852, 307, 388, 1401),
    `Package raw n`   = c(323, 139, 152, 294, 834, 307, 424, 1230),
    `Paper's final n` = c(355, 152, 336, 644, 1704, 614, 776, "n/a"),
    `Package final n` = c(359, 155, 338, 654, "n/a", "n/a", "n/a", "n/a"),
    check.names = FALSE
  )
}
