#' UI for the Calculator tab
#'
#' This tab's parameter form changes depending on which study design
#' was chosen in the Wizard tab, so most of it is built dynamically
#' on the server side (see server.R and ui_calculator_forms.R). This
#' function only lays out the static skeleton: a slot for the
#' dynamic input form, a results card, an interactive plot, and the
#' auto-generated report text.
#'
#' @return A shiny tag list, ready to be placed inside a nav_panel().
#' @keywords internal
ui_calculator <- function() {
  bslib::layout_column_wrap(
    width = "420px",

    # ---- Dynamic input form ---------------------------------------------
    bslib::card(
      bslib::card_header("Parameters"),
      shiny::uiOutput("calculator_inputs")
    ),

    # ---- Results ---------------------------------------------------------
    bslib::card(
      bslib::card_header("Result"),
      shiny::uiOutput("calculator_result")
    ),

    # ---- Interactive sensitivity plot -------------------------------------
    bslib::card(
      bslib::card_header("How sample size changes with your parameters"),
      shiny::selectInput(
        inputId = "plot_parameter",
        label   = "Vary this parameter:",
        choices = NULL
      ),
      plotly::plotlyOutput("sensitivity_plot", height = "350px")
    ),

    # ---- Auto-generated Methods paragraph -----------------------------
    bslib::card(
      bslib::card_header("Methods-section text (copy into your manuscript)"),
      shiny::verbatimTextOutput("report_text"),
      shiny::tags$button(
        "Copy text",
        class   = "btn btn-outline-primary btn-sm mt-2",
        onclick = "navigator.clipboard.writeText(document.getElementById('report_text').innerText)"
      )
    )
  )
}
