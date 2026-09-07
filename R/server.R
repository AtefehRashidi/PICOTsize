#' Main Shiny server function for PICOTsize
#'
#' @param input,output,session Standard Shiny server arguments.
#' @keywords internal
#' @importFrom rlang .data
server <- function(input, output, session) {

  # ---- Step 1: work out which design the Wizard answers point to -------
  # This is the single source of truth for "which calculator form and
  # which calc_* function applies" -- everything else in this server
  # reads from current_design() rather than re-checking the wizard
  # inputs directly.
  current_design <- shiny::reactive({
    shiny::req(input$wizard_comparison)

    if (input$wizard_comparison == "single") {
      shiny::req(input$wizard_outcome_type)
      if (input$wizard_outcome_type == "binary") {
        "cross_binary"
      } else {
        "cross_continuous"
      }
    } else {
      shiny::req(input$wizard_design)

      if (input$wizard_design == "casecontrol") {
        "casecontrol"
      } else if (input$wizard_design == "cohort") {
        "cohort"
      } else if (input$wizard_design == "diagnostic") {
        "diagnostic"
      } else if (input$wizard_design == "trial") {
        shiny::req(input$wizard_trial_type)
        switch(
          input$wizard_trial_type,
          "superiority"    = "trial_superiority",
          "noninferiority" = "trial_noninferiority",
          "equivalence"    = "trial_equivalence"
        )
      } else {
        NULL
      }
    }
  })

  # A human-readable label for each design key, used both in the
  # Wizard summary and later as the section header on the Calculator
  # tab -- kept in one place so the wording only needs to change once.
  design_label <- function(design_key) {
    switch(
      design_key,
      "cross_binary"          = "Cross-sectional study (binary outcome)",
      "cross_continuous"      = "Cross-sectional study (continuous outcome)",
      "casecontrol"           = "Case-control study",
      "cohort"                = "Cohort study",
      "trial_superiority"     = "Clinical trial (superiority)",
      "trial_noninferiority"  = "Clinical trial (non-inferiority)",
      "trial_equivalence"     = "Clinical trial (equivalence)",
      "diagnostic"            = "Diagnostic test accuracy study",
      "Please complete the questions above."
    )
  }

  # ---- Step 2: show a plain-language summary at the bottom of the Wizard --
  output$wizard_summary <- shiny::renderUI({
    design_key <- tryCatch(current_design(), error = function(e) NULL)

    if (is.null(design_key)) {
      shiny::tags$p(shiny::em("Answer the questions above to see your study design."))
    } else {
      shiny::tags$p(
        shiny::strong("Design identified: "), design_label(design_key)
      )
    }
  })

  # ---- Step 3: "Go to Calculator" button switches tabs -------------------
  shiny::observeEvent(input$wizard_confirm, {
    shiny::req(current_design())   # don't navigate if the wizard isn't complete
    bslib::nav_select(id = "main_nav", selected = "Calculator", session = session)
  })

  # ---- Step 4: render the dynamic parameter form -------------------------
  output$calculator_inputs <- shiny::renderUI({
    design_key <- current_design()
    shiny::req(design_key)
    build_calculator_form(design_key)
  })

  # ---- Step 5: keep the plot's parameter dropdown in sync with the design --
  shiny::observeEvent(current_design(), {
    choices <- plot_parameter_choices(current_design())
    shiny::updateSelectInput(session, "plot_parameter", choices = choices)
  }, ignoreNULL = TRUE)

  # ---- Step 6: a single place that maps a design to its calc_* function ----
  # Kept as a named list (not a switch) so it can be looked up by key
  # in more than one place below without repeating the mapping.
  design_functions <- list(
    cross_binary         = calc_crosssectional_binary,
    cross_continuous     = calc_crosssectional_continuous,
    casecontrol          = calc_casecontrol,
    cohort               = calc_cohort,
    trial_superiority    = calc_trial_superiority,
    trial_noninferiority = calc_trial_noninferiority,
    trial_equivalence    = calc_trial_equivalence,
    diagnostic           = calc_diagnostic_accuracy
  )

  # Reads the calculator form's current values into one plain list,
  # using short generic names. Kept separate from the design-specific
  # argument names so the same raw values can be reused, with one
  # field overridden, when building the sensitivity plot.
  read_raw_inputs <- function() {
    list(
      p1         = input$calc_p1,
      p2         = input$calc_p2,
      precision  = input$calc_precision,
      mean       = input$calc_mean,
      sd         = input$calc_sd,
      delta      = input$calc_delta,
      power      = input$calc_power,
      ratio      = input$calc_ratio,
      prevalence = input$calc_prevalence,
      dropout    = input$calc_dropout,
      conflevel  = input$calc_conflevel,
      siglevel   = input$calc_siglevel,
      errortype  = input$calc_errortype,
      sided      = input$calc_sided
    )
  }

  # Translates the generic raw values above into the exact argument
  # names each calc_* function expects. This is the one place that
  # needs updating if a calc_* function's arguments ever change.
  build_calc_args <- function(design_key, raw) {
    dropout_rate <- raw$dropout / 100

    switch(
      design_key,
      "cross_binary" = list(
        p = raw$p1, precision = raw$precision, error_type = raw$errortype,
        conf_level = as.numeric(raw$conflevel), dropout_rate = dropout_rate
      ),
      "cross_continuous" = list(
        mean = raw$mean, sd = raw$sd, precision = raw$precision,
        error_type = raw$errortype, conf_level = as.numeric(raw$conflevel),
        dropout_rate = dropout_rate
      ),
      "casecontrol" = list(
        p_exposed_controls = raw$p1, p_exposed_cases = raw$p2,
        control_case_ratio = raw$ratio, power = raw$power / 100,
        sig_level = as.numeric(raw$siglevel), dropout_rate = dropout_rate
      ),
      "cohort" = list(
        incidence_unexposed = raw$p1, incidence_exposed = raw$p2,
        exposed_unexposed_ratio = raw$ratio, power = raw$power / 100,
        sig_level = as.numeric(raw$siglevel), dropout_rate = dropout_rate
      ),
      "trial_superiority" = list(
        p_standard = raw$p1, p_new = raw$p2, delta = raw$delta,
        power = raw$power / 100, sig_level = as.numeric(raw$siglevel),
        sided_test = as.integer(raw$sided), dropout_rate = dropout_rate
      ),
      "trial_noninferiority" = list(
        p_standard = raw$p1, p_new = raw$p2, delta = raw$delta,
        power = raw$power / 100, sig_level = as.numeric(raw$siglevel),
        dropout_rate = dropout_rate
      ),
      "trial_equivalence" = list(
        p_standard = raw$p1, p_new = raw$p2, delta = raw$delta,
        power = raw$power / 100, sig_level = as.numeric(raw$siglevel),
        dropout_rate = dropout_rate
      ),
      "diagnostic" = list(
        expected_sensitivity = raw$p1, expected_specificity = raw$p2,
        prevalence = raw$prevalence, precision = raw$precision,
        error_type = raw$errortype, conf_level = as.numeric(raw$conflevel),
        dropout_rate = dropout_rate
      ),
      NULL
    )
  }

  # ---- Step 7: run the actual calculation ---------------------------------
  calc_result <- shiny::reactive({
    design_key <- current_design()
    shiny::req(design_key)

    raw <- read_raw_inputs()
    shiny::req(raw$p1)   # a proxy check that the dynamic form has rendered

    args <- build_calc_args(design_key, raw)
    fn   <- design_functions[[design_key]]

    tryCatch(do.call(fn, args), error = function(e) NULL)
  })

  # ---- Step 8: show the result --------------------------------------------
  output$calculator_result <- shiny::renderUI({
    result <- calc_result()
    shiny::req(result)

    shiny::tagList(
      shiny::h2(style = "color:#1B3A6B;",
                paste(result$n_final, "participants")),
      shiny::p(style = "color:#666;",
               paste0("(raw sample size before dropout adjustment: ", result$n_raw, ")"))
    )
  })

  # ---- Step 9: the Methods-section report text ----------------------------
  output$report_text <- shiny::renderText({
    result <- calc_result()
    shiny::req(result)
    generate_report_text(result)
  })

  # ---- Step 10: the interactive sensitivity plot --------------------------
  # Builds a short sequence of plausible values for whichever field is
  # selected, recalculates n_final for each one (holding everything
  # else fixed at the form's current values), and highlights the
  # form's current value on the resulting curve.
  param_sequence <- function(field, current_value) {
    switch(
      field,
      "p1"         = ,
      "p2"         = ,
      "prevalence" = seq(0.05, 0.95, by = 0.05),
      "precision"  = seq(0.01, 0.20, by = 0.01),
      "power"      = seq(70, 99, by = 2),
      "delta"      = seq(0.02, 0.30, by = 0.02),
      "ratio"      = 1:5,
      "sd"         = seq(max(0.1, current_value * 0.5), current_value * 1.5, length.out = 15),
      seq(current_value * 0.5, current_value * 1.5, length.out = 10)
    )
  }

  output$sensitivity_plot <- plotly::renderPlotly({
    design_key <- current_design()
    shiny::req(design_key, input$plot_parameter)

    raw           <- read_raw_inputs()
    field         <- input$plot_parameter
    current_value <- raw[[field]]
    shiny::req(current_value)

    fn     <- design_functions[[design_key]]
    values <- param_sequence(field, current_value)

    n_values <- vapply(values, function(v) {
      raw_mod       <- raw
      raw_mod[[field]] <- v
      args          <- build_calc_args(design_key, raw_mod)
      res           <- tryCatch(do.call(fn, args), error = function(e) NULL)
      if (is.null(res)) NA_real_ else res$n_final
    }, numeric(1))

    plot_df    <- data.frame(x = values, y = n_values)
    current_df <- data.frame(x = current_value, y = calc_result()$n_final)

    axis_label <- names(plot_parameter_choices(design_key))[
      plot_parameter_choices(design_key) == field
    ]

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_line(color = "#1B3A6B", linewidth = 1) +
      ggplot2::geom_point(data = current_df, ggplot2::aes(x = .data$x, y = .data$y),
                          color = "#FFFF00", size = 4) +
      ggplot2::labs(x = axis_label, y = "Required sample size") +
      ggplot2::theme_classic()

    plotly::ggplotly(p, height = 350) |>
      plotly::layout(
        autosize = TRUE,
        margin   = list(l = 70, r = 20, t = 20, b = 60)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })
  # ---- Step 11: the Validation tab's comparison table ---------------------
  output$validation_table <- shiny::renderTable({
    validation_dataset()
  })
}
