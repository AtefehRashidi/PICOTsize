#' Launch the PICOTsize Shiny app
#'
#' @return No return value. Launches the Shiny app in a browser or
#'   the RStudio Viewer pane.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @export
run_app <- function() {
  shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}
