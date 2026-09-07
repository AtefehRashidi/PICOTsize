# ---- Theme -----------------------------------------------------------
#' The PICOTsize bslib theme
#' @keywords internal
picotsize_theme <- bslib::bs_theme(
  version      = 5,
  bg           = "#FFFFFF",
  fg           = "#1a1a1a",
  primary      = "#1B3A6B",   # deep Isfahan lapis blue
  secondary    = "#178C8C",   # firoozeh turquoise
  base_font    = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter", wght = 600)
)

# ---- Decorative tile banners -------------------------------------------
# Real photographs of Isfahan tilework, cropped to a wide, thin strip.
# addResourcePath() tells Shiny where to find the "www" folder inside
# the installed package, so the images can be referenced by a simple
# relative path ("picotsize-www/...") from anywhere in the UI.
shiny::addResourcePath(
  "picotsize-www",
  system.file("app/www", package = "PICOTsize")
)
#' The title header
#' @keywords internal
tile_header <- function() {
  shiny::tags$img(
    src   = "picotsize-www/isfahan-tile-header.jpg",
    style = "width: 100%; height: 42px; object-fit: cover; display: block;"
  )
}
#' The title footer
#' @keywords internal
tile_footer <- function() {
  shiny::tags$img(
    src   = "picotsize-www/isfahan-tile-footer.jpg",
    style = "width: 100%; height: 28px; object-fit: cover; display: block;"
  )
}

# ---- Main UI -----------------------------------------------------------
#' The ui
#' @keywords internal
ui <- function() {
  bslib::page_navbar(
    id = "main_nav",
    title  = "PICOTsize",
    theme  = picotsize_theme,
    header = tile_header(),
    #footer = tile_footer(),

    bslib::nav_panel(
      title = "Wizard",
      ui_wizard()
    ),

    bslib::nav_panel(
      title = "Calculator",
      ui_calculator()
    ),

    bslib::nav_panel(
      title = "Validation",
      shiny::h3("Validation Against Bhardwaj et al. (2024)"),
      ui_validation()
    )
  )
}
