#' Adds the content of www to colorscale/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("colorscale", system.file('www', package = "colorscale"))
}
