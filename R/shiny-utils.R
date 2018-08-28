
#' Create rectangles in HTML to display colors
#'
#' @param colors A vector of colors to visualize
#' @param width The width of the HTML tag, e.g. '400px', or '100\%'.
#' @param height The height of the HTML tag, e.g. '400px', or '100\%'.
#'
#' @return An HTML tag
#' @export
#'
#' @importFrom htmltools tags validateCssUnit
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#'
#' if (interactive()) {
#'
#' library(shiny)
#' library(colorscale)
#'
#' ui <- fluidPage(
#'   tags$h2("Visualizing colors in Shiny"),
#'   colors_rect(colors = c(
#'     get_dark_cols("#1D9A6C"),
#'     "#1D9A6C",
#'     get_light_cols("#1D9A6C")
#'   ))
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#'
#' }
colors_rect <- function(colors, width = "100%", height = "45px") {
  # <div style='width:100%%;padding:5px;border-radius:4px;background:%s;color:%s'>%s</div>
  width <- validateCssUnit(width)
  height <- validateCssUnit(height)
  tags$div(
    style = "padding:5px; border-radius: 4px;",
    style = glue::glue("width: {width};"),
    style = glue::glue("height: {height};"),
    style = glue::glue("background:{colors};", colors = linear_gradient(colors))
  )
}

linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ")")
}
