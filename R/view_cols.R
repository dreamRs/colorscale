
#' Display colors in the viewer
#'
#' @param colors A vector of colors
#' @param width Width of each box.
#' @param height Height of each box
#'
#' @export
#'
#' @importFrom utils browseURL
#' @importFrom htmltools validateCssUnit tags doRenderTags
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#'
#' # Scale of greys
#' view_cols(c("#ffffff", "#e3e3e3", "#c6c6c6", "#aaaaaa",
#'             "#8e8e8e", "#717171", "#555555", "#393939",
#'             "#1c1c1c", "#000000"))
#'
#'
#' # 50 first colors from colors() (converted to HEX first)
#' col2hex <- function(col, alpha) # function from @davidorme
#'   rgb(t(col2rgb(col)), alpha=alpha, maxColorValue=255)
#'
#'
#' view_cols(col2hex(colors()[1:50]))
#'
#' }
view_cols <- function(colors, width = 80, height = 80) {
  width <- validateCssUnit(width)
  height <- validateCssUnit(height)
  col_tags <- lapply(
    X = colors,
    FUN = function(x) {
      tags$div(
        style = "display: inline-block; text-align: center; margin-bottom: 5px;",
        style = glue::glue(
          "height: {height}; line-height: {height}; background-color : {color}; width: {width}; color: {textColor}",
          color = x, width = width, height = height,
          textColor = ifelse(chroma_contrast(x, "black") >= 4.5, "black", "white")
        ),
        x
      )
    }
  )
  dir <- tempfile()
  dir.create(dir)
  htmlFile <- file.path(dir, "index.html")
  writeLines(text = doRenderTags(col_tags), con = htmlFile)
  viewer <- getOption("viewer")
  if (!is.null(viewer))
    viewer(url = htmlFile)
  else
    utils::browseURL(url = htmlFile)
}

