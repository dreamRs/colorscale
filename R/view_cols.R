
#' Display colors in the viewer
#'
#' @param colors A vector of colors
#' @param width Width of each box.
#' @param height Height of each box
#'
#' @export
#'
#' @name view-colors
#'
#' @importFrom htmltools validateCssUnit tags browsable
#' @importFrom glue glue
#'
#' @examples
#'
#' ### View colors
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
view_cols <- function(colors, width = 80, height = 80) {
  width <- validateCssUnit(width)
  height <- validateCssUnit(height)
  col_tags <- lapply(
    X = seq_along(colors),
    FUN = function(i) {
      col <- colors[i]
      tags$div(
        style = "display: inline-block; text-align: center; margin-bottom: 5px;",
        style = glue::glue(
          "height: {height}; line-height: {height}; background-color : {color}; width: {width}; color: {textColor}",
          color = col, width = width, height = height,
          textColor = ifelse(chroma_contrast(col, "black") >= 4.5, "black", "white")
        ),
        if (!is.null(names(colors))) {
          names(colors)[i]
        } else {
          col
        }
      )
    }
  )
  browsable(tags$div(col_tags))
}

#' @rdname view-colors
#' @export
#' @param pal A function for generating a palette.
#' @importFrom htmltools tagList browsable
#'
#' @examples
#'
#' ### View palettes
#'
#' # View one palette
#' view_pal(pal = chroma_continuous_pal())
#' view_pal(pal = chroma_continuous_pal(mode = "lch"))
#'
#' # View several palettes
#' view_pal(pal = list(
#'   "default (mode lab)" = chroma_continuous_pal(),
#'   "mode lch" = chroma_continuous_pal(mode = "lch"),
#'   "mode rgb" = chroma_continuous_pal(mode = "rgb")
#' ))
#'
#' # View scales palettes
#' library(scales)
#' view_pal(pal = list(
#'   "seq_gradient_pal" = seq_gradient_pal(),
#'   "gradient_n_pal" = gradient_n_pal(c("yellow", "navy"))
#' ))
#'
view_pal <- function(pal, height = 80) {
  height <- validateCssUnit(height)
  x <- seq(0, 1, length.out = 100)
  if (is.function(pal)) {
    colors <- pal(x)
    gradient <- linear_gradient(cols = colors)
    col_tags <- tags$div(
      style = glue::glue("width: 100%; height: {height};"),
      style = glue::glue("background: {gradient};")
    )
  } else if (is.list(pal)) {
    gradient <- lapply(
      X = pal,
      FUN = function(fun) {
        colors <- fun(x)
        linear_gradient(cols = colors)
      }
    )
    gradient <- unlist(gradient)
    col_tags <- lapply(
      X = seq_along(gradient),
      FUN = function(i) {
        tagList(
          tags$p(names(pal)[i]),
          tags$div(
            style = glue::glue("width: 100%; height: {height};"),
            style = glue::glue("background: {gradient};", gradient = gradient[i])
          )
        )
      }
    )
  } else {
    stop("'pal' must be a function or a list of function.", call. = FALSE)
  }
  browsable(tags$div(col_tags))
}

#' @rdname view-colors
#' @export
#'
#' @examples
#'
#' ### View a gradient
#'
#' x <- seq(0, 1, length.out = 100)
#' view_gradient(chroma_continuous_pal()(x))
#'
#' # with a list
#' view_gradient(list(
#'   "chroma" = chroma_continuous_pal()(x),
#'   "bezier" = bezier_continuous_pal()(x)
#' ))
view_gradient <- function(colors, height = 80) {
  height <- validateCssUnit(height)
  if (is.atomic(colors)) {
    gradient <- linear_gradient(cols = colors)
    col_tags <- tags$div(
      style = glue::glue("width: 100%; height: {height};"),
      style = glue::glue("background: {gradient};")
    )
  } else if (is.list(colors)) {
    gradient <- lapply(
      X = colors,
      FUN = linear_gradient
    )
    gradient <- unlist(gradient)
    col_tags <- lapply(
      X = seq_along(gradient),
      FUN = function(i) {
        tagList(
          tags$p(names(colors)[i]),
          tags$div(
            style = glue::glue("width: 100%; height: {height};"),
            style = glue::glue("background: {gradient};", gradient = gradient[i])
          )
        )
      }
    )
  } else {
    stop("'pal' must be a function or a list of function.", call. = FALSE)
  }
  browsable(tags$div(col_tags))
}










