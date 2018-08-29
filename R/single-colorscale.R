
#' Get n darkest colors from a single one
#'
#' @param color Hexadecimal string or a name for color.
#' @param n Number of desired colors.
#' @param darkness Percentage of darkness (\code{[0, 1]}).
#' @param rotate Rotate coefficient in HSL space (\code{[-360, 360]}).
#' @param saturation Saturation percentage in HSL space (\code{[-1, 1]}).
#'
#' @return a vector of \code{n} colors.
#' @export
#'
#' @examples
#' get_dark_cols("#1D9A6C")
#'
#' # display in viewer
#' view_cols(get_dark_cols(color = "#1D9A6C"))
get_dark_cols <- function(color, n = 4, darkness = 0.5, rotate = -51, saturation = 0.14) {

  stopifnot(length(color) == 1)

  ch <- chroma$new(color)

  hsl.h <- chroma_get(color, "hsl.h")
  hsl.s <- chroma_get(color, "hsl.s")

  ch$set(channel = "hsl.h", value = rev(seq_len(n)) / n * -rotate + hsl.h)
  ch$set(channel = "hsl.s", value = (rev(seq_len(n)) / n * saturation) * hsl.s + hsl.s)
  ch$mix(color2 = "black", ratio = darkness * rev(seq_len(n)) / n)

  ch$eval()
}


#' Get n lightest colors from a single one
#'
#' @param color Hexadecimal string or a name for color.
#' @param n Number of desired colors.
#' @param lightness Percentage of lightness (\code{[0, 1]}).
#' @param rotate Rotate coefficient in HSL space (\code{[-360, 360]}).
#' @param saturation Saturation percentage in HSL space (\code{[-1, 1]}).
#'
#' @return a vector of \code{n} colors.
#' @export
#'
#' @examples
#' get_light_cols("#1D9A6C")
#'
#' # display in viewer
#' view_cols(get_light_cols(color = "#1D9A6C"))
get_light_cols <- function(color, n = 6, lightness = 0.8, rotate = 67, saturation = 0.20) {

  stopifnot(length(color) == 1)

  ch <- chroma$new(color)

  hsl.h <- chroma_get(color, "hsl.h")
  hsl.s <- chroma_get(color, "hsl.s")

  ch$set(channel = "hsl.h", value = seq_len(n) / n * -rotate + hsl.h)
  ch$set(channel = "hsl.s", value = (seq_len(n) / n * saturation) * hsl.s + hsl.s)
  ch$mix(color2 = "white", ratio = lightness * seq_len(n) / n)

  ch$eval()
}






#' Create a color scale from a single color
#'
#' @param color Hexadecimal string or a name for color.
#' @param n_light Number of light colors to generate.
#' @param n_dark Number of dark colors to generate.
#' @param lightness Percentage of lightness (\code{[0, 1]}).
#' @param darkness Percentage of darkness (\code{[0, 1]}).
#' @param rotate_light Rotate coefficient in HSL space for light colors (\code{[-360, 360]}).
#' @param rotate_dark Rotate coefficient in HSL space for dark colors (\code{[-360, 360]}).
#' @param saturation_light Saturation percentage in HSL space for light colors (\code{[-1, 1]}).
#' @param saturation_dark Saturation percentage in HSL space for dark colors (\code{[-1, 1]}).
#'
#' @return a vector of \code{n_light + n_dark + 1} colors.
#' @export
#'
#' @examples
#' single_scale(color = "#1D9A6C")
#'
#' # display in viewer
#' view_cols(single_scale(color = "#1D9A6C"))
single_scale <- function(color,
                         n_light = 6, n_dark = 4,
                         lightness = 0.8, darkness = 0.5,
                         rotate_light = 67, rotate_dark = -51,
                         saturation_light = 0.20, saturation_dark = 0.14) {
  if (length(color) != 1) {
    stop("'color' must be of length one !", call. = FALSE)
  }
  c(
    get_dark_cols(
      color = color,
      n = n_dark,
      darkness = darkness,
      rotate = rotate_dark,
      saturation = saturation_dark
    ),
    color,
    get_light_cols(
      color = color,
      n = n_light,
      lightness = lightness,
      rotate = rotate_light,
      saturation = saturation_light
    )
  )
}


