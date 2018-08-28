
#' Get n darkest colors from a single one
#'
#' @param color Hexadecimal string or a name for color.
#' @param n Number of desired colors.
#' @param darkness Percentage of darkness.
#' @param rotate Rotate coefficient in HSL space.
#' @param saturation Saturation percentage in HSL space
#'
#' @return a vector of \code{n} colors
#' @export
#'
#' @examples
#' get_dark_cols("#1D9A6C")
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
#' @param lightness Percentage of lightness.
#' @param rotate Rotate coefficient in HSL space.
#' @param saturation Saturation percentage in HSL space
#'
#' @return a vector of \code{n} colors
#' @export
#'
#' @examples
#' get_light_cols("#1D9A6C")
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




