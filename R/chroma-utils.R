

#' Mixes two colors
#'
#' @param color1 Hexadecimal string or a name for color one.
#' @param color2 Hexadecimal string or a name for color two.
#' @param ratio A value between 0 and 1.
#' @param mode Color space used for interpolation.
#'
#' @return A hexadecimal string
#' @export
#'
#' @examples
#' chroma_mix("red", "blue")
#'
#' chroma_mix("red", "blue", ratio = 0.75)
#'
#' chroma_mix("red", "blue", mode = "lab")
chroma_mix <- function(color1, color2, ratio = 0.5, mode = c("rgb", "hsl", "lab", "lrgb", "lch")) {
  ch <- chroma$new(color1)
  ch$mix(color2 = color2, ratio = ratio, mode = mode)
  ch$eval()
}


#' Simple averaging of colors
#'
#' @param colors A vector of hexadecimal strings, minimum length 2.
#' @param mode Color space used for interpolation.
#'
#' @return A hexadecimal string
#' @export
#'
#' @examples
#' chroma_avg(colors = c("#ddd", "yellow", "red", "teal"))
chroma_avg <- function(colors, mode = c("rgb", "hsl", "lab", "lrgb", "lch")) {
  if (length(colors) <= 2)
    stop("'colors' must bve minimum length 2.", call. = FALSE)
  ch <- chroma$new()
  ch$average(colors = colors, mode = mode)
  ch$eval()
}


#' Random colors
#'
#' Creates a random color by generating a random hexadecimal string.
#'
#' @param n Number of colors desired.
#'
#' @return A vector of hexadecimal string(s).
#' @export
#'
#' @examples
#' chroma_random()
#' chroma_random(10)
chroma_random <- function(n = 1L) {
  ch <- chroma$new()
  replicate(n = n, expr = {ch$random();ch$eval()})
}


#' Distance between two colors
#'
#' Computes the Euclidean distance between two colors in a given color space (default is Lab).
#'
#' @param color1 Hexadecimal string or a name for color one.
#' @param color2 Hexadecimal string or a name for color two.
#' @param mode Color space used for interpolation.
#'
#' @return a numeric
#' @export
#'
#' @examples
#' chroma_distance("#fff", "#ff0")
#' chroma_distance("#fff", "#ff0", "rgb")
chroma_distance <- function(color1, color2, mode = c("lab", "rgb", "hsl", "lrgb", "lch")) {
  ch <- chroma$new()
  ch$distance(color1 = color1, color2 = color2, mode = mode)
  ch$eval()
}



#' Returns a single channel value.
#'
#' @param color Hexadecimal string or a name for color.
#' @param channel Channel desired, for example \code{"hsl.h"} for hue parameter in hsl color space.
#'
#' @return a single numeric value
#' @export
#'
#' @examples
#' chroma_get("#1D9A6C", "hsl.h")
#' chroma_get("hotpink", "hsl.h")
#'
#' # vectorized over color
#' chroma_get(c("hotpink", "steelblue"), "rgb.g")
chroma_get <- function(color, channel) {
  if (length(color) == 1) {
    value <- chroma$new(color)
    value$get(channel = channel)
    value$eval()
  } else {
    res <- lapply(
      X = color,
      FUN = chroma_get,
      channel = channel
    )
    unlist(res)
  }
}


#' Contrast between two colors
#'
#' Computes the WCAG contrast ratio between two colors.
#'  A minimum contrast of 4.5:1 is recommended to ensure
#'  that text is still readable against a background color.
#'
#' @param color1 Hexadecimal string or a name for color.
#' @param color2 Hexadecimal string or a name for color.
#'
#' @return a numeric value
#' @export
#'
#' @examples
#' chroma_contrast("pink", "hotpink")
#'
#'
#' # black on light grey => good
#' chroma_contrast("#e3e3e3", "black")
#'
#' # black on dark grey => bad
#' chroma_contrast("#393939", "black")
chroma_contrast <- function(color1, color2) {
  stopifnot(length(color1) == length(color2))
  if (length(color1) == 1) {
    value <- chroma$new()
    value$contrast(color1 = color1, color2 = color2)
    value$eval()
  } else {
    res <- lapply(
      X = seq_along(color1),
      FUN = function(i) {
        chroma_contrast(color1 = color1[i], color2 = color2[i])
      }
    )
    unlist(res)
  }
}




