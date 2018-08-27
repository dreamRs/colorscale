

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
#' @return
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





