

#' Mix two colors
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

