
#' Continuous color palettes
#'
#' @param colors Vector of hexadecimal strings or names for colors.
#' @param mode Color space used for interpolation.
#' @param padding Reduces the color range by cutting of a fraction of the gradient on both sides.
#'  If you pass a single number, the same padding will be applied to both ends.
#' @param correct_lightness Makes sure the lightness range is spread evenly across a color scale.
#'
#' @return a function
#' @export
#'
#' @name continuous-palettes
#'
#' @examples
#' x <- seq(0, 1, length.out = 25)
#' view_cols(chroma_continuous_pal()(x))
#'
#' # change mode
#' view_cols(chroma_continuous_pal(mode = "lch")(x))
#'
#'
#' # Bezier interpolation
#' view_cols(bezier_continuous_pal()(x))
#'
#' # Color Brewer :
#' view_cols(chroma_continuous_pal("RdYlBu")(x))
#' # add some padding :
#' view_cols(chroma_continuous_pal("RdYlBu", padding = 0.15)(x))
#'
chroma_continuous_pal <- function(colors = c("yellow", "navy"), mode = "lab", padding = NULL, correct_lightness = TRUE) {
  function(x) {
    cs <- chroma_scale$new()
    cs$scale(colors = colors)
    if (length(colors) > 1) {
      cs$mode(mode = mode)
      if (correct_lightness) {
        cs$correctLightness()
      }
    }
    if (!is.null(padding)) {
      cs$padding(padding)
    }
    cs$colors(n = x)
    cs$eval()
  }
}

#' @rdname continuous-palettes
#' @export
bezier_continuous_pal <- function(colors = c("yellow", "red", "black")) {
  function(x) {
    cs <- chroma_scale$new()
    cs$bezier(colors = colors)
    cs$colors(n = x)
    cs$eval()
  }
}


