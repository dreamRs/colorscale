#' Chroma API
#'
#' @export
#'
#' @importFrom V8 v8
#' @importFrom R6 R6Class
#' @importFrom glue glue single_quote
#' @importFrom stringi stri_c
#'
chroma <- R6::R6Class(
  classname = "chroma",
  public = list(
    initialize = function(color = NULL) {
      if (is.null(color)) {
        private$chroma$chroma <- "chroma"
      } else {
        private$initialized <- TRUE
        if (length(color) == 1) {
          private$chroma$chroma <- glue::glue("chroma('{color}')")
        } else if (length(color) == 3) {
          private$chroma$chroma <- glue::glue("chroma({color})", color = paste(color, collapse = ","))
        }
      }
    },


    chroma.hsl = function(hue, saturation, lightness) {
      private$initialized <- TRUE
      private$chroma$chroma <- glue::glue("chroma.hsl({hue}, {saturation}, {lightness})")
    },
    chroma.hsv = function(hue, saturation, value) {
      private$initialized <- TRUE
      private$chroma$chroma <- glue::glue("chroma.hsv({hue}, {saturation}, {value})")
    },
    chroma.lch = function(Lightness, chroma, hue) {
      private$initialized <- TRUE
      private$chroma$chroma <- glue::glue("chroma.lch({Lightness}, {chroma}, {hue})")
    },
    chroma.gl = function(red, green, blue, alpha = NULL) {
      private$initialized <- TRUE
      if (is.null(alpha)) {
        private$chroma$chroma <- glue::glue("chroma.gl({red}, {green}, {blue})")
      } else {
        private$chroma$chroma <- glue::glue("chroma.gl({red}, {green}, {blue}, {alpha})")
      }
    },

    # API


    # color
    alpha = function(value = NA) {
      if_initialized(private$initialized)
      private$chroma$alpha <- glue::glue("alpha({value})", .na = "")
    },
    brighten = function(value = 1) {
      if_initialized(private$initialized)
      private$chroma$brighten <- glue::glue("brighten({value})")
    },
    darken = function(value = 1) {
      if_initialized(private$initialized)
      private$chroma$darken <- glue::glue("darken({value})")
    },
    saturate = function(value = 1) {
      if_initialized(private$initialized)
      private$chroma$saturate <- glue::glue("saturate({value})")
    },
    desaturate = function(value = 1) {
      if_initialized(private$initialized)
      private$chroma$desaturate <- glue::glue("desaturate({value})")
    },
    set = function(channel, value) {
      if_initialized(private$initialized)
      private$chroma$set <- glue::glue("set('{channel}', {value})")
    },
    get = function(channel) {
      if_initialized(private$initialized)
      private$chroma$get <- glue::glue("get('{channel}')")
    },
    luminance = function(lum = NULL, mode = "rgb") {
      if_initialized(private$initialized)
      if (is.null(lum)) {
        private$chroma$luminance <- "luminance()"
      } else {
        private$chroma$luminance <- glue::glue("luminance({lum}, {mode})", lum = lum, mode = glue::single_quote(mode))
      }
    },
    hex = function() {
      if_initialized(private$initialized)
      private$chroma$hex <- "hex()"
    },
    name = function() {
      if_initialized(private$initialized)
      private$chroma$name <- "name()"
    },
    css = function() {
      if_initialized(private$initialized)
      private$chroma$css <- "css()"
    },
    rgb = function(round = TRUE) {
      if_initialized(private$initialized)
      private$chroma$rgb <- glue::glue("rgb({tolower(round)})")
    },
    rgba = function(round = TRUE) {
      if_initialized(private$initialized)
      private$chroma$rgba <- glue::glue("rgba({tolower(round)})")
    },
    print = function() {
      code <- private$chroma
      code$sep <- "."
      code <- do.call(stri_c, code)
      code <- paste0(code, ";")
      return(code)
    },
    eval = function() {
      chromajs <- V8::v8()
      chromajs$source(file = system.file("chroma/chroma.min.js", package = "colorscale"))
      code <- private$chroma
      code$sep <- "."
      code <- do.call(stri_c, code)
      code <- paste0(code, ";")
      # print(code)
      # chromajs$eval(code)
      res <- vector(mode = "character", length = length(code))
      for (i in seq_along(code)) {
        res[i] <- chromajs$eval(code[i])
      }
      return(res)
    }
  ),
  private = list(
    initialized = FALSE,
    chroma = NULL
  )
)

