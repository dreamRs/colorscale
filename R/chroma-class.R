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
        private$chroma$chroma <- glue::glue("chroma('{color}')")
      }
    },

    # API


    # color
    alpha = function(value = NA) {
      private$chroma$alpha <- glue::glue("alpha({value})", .na = "")
    },
    brighten = function(value = 1) {
      private$chroma$brighten <- glue::glue("brighten({value})")
    },
    darken = function(value = 1) {
      private$chroma$darken <- glue::glue("darken({value})")
    },
    saturate = function(value = 1) {
      private$chroma$saturate <- glue::glue("saturate({value})")
    },
    desaturate = function(value = 1) {
      private$chroma$desaturate <- glue::glue("desaturate({value})")
    },
    set = function(channel, value) {
      private$chroma$set <- glue::glue("set('{channel}', {value})")
    },
    get = function(channel) {
      private$chroma$get <- glue::glue("get('{channel}')")
    },
    luminance = function(lum = NULL, mode = "rgb") {
      if (is.null(lum)) {
        private$chroma$luminance <- "luminance()"
      } else {
        private$chroma$luminance <- glue::glue("luminance({lum}, {mode})", lum = lum, mode = glue::single_quote(mode))
      }
    },
    hex = function() {
      private$chroma$hex <- "hex()"
    },
    name = function() {
      private$chroma$name <- "name()"
    },
    css = function() {
      private$chroma$css <- "css()"
    },
    rgb = function(round = TRUE) {
      private$chroma$rgb <- glue::glue("rgb({tolower(round)})")
    },
    rgba = function(round = TRUE) {
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
    chroma = NULL
  )
)

