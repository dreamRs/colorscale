#' Chroma API
#'
#' @export
#'
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
    mix = function(color2, ratio = 0.5, mode = c("rgb", "hsl", "lab", "lrgb", "lch")) {
      is_initialized(private$initialized)
      mode <- match.arg(mode)
      if (!all(0 <= ratio & ratio <= 1))
        stop("'ratio' must be between 0 and 1 (inclusive)", call. = FALSE)
      private$chroma$mix <- glue::glue("mix('{color2}', {ratio}, '{mode}')")
    },
    average = function(colors, mode = c("rgb", "hsl", "lab", "lrgb", "lch")) {
      mode <- match.arg(mode)
      private$chroma$average <- glue::glue(
        "average([{colors}], '{mode}')",
        colors = stri_c(glue::single_quote(colors), collapse = ", "),
        mode = mode
      )
    },
    blend = function(color2, type = c("multiply", "darken", "lighten", "screen", "overlay", "burn", "dodge")) {
      is_initialized(private$initialized)
      type <- match.arg(type)
      private$chroma$blend <- glue::glue("blend('{color2}', '{type}')")
    },
    random = function() {
      private$chroma$random <- "random()"
    },
    contrast = function(color1, color2) {
      not_initialized(private$initialized)
      private$chroma$contrast <- glue::glue("contrast('{color1}', '{color2}')")
    },
    distance = function(color1, color2, mode = c("lab", "rgb", "hsl", "lrgb", "lch")) {
      not_initialized(private$initialized)
      mode <- match.arg(mode)
      private$chroma$distance <- glue::glue("distance('{color1}', '{color2}', '{mode}')")
    },

    # color
    alpha = function(value = NA) {
      is_initialized(private$initialized)
      private$chroma$alpha <- glue::glue("alpha({value})", .na = "")
    },
    brighten = function(value = 1) {
      is_initialized(private$initialized)
      private$chroma$brighten <- glue::glue("brighten({value})")
    },
    darken = function(value = 1) {
      is_initialized(private$initialized)
      private$chroma$darken <- glue::glue("darken({value})")
    },
    saturate = function(value = 1) {
      is_initialized(private$initialized)
      private$chroma$saturate <- glue::glue("saturate({value})")
    },
    desaturate = function(value = 1) {
      is_initialized(private$initialized)
      private$chroma$desaturate <- glue::glue("desaturate({value})")
    },
    set = function(channel, value) {
      is_initialized(private$initialized)
      private$chroma[[stri_c("set", channel, sep = ".")]] <- glue::glue("set('{channel}', {value})")
    },
    get = function(channel) {
      is_initialized(private$initialized)
      private$chroma$get <- glue::glue("get('{channel}')")
    },
    luminance = function(lum = NULL, mode = "rgb") {
      is_initialized(private$initialized)
      if (is.null(lum)) {
        private$chroma$luminance <- "luminance()"
      } else {
        private$chroma$luminance <- glue::glue("luminance({lum}, {mode})", lum = lum, mode = glue::single_quote(mode))
      }
    },


    # Convert methods
    hex = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "hex()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    name = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "name()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    css = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "css()"
      res <- self$eval(split_char = FALSE)
      private$chroma$output <- NULL
      return(res)
    },
    rgb = function(round = TRUE) {
      is_initialized(private$initialized)
      private$chroma$output <- glue::glue("rgb({tolower(round)})")
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    rgba = function(round = TRUE) {
      is_initialized(private$initialized)
      private$chroma$output <- glue::glue("rgba({tolower(round)})")
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    hsl = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "hsl()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    hsv = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "hsv()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    hsi = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "hsi()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    lab = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "lab()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    lch = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "lch()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    hcl = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "hcl()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },
    gl = function() {
      is_initialized(private$initialized)
      private$chroma$output <- "gl()"
      res <- self$eval()
      private$chroma$output <- NULL
      return(res)
    },



    # R methods
    print = function() {
      code <- private$chroma
      code <- wrap_code(code)
      return(code)
    },
    eval = function(type_convert = TRUE, split_char = ",") {
      code <- private$chroma
      res <- eval_chroma(code = code, type_convert = type_convert, split_char = split_char)
      return(res)
    }
  ),
  private = list(
    initialized = FALSE,
    chroma = NULL
  )
)






#' @title Chroma Scale API
#'
#' @description Create a color scale with chroma.js
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @importFrom glue glue single_quote
#' @importFrom stringi stri_c
#'
chroma_scale <- R6::R6Class(
  classname = "chroma_scale",
  public = list(
    initialize = function() {
      private$chroma$chroma <- "chroma"
    },
    # Scales
    scale = function(colors) {
      if (is.null(private$chroma$bezier))
        not_initialized(private$initialized, what = "scale")
      if (missing(colors)) {
        private$chroma$scale <- "scale()"
      } else {
        private$chroma$scale <- glue::glue(
          "scale([{colors}])",
          colors = stri_c(glue::single_quote(colors), collapse = ", ")
        )
      }
      private$initialized <- TRUE
    },
    bezier = function(colors) {
      not_initialized(private$initialized, what = "scale")
      private$chroma$bezier <- glue::glue(
        "bezier([{colors}])",
        colors = stri_c(glue::single_quote(colors), collapse = ", ")
      )
      private$initialized <- TRUE
    },
    mode = function(mode = c("rgb", "lab", "lrgb", "hsl", "lch")) {
      is_initialized(private$initialized, what = "scale")
      mode <- match.arg(mode)
      private$chroma$mode <- glue::glue("mode('{mode}')")
    },
    correctLightness = function() {
      is_initialized(private$initialized)
      private$chroma$correctLightness <- "correctLightness()"
    },
    colors = function(n) {
      is_initialized(private$initialized, what = "scale")
      if (length(n) == 1) {
        private$chroma$colors <- glue::glue("colors({n})")
      } else {
        private$chroma$colors <- glue::glue("([{n}])", n = stri_c(n, collapse = ", "))
      }
    },
    domain = function(domain) {
      is_initialized(private$initialized, what = "scale")
      private$chroma$domain <- glue::glue(
        "domain([{domain}])", domain = stri_c(n, collapse = ", ")
      )
    },
    gamma = function(gamma) {
      is_initialized(private$initialized, what = "scale")
      private$chroma$gamma <- glue::glue("gamma({gamma})")
    },
    padding = function(padding) {
      is_initialized(private$initialized, what = "scale")
      private$chroma$padding <- glue::glue("padding({padding})")
    },

    # R methods
    print = function() {
      code <- private$chroma
      code <- wrap_code(code)
      return(code)
    },
    eval = function(type_convert = TRUE, split_char = ",") {
      code <- private$chroma
      res <- eval_chroma(code = code, type_convert = type_convert, split_char = split_char)
      return(res)
    }

  ),
  private = list(
    initialized = FALSE,
    chroma = NULL
  )
)



