
#' @importFrom V8 v8
#' @importFrom stringi stri_split_fixed
#' @importFrom utils type.convert
eval_chroma <- function(code = NULL, type_convert = TRUE, split_char = ",") {
  chromajs <- V8::v8()
  chromajs$source(file = system.file("chroma/chroma.min.js", package = "colorscale"))
  code <- wrap_code(code)
  res <- vector(mode = "character", length = length(code))
  for (i in seq_along(code)) {
    res[i] <- chromajs$eval(code[i])
  }
  if (!is.null(split_char)) {
    res <- unlist(stri_split_fixed(str = res, pattern = split_char))
  }
  if (type_convert) {
    res <- type.convert(x = res, as.is = TRUE)
  }
  return(res)
}

#' @importFrom stringi stri_c
wrap_code <- function(code) {
  if ("colors" %in% names(code)) {
    code <- code[c(setdiff(names(code), "colors"), "colors")]
  }
  code$sep <- "."
  code <- do.call(stri_c, code)
  code <- paste0(code, ";")
  return(code)
}




#' @importFrom glue single_quote
add_quote <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) == 1) {
    if (is.null(x)) {
      return(NULL)
    }
    if (is.numeric(x)) {
      return(x)
    }
    if (is.logical(x)) {
      return(tolower(x))
    }
    return(glue::single_quote(x))
  } else {
    unlist(lapply(x, add_quote))
  }
}


is_initialized <- function(x, what = "color") {
  if (!isTRUE(x)) {
    stop(glue::glue("You must initialized a {what} !"), call. = FALSE)
  }
}
not_initialized <- function(x, what = "color") {
  if (isTRUE(x)) {
    stop(glue::glue("You must NOT initialized a {what} !"), call. = FALSE)
  }
}

