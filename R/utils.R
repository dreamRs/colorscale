
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
