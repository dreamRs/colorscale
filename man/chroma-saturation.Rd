% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chroma-utils.R
\name{chroma-saturation}
\alias{chroma-saturation}
\alias{chroma_saturate}
\alias{chroma_desaturate}
\title{Change saturation of a color}
\usage{
chroma_saturate(color, value = 1)

chroma_desaturate(color, value = 1)
}
\arguments{
\item{color}{Hexadecimal string or a name for color.}

\item{value}{Value for saturate / desaturate}
}
\value{
Hexadecimal string
}
\description{
Change saturation of a color
}
\examples{

chroma_saturate("#75b8d1")
chroma_desaturate("#75b8d1")

view_cols(c(
  chroma_desaturate("#75b8d1", 3),
  chroma_desaturate("#75b8d1"),
  "#75b8d1",
  chroma_saturate("#75b8d1"),
  chroma_saturate("#75b8d1", 3)
))
}
