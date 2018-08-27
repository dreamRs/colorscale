# colorscale

> Create a Color Scale From a Single Color & R6 api for 'chroma.js'.

[![Travis build status](https://travis-ci.org/dreamRs/colorscale.svg?branch=master)](https://travis-ci.org/dreamRs/colorscale)


## Installation

Install development version from GitHub with:

``` r
# with remotes
remotes::install_github("dreamRs/colorscale")

# or with install-github.me service (based on remotes)
source("https://install-github.me/dreamRs/colorscale")

# or with devtools:
devtools::install_github("dreamRs/colorscale")
```

## chroma.js API

Create a R6 class to access chroma.js methods:

```r
library(colorscale)

ch <- chroma$new("hotpink")
ch$darken(2)
ch$eval()
#> [1] "#930058"

hsl <- chroma$new()
hsl$chroma.hsl(330, 1, 0.6)
scales::show_col(hsl$eval())
hsl$darken(seq(0.2, 3, 0.2))
scales::show_col(hsl$eval())
```


Some methods have wrapper for easier use:

```r
# Mixes two colors
chroma_mix(color1 = "red", color2 = "blue")
#> [1] "#800080"

# Average between colors
chroma_avg(colors = c("#ddd", "yellow", "red", "teal"))
#> [1] "#b79757"

# Random colors
chroma_random()
#> [1] "#650a0c"
chroma_random(10)
#> [1] "#63e24b" "#909363" "#5a3d7d" "#41a505" "#8f5f13" "#df6535" "#da43d3" "#04fc8f" "#6ee31c" "#ac5c94"

# Euclidean distance between two colors
chroma_distance("#fff", "#ff0")
#> [1] 96.94758
chroma_distance("#fff", "#ff0", "rgb")
#> [1] 255
```

Convert color to specific color space:

````r
ch <- chroma$new("orange")
ch$hex()
#> [1] "#ffa500"
ch$rgb()
#> [1] 255 165   0
ch$hsl()
#> [1] 38.82353  1.00000  0.50000
ch$lab()
#> [1] 74.93565 23.93317 78.94978
ch$gl()
#> [1] 1.0000000 0.6470588 0.0000000 1.0000000
ch$css()
#> [1] "rgb(255,165,0)"
```

