
#' Color Scale Shiny Gadget
#'
#' Interactively create a color palette from a unique color
#'
#' @param color Hexadecimal string or a name for color.
#'
#' @export
#'
#' @importFrom miniUI miniPage miniContentPanel
#' @importFrom htmltools tags
#' @importFrom shiny uiOutput renderUI runGadget paneViewer actionButton
#'  sliderInput splitLayout icon dialogViewer stopApp observeEvent reactiveValues
#' @importFrom shinyWidgets spectrumInput chooseSliderSkin
#'
#' @examples
#' \dontrun{
#'
#' if (interactive()) {
#'
#' # Launch the gadget with :
#' color_scale()
#'
#' }
#'
#' }
color_scale <- function(color = "#1D9A6C") {
  stopifnot(length(color) == 1)

  ui <- miniPage(
    chooseSliderSkin("Modern", color = "#112446"),
    # style sheet
    tags$link(rel="stylesheet", type="text/css", href="colorscale/styles.css"),
    # title
    tags$div(
      class="gadget-title dreamrs-title-box",
      tags$h1(icon("paint-brush"), "Color Scale from one color", class = "dreamrs-title"),
      actionButton(
        inputId = "close", label = "Close",
        class = "btn-sm pull-left"
      )
    ),
    # body
    miniContentPanel(
      padding = 10,
      spectrumInput(
        inputId = "main_col",
        label = "Choose a color:",
        selected = color,
        options = list(
          "show-buttons" = FALSE,
          "preferred-format" = "hex",
          "show-input" = TRUE
        )
      ),
      tags$br(),
      tags$b("Output palette:"),
      uiOutput(outputId = "rect_cols"),
      tags$br(),
      # tags$b("Parameters:"),
      splitLayout(
        tags$div(
          # width = 6,
          # tags$p("Dark colors"),
          sliderInput(
            inputId = "n_dark",
            label = "Number of dark colors:",
            min = 1,
            max = 10,
            value = 4,
            width = "90%"
          ),
          sliderInput(
            inputId = "p_dark",
            label = "Percentage of darkness:",
            min = 0,
            max = 100,
            value = 50,
            post = "%",
            width = "90%"
          ),
          sliderInput(
            inputId = "a_dark",
            label = "Dark hue angle:",
            min = -360,
            max = 360,
            value = -51,
            post = "\u00b0",
            width = "90%"
          ),
          sliderInput(
            inputId = "s_dark",
            label = "Dark colors saturation:",
            min = -100,
            max = 100,
            value = -14,
            post = "%",
            width = "90%"
          )
        ),
        tags$div(
          # width = 6,
          # tags$p("Light colors"),
          sliderInput(
            inputId = "n_light",
            label = "Number of light colors:",
            min = 1,
            max = 10,
            value = 6,
            width = "90%"
          ),
          sliderInput(
            inputId = "p_light",
            label = "Percentage of lightness:",
            min = 0,
            max = 100,
            value = 80,
            post = "%",
            width = "90%"
          ),
          sliderInput(
            inputId = "a_light",
            label = "Light hue angle:",
            min = -360,
            max = 360,
            value = 67,
            post = "\u00b0",
            width = "90%"
          ),
          sliderInput(
            inputId = "s_light",
            label = "Light colors saturation:",
            min = -100,
            max = 100,
            value = 20,
            post = "%",
            width = "90%"
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    result_scale <- reactiveValues(colors = NULL)

    output$rect_cols <- renderUI({
      color <- input$main_col
      res_colors <- single_scale(
        color = color,
        n_dark = input$n_dark,
        darkness = input$p_dark / 100,
        rotate_dark = input$a_dark,
        saturation_dark = input$s_dark / 100,
        n_light = input$n_light,
        lightness = input$p_light / 100,
        rotate_light = input$a_light,
        saturation_light = input$s_light / 100
      )
      result_scale$colors <- res_colors
      colors_rect(colors = res_colors)
    })

    observeEvent(input$close, stopApp())

  }

  runGadget(app = ui, server = server, viewer = paneViewer(minHeight = 600)) # minHeight = "maximize"
}




