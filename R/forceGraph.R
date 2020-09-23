#' Create a force-directed graph widget
#'
#' This function creates a force-directed graph using \pkg{htmlwidgets}, which can be rendered
#' in R Markdown documents and shiny applications.
#' The rendering is conducted using a variant of the Sigma which is a JavaScript library dedicated
#' to graph drawing. The force-graph layout algorithm is ForceAtlas2
#' [http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0098679] and implemented by
#' Mathieu Jacomy [https://github.com/jacomyma], Guillaume Plique [https://github.com/Yomguithereal]
#' and Sébastien Heymann. [https://github.com/sheymann].
#'
#' @param message the message of the widget
#' @param width the width of the widget
#' @param height the height of the widget
#'
#' @import htmlwidgets
#'
forceGraph <- function(message, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    message = message
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'forceGraph',
    x,
    width = width,
    height = height,
    package = 'dfbmd'
  )
}

#' Shiny bindings for forceGraph
#'
#' Output and render functions for using forceGraph within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a forceGraph
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name forceGraph-shiny
#' @importFrom htmlwidgets shinyWidgetOutput
forceGraphOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'forceGraph', width, height, package = 'dfbmd')
}

#' @rdname forceGraph-shiny
#' @importFrom htmlwidgets shinyRenderWidget

renderForceGraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, forceGraphOutput, env, quoted = TRUE)
}
