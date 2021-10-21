#' htmlwidget for d3.js scatter plot matrix
#'
#' @param data 
#'   data.frame with data to use in the chart.
#' @param controlWidgets 
#'   Tells if some widgets must be available to control plot; 
#'   \code{NULL} is allowed, meaning that '!HTMLWidgets.shinyMode' is to use; 
#'   default value is \code{FALSE}.
#' @param categorical 
#'   List of list (one for each data column) containing the name of available categories, 
#'   or \code{NULL} if column corresponds to continuous data; 
#'   \code{NULL} is allowed, meaning all columns are continuous.
#' @param inputColumns 
#'   List of boolean (one for each data column), \code{TRUE} for an input column, \code{FALSE} for an output column; 
#'   \code{NULL} is allowed, meaning all columns are inputs.
#' @param keptColumns 
#'   List of boolean (one for each data column), \code{FALSE} if column has to be ignored; 
#'   \code{NULL} is allowed, meaning all columns are available.
#' @param zAxisDim 
#'   Name of the column represented by z axis (used to determine the color to attribute to a point); 
#'   \code{NULL} is allowed, meaning there is no coloring to apply.
#' @param distribType 
#'   Binary code indicating the type of distribution plot (bit 1: density plot, bit 2: histogram).
#' @param regressionType 
#'   Binary code indicating the type of regression plot (bit 1: linear, bit 2: loess).
#' @param corrPlotType 
#'   String indicating the type of correlation plots to use;
#'   supported values: \code{Circles} to use a circle tree map, \code{Text} to use simple text labels; 
#'   default value is \code{Circles}.
#' @param corrPlotCS 
#'   Name of the color Scale to use for correlation plot when plot type is 'Text';
#'   supported names: "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault", 
#'   "Blues","Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd", "PuBuGn","PuBu", 
#'   "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"; 
#'   default value is \code{Plasma}.
#' @param rotateTitle 
#'   \code{TRUE} if column title must be rotated.
#' @param columnLabels 
#'   List of string (one for each data column) to display in place of column name found in data, 
#'   or \code{NULL} if there is no alternative name; 
#'   \code{NULL} is allowed, meaning all columns are without alternative name; 
#'   \code{<br>} can be used to insert line breaks.
#' @param continuousCS 
#'   Name of the color Scale to use for continuous data;
#'   supported names: "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault", 
#'   "Blues","Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd", "PuBuGn","PuBu", 
#'   "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"; 
#'   default value is \code{Viridis}.
#' @param categoricalCS 
#'   Name of the color Scale to use for categorical data; 
#'   supported names: Category10, Accent, Dark2, Paired, Set1; 
#'   default value is \code{Category10}.
#' @param eventInputId 
#'   When plot event occured, reactive input to write to; \code{NULL} is allowed, default value is 'plotEvent'.
#' @param width 
#'   Integer in pixels defining the width of the widget.
#' @param height 
#'   Integer in pixels defining the height of the widget.
#' @param elementId 
#'   Unique \code{CSS} selector id for the widget.
#'
#' @examples
#'  if(interactive()) {
#'    library(scatterPlotMatrix)
#'
#'    scatterPlotMatrix(iris, zAxisDim = "Species")
#'    # Each point has a color depending of its 'Species' value
#'
#'    categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
#'    scatterPlotMatrix(mtcars, categorical = categorical, zAxisDim = "cyl")
#'    # 'cyl' and four last columns have a box representation for its categories
#'    # (use top slider to see the last three columns)
#'
#'    scatterPlotMatrix(iris, zAxisDim = "Species", distribType = 1)
#'    # Distribution plots are of type 'density plot' (instead of histogram)
#'
#'    scatterPlotMatrix(iris, zAxisDim = "Species", regressionType = 1)
#'    # Add linear regression plots
#'
#'    columnLabels <- gsub("\\.", "<br>", colnames(iris))
#'    scatterPlotMatrix(iris, zAxisDim = "Species", columnLabels = columnLabels)
#'    # Given names are displayed in place of dataset column names; <br> is used to insert line breaks
#'  }
#'
#' @importFrom htmlwidgets createWidget sizingPolicy shinyWidgetOutput shinyRenderWidget
#'
#' @export
scatterPlotMatrix <- function(
  data, 
  controlWidgets = FALSE, 
  categorical = NULL, 
  inputColumns = NULL, 
  keptColumns = NULL, 
  zAxisDim = NULL, 
  distribType = 2, 
  regressionType = 0, 
  corrPlotType = 'Circles', 
  corrPlotCS = 'Plasma', 
  rotateTitle = FALSE, 
  columnLabels = NULL, 
  continuousCS = 'Viridis', 
  categoricalCS = 'Category10', 
  eventInputId = NULL, 
  width = NULL, 
  height = NULL,
  elementId = NULL
) {

  args <- checkSpmArgs(
    list(
      data = data,
      rowLabels = rownames(data),
      controlWidgets = controlWidgets,
      categorical = categorical,
      inputColumns = inputColumns,
      keptColumns = keptColumns,
      zAxisDim = zAxisDim,
      distribType = distribType,
      regressionType = regressionType,
      corrPlotType = corrPlotType,
      corrPlotCS = corrPlotCS,
      rotateTitle = rotateTitle,
      columnLabels = columnLabels,
      continuousCS = continuousCS,
      categoricalCS = categoricalCS,
      eventInputId = eventInputId
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'scatterPlotMatrix',
    args,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.defaultHeight = 960, browser.defaultWidth = 'auto'),
    package = 'scatterPlotMatrix',
    elementId = elementId
  )
}

#' Shiny bindings for scatterPlotMatrix
#'
#' Output and render functions for using scatterPlotMatrix within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a scatterPlotMatrix
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name scatterPlotMatrix-shiny
#'
#' @export
scatterPlotMatrixOutput <- function(outputId, width = '100%', height = '600px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'scatterPlotMatrix', width, height, package = 'scatterPlotMatrix')
}

#' @rdname scatterPlotMatrix-shiny
#' @export
renderScatterPlotMatrix <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, scatterPlotMatrixOutput, env, quoted = TRUE)
}

checkSpmArgs <- function(args) {
  return (
    checkEventInputId(
      checkCategoricalCS(
        checkContinuousCS(
          checkColumnLabels(
            checkColumnLabels(
              checkRotateTitle(
                checkRegressionType(
                  checkCorrPlotCS(
                    checkCorrPlotType(
                      checkDistribType(
                        checkZAxisDim(
                          checkInputColumns(
                            checkKeptColumns(
                              checkCategorical(
                                checkData(args)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

checkData <- function(args) {
  if (!is.data.frame(args$data) && !is.matrix(args$data)) {
    stop("'data' must be a dataframe")
  }
  return(args)
}

checkCategorical <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$categorical) && !is.list(args$categorical)) {
    message("'categorical' must be a list")
    args["categorical"] <- list(NULL)
  }
  if (!is.null(args$categorical)) {
    args$categorical <- as.list(args$categorical)
    if (colCount != length(args$categorical)) {
      message("Length of 'categorical' must be equal to the number of columns of 'data'")
      args["categorical"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$categorical))) {
        if (!is.null(args$categorical[[i]])) {
          if (is.vector(args$categorical[[i]])) {
            args$categorical[[i]] <- as.list(args$categorical[[i]])
          }
          else {
            message(paste("categorical", i, "must be a vector"))
            args[["categorical"]][i] <- list(NULL)
          }
        }
      }
    }
  }
  else {
    # Try some automatic generations
    categorical <- lapply(seq_len(ncol(args$data)), function(icol) {
      if (is.factor(args$data[, icol])) {
        categories <- as.list(levels(args$data[, icol]))
        if (length(categories) < 8) {
          return(categories)
        }
      }
      return(NULL)
    })
    args["categorical"] <- list(categorical)
  }
  return(args)
}

checkKeptColumns <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$keptColumns) && !is.vector(args$keptColumns)) {
    message("'keptColumns' must be a vector")
    args["keptColumns"] <- list(NULL)
  }
  if (!is.null(args$keptColumns)) {
    if (colCount != length(args$keptColumns)) {
      message("Length of 'keptColumns' must be equal to the number of columns of 'data'")
      args["keptColumns"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$keptColumns))) {
        if (!is.logical(args$keptColumns[[i]])) {
          message(paste("keptColumns", i, "must be of logical type"))
          args[["keptColumns"]][i] <- TRUE
        }
      }
    }
  }
  return(args)
}

checkInputColumns <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$inputColumns) && !is.vector(args$inputColumns)) {
    message("'inputColumns' must be a vector")
    args["inputColumns"] <- list(NULL)
  }
  if (!is.null(args$inputColumns)) {
    if (colCount != length(args$inputColumns)) {
      message("Length of 'inputColumns' must be equal to the number of columns of 'data'")
      args["inputColumns"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$inputColumns))) {
        if (!is.logical(args$inputColumns[[i]])) {
          message(paste("inputColumns", i, "must be of logical type"))
          args[["inputColumns"]][i] <- TRUE
        }
      }
    }
  }
  return(args)
}

checkZAxisDim <- function(args) {
  colNames <- colnames(args$data)
  if (!is.null(args$zAxisDim) && is.na(match(args$zAxisDim, colNames))) {
    message(paste("zAxisDim:", args$zAxisDim, "must be a valid column dimension, it must be one of:", toString(colNames)))
    args["zAxisDim"] <- list(NULL)
  }
  return(args)
}
  
checkDistribType <- function(args) {
  if (!is.numeric(args$distribType)) {
    message("'distribType' must be of numeric type")
    args["distribType"] <- 2
  }
  else {
    if (is.na(match(args$distribType, 0:3))) {
      message(paste("distribType:", args$distribType, "must be one of:", toString(0:3)))
      args["distribType"] <- 2
    }
  }
  return(args)
}

checkRegressionType <- function(args) {
  if (!is.numeric(args$regressionType)) {
    message("'regressionType' must be of numeric type")
    args["regressionType"] <- 0
  }
  else {
    if (is.na(match(args$regressionType, 0:3))) {
      message(paste("regressionType:", args$regressionType, "must be one of:", toString(0:3)))
      args["regressionType"] <- 0
    }
  }
  return(args)
}

checkCorrPlotType <- function(args) {
  corrPlotTypes <- c("Circles", "Text")
  if (is.na(match(args$corrPlotType, corrPlotTypes))) {
    message(paste("corrPlotType:", args$corrPlotType, "must be a valid correlation plot type, it must be one of:", toString(corrPlotTypes)))
    args$corrPlotType <- corrPlotTypes[1]
  }
  return(args)
}

checkCorrPlotCS <- function(args) {
  continuousCSList <- c(
    "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault", 
    "Blues","Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd", 
    "PuBuGn","PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"
  )
  if (is.na(match(args$corrPlotCS, continuousCSList))) {
    message(paste("corrPlotCS:", args$corrPlotCS, "must be a valid continuous color scale name, it must be one of:", toString(continuousCSList)))
    args$corrPlotCS <- continuousCSList[4]
  }
  return(args)
}

checkRotateTitle <- function(args) {
  if (!is.logical(args$rotateTitle)) {
    message("'rotateTitle' must be of logical type")
    args["rotateTitle"] <- FALSE
  }
  return(args)
}

checkColumnLabels <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$columnLabels) && !is.vector(args$columnLabels)) {
    message("'columnLabels' must be a vector")
    args["columnLabels"] <- list(NULL)
  }
  if (!is.null(args$columnLabels)) {
    if (colCount != length(args$columnLabels)) {
      message("Length of 'columnLabels' must be equal to the number of columns of 'data'")
      args["columnLabels"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$columnLabels))) {
        if (!is.character(args$columnLabels[[i]])) {
          message(paste("columnLabels", i, "must be of character type"))
          args[["columnLabels"]][i] <- list(NULL)
        }
      }
    }
  }
  return(args)
}
  
checkContinuousCS <- function(args) {
  continuousCSList <- c(
    "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault", 
    "Blues","Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd", 
    "PuBuGn","PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"
  )
  if (is.na(match(args$continuousCS, continuousCSList))) {
    message(paste("continuousCS:", args$continuousCS, "must be a valid continuous color scale name, it must be one of:", toString(continuousCSList)))
    args$continuousCS <- continuousCSList[1]
  }
  return(args)
}

checkCategoricalCS <- function(args) {
  categoricalCSList <- c("Category10", "Accent", "Dark2", "Paired", "Set1")
  if (is.na(match(args$categoricalCS, categoricalCSList))) {
    message(paste("categoricalCS:", args$categoricalCS, "must be a valid categorical color scale name, it must be one of:", toString(categoricalCSList)))
    args["categoricalCS"] <- categoricalCSList[1]
  }
  return(args)
}
  
checkEventInputId <- function(args) {
  if (!is.null(args$eventInputId) && !is.character(args$eventInputId)) {
    message("'eventInputId' must be of character type")
    args["eventInputId"] <- list(NULL)
  }
  return(args)
}

#' Tells which type of representation to use for distribution plots.
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param distribType 
#'   Binary code indicating the type of distribution plot (bit 1: histogram, bit 2: density plot).
#'
#' @return 
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "distribType", 
#'        "Distribution Representation:", 
#'        choices = list("Histogram" = 2, "Density Plot" = 1), 
#'        selected = 2
#'      ),
#'      p("The selector controls type of representation to use for distribution plots"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'      observeEvent(input$distribType, {
#'        scatterPlotMatrix::setDistribType("spMatrix", input$distribType)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setDistribType <- function(id, distribType) {
  method <- "setDistribType"
  callJS()
}

#' Tells which type of regression to use for regression plots.
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param regressionType 
#'   Binary code indicating the type of regression plot (bit 1: linear, bit 2: loess).
#'
#' @return 
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      checkboxInput("linearRegressionCB", "Linear Regression", FALSE),
#'      checkboxInput("loessCB", "Local Polynomial Regression", FALSE),
#'      p("The chech boxes controls type of regression to use for regression plots"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'      observe({
#'        linearFlag <- ifelse(input$linearRegressionCB, 1, 0)    
#'        loessFlag <- ifelse(input$loessCB, 2, 0)    
#'        scatterPlotMatrix::setRegressionType("spMatrix", linearFlag + loessFlag)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setRegressionType <- function(id, regressionType) {
  method <- "setRegressionType"
  callJS()
}

#' Tells which type of correlation plot to use.
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param corrPlotType 
#'   One of the available correlation plot types (Circles, Text).
#'
#' @return 
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "corrPlotTypeSelect", 
#'        "Correlation Plot Type:", 
#'        choices = list("Circles" = "Circles", "Text" = "Text"), 
#'        selected = "Circles"
#'      ),
#'      p("The selector controls the type of correlation to use"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris, zAxisDim = "Sepal.Length", continuousCS = "Plasma")
#'      })
#'      observeEvent(input$corrPlotTypeSelect, {
#'        scatterPlotMatrix::setCorrPlotType("spMatrix", input$corrPlotTypeSelect)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCorrPlotType <- function(id, corrPlotType) {
  method <- "setCorrPlotType"
  callJS()
}

#' Tells which color scale to use for correlation plots.
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param corrPlotCsId 
#'   One of the available color scale ids 
#'   ("Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault", 
#'    "Blues","Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd", 
#'    "PuBuGn","PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd").
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "corrPlotCsSelect", 
#'        "Correlation Plot Color Scale:", 
#'        choices = list(
#'          "Viridis" = "Viridis", "Inferno" = "Inferno", "Magma" = "Magma", 
#'          "Plasma" = "Plasma", "Warm" = "Warm", "Cool" = "Cool", "Rainbow" ="Rainbow", 
#'          "CubehelixDefault" = "CubehelixDefault", "Blues" = "Blues", 
#'          "Greens" = "Greens", "Greys" = "Greys", "Oranges" = "Oranges", 
#'          "Purples" = "Purples", "Reds" = "Reds", "BuGn" = "BuGn", "BuPu" = "BuPu", 
#'          "GnBu" = "GnBu", "OrRd" = "OrRd", "PuBuGn" = "PuBuGn", "PuBu" = "PuBu", 
#'          "PuRd" = "PuRd", "RdBu" = "RdBu", "RdPu" = "RdPu", "YlGnBu" = "YlGnBu", 
#'          "YlGn" = "YlGn", "YlOrBr" = "YlOrBr", "YlOrRd" = "YlOrRd"
#'        ), 
#'        selected = "Plasma"
#'      ),
#'      p("The selector controls the color scale to use for correlation plot 
#'         when plot type is 'Text'"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris, corrPlotType = "Text")
#'      })
#'      observeEvent(input$corrPlotCsSelect, {
#'        scatterPlotMatrix::setCorrPlotCS("spMatrix", input$corrPlotCsSelect)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCorrPlotCS <- function(id, corrPlotCsId) {
  method <- "setCorrPlotCS"
  callJS()
}

#' Tells which color scale to use for continuous columns.
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param continuousCsId 
#'   One of the available color scale ids 
#'   ("Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault", 
#'    "Blues","Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd", 
#'    "PuBuGn","PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd").
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "continuousCsSelect", 
#'        "Continuous Color Scale:", 
#'        choices = list(
#'          "Viridis" = "Viridis", "Inferno" = "Inferno", "Magma" = "Magma", 
#'          "Plasma" = "Plasma", "Warm" = "Warm", "Cool" = "Cool", "Rainbow" ="Rainbow", 
#'          "CubehelixDefault" = "CubehelixDefault", "Blues" = "Blues", 
#'          "Greens" = "Greens", "Greys" = "Greys", "Oranges" = "Oranges", 
#'          "Purples" = "Purples", "Reds" = "Reds", "BuGn" = "BuGn", "BuPu" = "BuPu", 
#'          "GnBu" = "GnBu", "OrRd" = "OrRd", "PuBuGn" = "PuBuGn", "PuBu" = "PuBu", 
#'          "PuRd" = "PuRd", "RdBu" = "RdBu", "RdPu" = "RdPu", "YlGnBu" = "YlGnBu", 
#'          "YlGn" = "YlGn", "YlOrBr" = "YlOrBr", "YlOrRd" = "YlOrRd"
#'        ), 
#'        selected = "Viridis"
#'      ),
#'      p("The selector controls the colors used when reference column is of type continuous"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris, zAxisDim = "Sepal.Length")
#'      })
#'      observeEvent(input$continuousCsSelect, {
#'        scatterPlotMatrix::setContinuousColorScale("spMatrix", input$continuousCsSelect)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setContinuousColorScale <- function(id, continuousCsId) {
  method <- "setContinuousColorScale"
  callJS()
}

#' Tells which color scale to use for categorical columns.
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param categoricalCsId 
#'   One of the available color scale ids (Category10, Accent, Dark2, Paired, Set1).
#'
#' @return 
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "categoricalCsSelect", 
#'        "Categorical Color Scale:", 
#'        choices = list(
#'          "Category10" = "Category10", "Accent" = "Accent", "Dark2" = "Dark2", 
#'          "Paired" = "Paired", "Set1" = "Set1"
#'        ), 
#'        selected = "Category10"
#'      ),
#'      p("The selector controls the colors used when reference column is of type categorical"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris, zAxisDim = "Species")
#'      })
#'      observeEvent(input$categoricalCsSelect, {
#'        scatterPlotMatrix::setCategoricalColorScale("spMatrix", input$categoricalCsSelect)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCategoricalColorScale <- function(id, categoricalCsId) {
  method <- "setCategoricalColorScale"
  callJS()
}

#' Column visibitity
#'
#' Tells which columns have to be visible.
#'
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param keptColumns 
#'   Vector of boolean (one for each data column), \code{FALSE} if column has to be hidden. 
#'   A named list can also be provided to only indicate which columns must be assigned to a new visibility.
#'
#' @return 
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      checkboxInput("hideColumnsCB", "Hide last columns", FALSE),
#'      p("The check box controls the visibility of the two last columns"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'      observeEvent(input$hideColumnsCB, {
#'        keptColumns <- vapply(
#'          1:ncol(iris), 
#'          function(i) {
#'            return(ifelse(input$hideColumnsCB, ncol(iris) - i >= 2, TRUE))
#'          },
#'          logical(1)
#'        )
#'        scatterPlotMatrix::setKeptColumns("spMatrix", keptColumns)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setKeptColumns <- function(id, keptColumns) {
  method <- "setKeptColumns"
  callJS()
}

#' This function allows to set the type of interaction; three types of mouse interactions are available ('tooltip', 'filter' or 'zoom').
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param interactionType 
#'   Type of mouse interaction.
#'
#' @return 
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "mouseMode", 
#'        "Mouse Interactions:", 
#'        c("Tooltip" = "tooltip", "Filter" = "filter", "Zoom" = "zoom")
#'      ),
#'      p("The selector controls the type of mouse interactions with the scatterPlotMatrix"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'      observe({
#'        scatterPlotMatrix::changeMouseMode("spMatrix", input$mouseMode)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
changeMouseMode <- function(id, interactionType) {
  method <- "changeMouseMode"
  callJS()
}

#' Tells which dim is to display on Z axis.
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param dim 
#'   Is to display on X axis.
#'
#' @return 
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      fluidRow(
#'        column(
#'          2,
#'          selectInput("zAxisSelect", "Z Axis:", colnames(iris))
#'        ),
#'        column(
#'          2,
#'          checkboxInput("zAxisUsedCB", "Use Z Axis", FALSE)
#'        )
#'      ),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'        
#'      observe({
#'        scatterPlotMatrix::setZAxis("spMatrix", if (input$zAxisUsedCB) input$zAxisSelect else NULL)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setZAxis <- function(id, dim) {
  method <- "setZAxis"
  callJS()
}

#' Asks to retrieve the plot configuration. 
#' Result will be sent through a reactive input.
#' @param id 
#'   Output variable to read from (id which references the requested plot).
#' @param configInputId 
#'   Reactive input to write to. 
#'
#' @return 
#'   No return value, called from shiny applications for side effects.
#'
#' @export
getPlotConfig <- function(id, configInputId) {
  method <- "getPlotConfig"
  callJS()
}

callJS <- function() {
  message <- Filter(function(x) !is.symbol(x), as.list(parent.frame(1)))
  session <- shiny::getDefaultReactiveDomain()
  method <- paste0("scatterPlotMatrix:", message$method)
  session$sendCustomMessage(method, message)
}
