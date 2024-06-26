% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatterPlotMatrix.R
\name{setCutoffs}
\alias{setCutoffs}
\title{Cutoffs values}
\usage{
setCutoffs(id, cutoffs)
}
\arguments{
\item{id}{output variable to read from (id which references the requested plot)}

\item{cutoffs}{List of \code{SpCutoff};
a \code{SpCutoff} is a list defining a \code{xDim}, \code{yDim} and a list of \code{xyCutoff};
a \code{xyCutoff} is a pair of \code{cutoff} (one for x axis, one for y axis);
a \code{cutoff} is a list containing two values (min and max values)
or \code{NULL} if there is no cutoff to apply for this axis;
\code{NULL} is allowed, meaning there is no cutoff to apply.}
}
\value{
No return value, called from shiny applications for side effects.
}
\description{
Tells which cutoffs to use for each pair of columns.
It's possible to filter some points by defining cutoffs to apply to columns.
}
\examples{
 if(interactive() && require(shiny)) {
   library(shiny)
   library(scatterPlotMatrix)

   ui <- fluidPage(
     checkboxInput("setosaCB", "Setosa", TRUE),
     checkboxInput("versicolorCB", "Versicolor", TRUE),
     checkboxInput("viginicaCB", "Viginica", TRUE),
     scatterPlotMatrixOutput("spMatrix")
   )

   server <- function(input, output, session) {
     output$spMatrix <- renderScatterPlotMatrix({
       scatterPlotMatrix(
         data = iris,
         zAxisDim = "Species"
       )
     })

     observe({
       speciesCBs = c(input$setosaCB, input$versicolorCB, input$viginicaCB)
       toKeepIndexes <- Filter(function(i) speciesCBs[i], 1:length(speciesCBs))
       xyCutoffs <- sapply(toKeepIndexes, function(i) {
         list(list(NULL, c(i - 1.1, i - 0.9)))
       })
       scatterPlotMatrix::setCutoffs("spMatrix", list(
         list(xDim="Sepal.Length", yDim="Species", xyCutoffs = xyCutoffs)
       ))
     })
   }
   shinyApp(ui, server)
 }

}
