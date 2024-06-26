% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatterPlotMatrix.R
\name{setDistribType}
\alias{setDistribType}
\title{Distribution plots}
\usage{
setDistribType(id, distribType)
}
\arguments{
\item{id}{Output variable to read from (id which references the requested plot).}

\item{distribType}{Binary code indicating the type of distribution plot
(bit 1: histogram, bit 2: density plot).}
}
\value{
No return value, called from shiny applications for side effects.
}
\description{
Tells which type of representation to use for distribution plots.
}
\examples{
 if(interactive() && require(shiny)) {
   library(shiny)
   library(scatterPlotMatrix)

   ui <- fluidPage(
     selectInput(
       "distribType",
       "Distribution Representation:",
       choices = list("Histogram" = 2, "Density Plot" = 1),
       selected = 2
     ),
     p("The selector controls type of representation to use for distribution plots"),
     scatterPlotMatrixOutput("spMatrix")
   )

   server <- function(input, output, session) {
     output$spMatrix <- renderScatterPlotMatrix({
       scatterPlotMatrix(iris)
     })
     observeEvent(input$distribType, {
       scatterPlotMatrix::setDistribType("spMatrix", input$distribType)
     })
   }

   shinyApp(ui, server)
 }

}
