% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatterPlotMatrix.R
\name{setContinuousColorScale}
\alias{setContinuousColorScale}
\title{Tells which color scale to use for continuous columns.}
\usage{
setContinuousColorScale(id, continuousCsId)
}
\arguments{
\item{id}{Output variable to read from (id which references the requested plot).}

\item{continuousCsId}{One of the available color scale ids
("Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault",
 "Blues","Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd",
 "PuBuGn","PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd").}
}
\value{
No return value, called from shiny applications for side effects.
}
\description{
Tells which color scale to use for continuous columns.
}
\examples{
 if(interactive()) {
   library(shiny)
   library(scatterPlotMatrix)

   ui <- fluidPage(
     selectInput(
       "continuousCsSelect",
       "Continuous Color Scale:",
       choices = list(
         "Viridis" = "Viridis", "Inferno" = "Inferno", "Magma" = "Magma",
         "Plasma" = "Plasma", "Warm" = "Warm", "Cool" = "Cool", "Rainbow" ="Rainbow",
         "CubehelixDefault" = "CubehelixDefault", "Blues" = "Blues",
         "Greens" = "Greens", "Greys" = "Greys", "Oranges" = "Oranges",
         "Purples" = "Purples", "Reds" = "Reds", "BuGn" = "BuGn", "BuPu" = "BuPu",
         "GnBu" = "GnBu", "OrRd" = "OrRd", "PuBuGn" = "PuBuGn", "PuBu" = "PuBu",
         "PuRd" = "PuRd", "RdBu" = "RdBu", "RdPu" = "RdPu", "YlGnBu" = "YlGnBu",
         "YlGn" = "YlGn", "YlOrBr" = "YlOrBr", "YlOrRd" = "YlOrRd"
       ),
       selected = "Viridis"
     ),
     p("The selector controls the colors used when reference column is of type continuous"),
     scatterPlotMatrixOutput("spMatrix")
   )

   server <- function(input, output, session) {
     output$spMatrix <- renderScatterPlotMatrix({
       scatterPlotMatrix(iris, zAxisDim = "Sepal.Length")
     })
     observeEvent(input$continuousCsSelect, {
       scatterPlotMatrix::setContinuousColorScale("spMatrix", input$continuousCsSelect)
     })
   }

   shinyApp(ui, server)
 }

}
