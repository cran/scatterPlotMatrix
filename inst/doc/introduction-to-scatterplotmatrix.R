## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7
)

## ----setup--------------------------------------------------------------------
library(scatterPlotMatrix)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", categoricalCS = "Set1")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Sepal.Length")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Sepal.Length", continuousCS = "YlOrRd")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", corrPlotType = "Text", corrPlotCS = "YlGnBu")

## -----------------------------------------------------------------------------
scatterPlotMatrix(mtcars)

## -----------------------------------------------------------------------------
categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
scatterPlotMatrix(mtcars, categorical = categorical, zAxisDim = "cyl")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", distribType = 1)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", regressionType = 1)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", rotateTitle = TRUE)

## -----------------------------------------------------------------------------
columnLabels <- gsub("\\.", "<br>", colnames(iris))
scatterPlotMatrix(iris, zAxisDim = "Species", columnLabels = columnLabels)

