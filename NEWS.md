# scatterPlotMatrix 0.2.0

* Added:
  * new argument 'cssRules' to apply CSS rules
  * new argument 'scatterPlotProperties' to adjust some properties (size and color of points, etc.)
  * new argument and API 'cutoffs' to allow points filtering
  * 'multibrush' functionnality
  * new 'corrPlotType' values ('AbsText' and 'Empty')
  * send a 'zAxisChange' event when coloration is changed by clicking a column header

* Changed:
  * smooth density curves/reduce bands number
  * keep correlation circle positions unchanged from one tile to the next (and draw reference circles corresponding to a correlation of 1)
  * when 'corrPlotType' is 'Text' and if a categorical variable is selected for coloration, don't use a gradient, use the color associated to each category
  * set position of tooltips to the right side
  * when mouse hovers a point, highlighted point just has to be drawn greater, keeping its color (not black)
  * when mouse hovers a point, print values for all visible columns

* Fixed:
  * generating two times a plot with a categorical zAxis, some correlations and distribPlots are wrong
  * wrong display for 'CorrelationPlot' values if filtering keeps no points
  * Y axis of distributionPlot are not updated when filtered points are changed
  * 'setZAxis' should not log a warning when 'controlWidgets' is not active
  * don't send a 'ZAXIS_EVENT' when the zAxis is not set to a new value

# scatterPlotMatrix 0.1.0

* Added a `NEWS.md` file to track changes to the package.
