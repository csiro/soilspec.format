

library("scatterplot3d")
library(shiny)

library(plotly)


PlotSelection(spectraPopulation=reductSpecPopulation, samplePopulation=PCAsample$x, spikeIDs = spikeIDs$SpecIDs, outPath='c:/temp/index2.html')


