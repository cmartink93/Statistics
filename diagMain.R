library(ggplot2)
library(lawstat)
library(GGally)
library(gridExtra)

# until unix is set up properly


args <- read.table('something.txt',header = TRUE, sep = "\t")

#main <- function(args) {
  #args <- commandArgs(trailingOnly = TRUE)
  #read in data
  filePath <-as.character(args$var[1])
  csvData <- read.csv(file = filePath, header = TRUE)
  outTxt <- as.character(args$var[7])
  
  #SLR or MLR?
  if(args$var[2]==1){
    source('diagSLR.R')
  }

  #LR Summary
  sink(outTxt)
  print("Model Summary")
  mSum <- getSummary(slrModel)
  print(mSum)
  sink()

  #predictions
  sink(outTxt,append=TRUE)
  print("Confidence Interval")
  print(xhConf)
  print("Prediction Interval")
  print(xhPred)
  sink()
  
  #anova
  sink(outTxt,append=TRUE)
  print("ANOVA Table")
  print(anovaTable)
  sink()
  
  #diagnostics (numeric)
  sink(outTxt,append=TRUE)
  print("Numeric Tests")
  print(numericTable)
  sink()
  
  #diagnostics (plot)
  
  pdf(str(args[7]) + "diagPlots.pdf") 
  diagPlots <- grid.arrange(scatPlot, resPlot, outlierPlot, normProbPlot, nrow = 2)
  dev.off()
  
  
#  }

#main()


#FROM UNIX
#Rscript rFileName arg1 ... argn