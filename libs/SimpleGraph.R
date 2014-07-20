#!/usr/local/bin/Rscript
###Created on Jul 13, 2014
###Simple graph class

SimpleGraph <- function(debugFlag = FALSE){
  ###Class description
  information = list(
    author = "Linuxpham <thaihoabo@gmail.com>",
    contact = "thaihoabo@gmail.com",
    version = "1.0",
    date = "2014-07-13"
  )  
      
  ###Class table
  classTable = list(
    infor = information,
    debug = debugFlag,
    get = function(x) classTable[[x]],
    set = function(x, value) classTable[[x]] <<- value
  )
  
  ###Show class information
  classTable$showClassInfo = function(){
    print(classTable$infor)
  }
   
  ###Get counter number of RNAs that have FPKM > 5
  classTable$getFPKMCounter <- function(quantFilePath, iMaxNumber){
    ###Get all frame data
    arrFrameData <- read.table(quantFilePath, header = FALSE, sep = "\t")
    
    ###Sum the max TMP
    iTotalLine <- sum(arrFrameData$V3 >= iMaxNumber)
    
    ###Return data
    return(iTotalLine)
  }
  
  ###Draw FPKM graph
  classTable$drawFPKM <- function(arrInputData, outputFilePath, iMaxNumber) {
    ###Debug information
    if(classTable$debug == TRUE) {      
      for (iLoop in 1:length(arrInputData)) {
        print(arrInputData[[iLoop]])
      }
    }    
    
    ###Create vector for graph
    arrXLim <- c()
    arrYLim <- c()
    
    ###Loop comparing number to check FPKM
    for (iLoop in 1:length(arrInputData)) {
      ###Loop group in everyone comparing
      for (groupName in arrInputData[[iLoop]]) {
        ###Loop items in a group
        for (itemName in groupName$items) {
          print(sprintf("Counter FPKM of sample: %s", itemName$name))
          arrXLim <- append(arrXLim, itemName$name)
          arrYLim <- append(arrYLim, classTable$getFPKMCounter(itemName$sf, iMaxNumber))
        }      
      }
    }
    
    ###Create a title name
    yTitleName <- sprintf("FPKM > %d", iMaxNumber)
    xTitleName <- "RNA Samples"
    
    ###Create plot data
    plotData <- data.frame(
      Sample=factor(arrXLim),
      FPKM=arrYLim
    )   
    
    ###Create graph data
    imageRawData <- ggplot(data = plotData,aes(x = Sample, y = FPKM)) +
      geom_bar(colour = 'black', stat='identity', width=.4) + 
      labs(x = xTitleName, y = yTitleName) +
      theme(legend.text = element_text(size = 20, colour = "red"))
    
    ###Save as PNG file cairo-png
    png(file=outputFilePath, bg="transparent")
    print(imageRawData)
    dev.off()
  }
  
  ###Initalize class
  classTable <- list2env(classTable)
  class(classTable) <- "SimpleGraph"
  return(classTable)
}