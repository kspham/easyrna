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
          iTotalFPKM <- classTable$getFPKMCounter(itemName$sf, iMaxNumber)
          print(sprintf("Counter FPKM of %s : (%d)", itemName$name, iTotalFPKM))
          arrYLim <- append(arrYLim, iTotalFPKM)
          arrXLim <- append(arrXLim, sprintf("%s (%d)", itemName$name, iTotalFPKM))
        }      
      }
    }
    
    ###Create a title name
    yTitleName <- sprintf("FPKM > %d", iMaxNumber)
    xTitleName <- "RNA Order"
    
    ###Create plot data
    plotData <- data.frame(
      Samples=factor(arrXLim),
      FPKM=arrYLim,
      Title=c(1:length(arrYLim))
    )   
    
    ###Create graph data
    imageRawData <- ggplot(data = plotData, aes(x = Title, y = FPKM)) +
      geom_bar(aes(fill = Samples), stat='identity', width=.4) + geom_line() +
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