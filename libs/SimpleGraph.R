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
  
  ###Process TSV for D3JS
  classTable$processFPKMD3JS <- function(outputFilePath, outputName, letterList, letterFrequency) {
    sCommand <- sprintf("python '%s/%s' -o '%s' -n %s -l %s -f %s", LIBRARY_PATH, "FPKM.py", outputFilePath, outputName, shQuote(letterList), shQuote(letterFrequency))
    system(sCommand, intern=TRUE)
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
    png(file=sprintf("%s/%s.%d.%s", outputFilePath, "fpkm", iMaxNumber, "png"), width = 1024, height = 800, bg="transparent")
    print(imageRawData)
    dev.off()
    
    ###Save as SVG file
    svg(file=sprintf("%s/%s.%d.%s", outputFilePath, "fpkm", iMaxNumber, "svg"), width = 14, height = 7, bg="transparent")
    print(imageRawData)
    dev.off()
    
    ###Save as TSV for D3JS
    letterList <- paste(arrXLim, collapse=",")    
    letterFrequency <- paste(arrYLim, collapse=",")
    classTable$processFPKMD3JS(outputFilePath, sprintf("%s.%d", "fpkm", iMaxNumber), letterList, letterFrequency)
  }
  
  ###Initalize class
  classTable <- list2env(classTable)
  class(classTable) <- "SimpleGraph"
  return(classTable)
}