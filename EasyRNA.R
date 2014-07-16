#!/usr/local/bin/Rscript
###Created on Jul 13, 2014
###Easy RNA script tools
allLibs <- c("rjson", "ggplot2", "RColorBrewer", "Cairo")
lapply(allLibs, library, character.only = TRUE)

###Get current directory
currDir <- getwd()

###Get library path
currLiraryPath <- sprintf("%s%s", currDir,"/libs")

###Load all R script
file.sources <- list.files(c(currLiraryPath), pattern="*.R$", full.names=TRUE, ignore.case=TRUE)
sapply(file.sources, source, .GlobalEnv)

###Flush all console
flush.console()

###Create help funciton
help <- function() {
  print("EasyRNA tools - Version 1.0")
  print("Author : Hoa.PT and Son.PK")
  print("Using  : Rscript EasyRNA.R <Action> <Json_Data>")
  print("                           +fpkm ..............")
  print("                           +diff ..............")
  quit()
}

###Get all ARGS data
args <- commandArgs(TRUE)

###Check arguments data
if(length(args) < 1) {  
  help()
}

###Check action of EasyRNA tool
switch(args[1],
  ###Plot of  the number of RNAs that have FPKM > MAX_NUMBER
  fpkm={    
    ###Create Simple Graph
    oSimpleGraph <- SimpleGraph()
    
    ###Check input data of FPKM
    if(length(args) < 3) {
      help()
    }
        
    ###Create JSON parser
    oParser <- newJSONParser()
    
    ###Add JSON data to parse
    oParser$addData(args[2])    
    
    ###Get all input data
    arrInputData <- oParser$getObject()
    
    ###Debug information
    #for (iLoop in 1:length(arrInputData)) {
    #  print(arrInputData[[iLoop]]$name)
    #}
    
    ###Create vector for graph
    arrXLim <- c()
    arrYLim <- c()
    
    ###Loop group to get sample and execute the FPKM processing
    for (groupName in arrInputData) {
      for (itemName in groupName$items) {
        arrXLim <- append(arrXLim, itemName$name)
        arrYLim <- append(arrYLim, oSimpleGraph$getFPKMCounter(itemName$sf, as.numeric(args[3])))
      }      
    }
         
    ###Create a title name
    yTitleName <- sprintf("FPKM > %d", as.numeric(args[3]))
    xTitleName <- "Sample"
    
    ###Create plot data
    plotData <- data.frame(
      Sample=factor(arrXLim),
      FPKM=arrYLim
    )   
    
    ###Create graph data
    imageRawData <- ggplot(data = plotData,aes(x = Sample, y = FPKM)) +
                          geom_bar(colour = 'black', stat='identity', width=.3) + 
                          labs(x = xTitleName, y = yTitleName) +
                          theme(legend.text = element_text(size = 20, colour = "red"))
        
    ###Save as PNG file cairo-png
    png(file="./fpkm.png", bg="transparent")
    print(imageRawData)
    dev.off()
  },
  ###Difference abundance in patient
  diff={    
    print('diff')    
  },
  ###Help table information
  {
    help()
  }
)

###Finish the script EasyRNA
print("Finish!!!")