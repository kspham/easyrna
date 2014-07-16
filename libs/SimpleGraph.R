#!/usr/local/bin/Rscript
###Created on Jul 13, 2014
###Simple graph class

SimpleGraph <- function(){
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
  
  ###Initalize class
  classTable <- list2env(classTable)
  class(classTable) <- "SimpleGraph"
  return(classTable)
}