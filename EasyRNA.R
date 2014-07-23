#!/usr/local/bin/Rscript
###Created on Jul 13, 2014
###Easy RNA script tools
allLibs <- c("rjson", "ggplot2", "RColorBrewer", "Cairo")
lapply(allLibs, library, character.only = TRUE)

###Get current directory, library path and templates path
DOCUMENT_ROOT <- getwd()
LIBRARY_PATH <- sprintf("%s%s", DOCUMENT_ROOT,"/libs")
TEMPLATE_PATH <- sprintf("%s%s", DOCUMENT_ROOT,"/templates")

###Load all R script
file.sources <- list.files(c(LIBRARY_PATH), pattern="*.R$", full.names=TRUE, ignore.case=TRUE)
sapply(file.sources, source, .GlobalEnv)

###Flush all console
flush.console()

###Create help funciton
help <- function() {
  print("EasyRNA tools - Version 1.0")
  print("Author : Hoa.PT and Son.PK")
  print("Using  : Rscript EasyRNA.R <Json_File>") 
  quit()
}

###Check existing param in vector
existParam <- function(arrVector, paramName) {
  return(is.element(paramName, arrVector))
}

###Check empty string data
isEmptyString <- function(stringData) {
  return(is.null(stringData) || (stringData == "") || (length(stringData) == 0))
}

###Get all ARGS data
args <- commandArgs(TRUE)

###Check arguments data
if(length(args) < 1) {  
  help()
}

###Get JSON data from file
jsonData <- fromJSON(file = toString(args[1]), method = "C", unexpected.escape = "error")

###Create output directory
dir.create(jsonData$outdir, showWarnings = FALSE, recursive = TRUE, mode = "0644")

###Get list action
arrAction <- as.vector(unlist(strsplit(jsonData$actions,",")),mode="list")

###Plot of  the number of RNAs that have FPKM > MAX_NUMBER
if (existParam(arrAction, "fpkm") == TRUE) {  
  ###Check FPKM args   
  if(isEmptyString(jsonData$fpkm_args) == TRUE)
  {
    print("Please input the FPKM min number")
    quit()
  }
  
  ###Create Simple Graph
  oSimpleGraph <- SimpleGraph(FALSE)

  ###drawHeatmap Draw a heatmap picture
  ##INPUT: (1) a sorted list of gene names, (2) A list of groups that do
  #not have common elements
  #OUTPUT: a heatmap plot
  #EXAMPLE: http://gtbinf.files.wordpress.com/2013/11/figure1.png
  
  ###Draw FPKM graph
  oSimpleGraph$drawFPKM(jsonData$inputs, jsonData$outdir, as.numeric(jsonData$fpkm_args))
} else {
  help()
}

###Finish the script EasyRNA
print("Finish!!!")
