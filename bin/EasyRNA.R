#!/usr/local/bin/Rscript
###Created on Jul 13, 2014
###Easy RNA script tools
allLibs <- c("rjson", "ggplot2", "RColorBrewer", "Cairo")
lapply(allLibs, library, character.only = TRUE)

###Get current directory, library path and templates path
global.document_root <- getwd()
global.library_path <- sprintf("%s%s", global.document_root,"/lib")
global.template_path <- sprintf("%s%s", global.document_root,"/template")
global.bin_path <- sprintf("%s%s", global.document_root,"/bin")

###Create list matrix from sample data
global.list_matrix <- list()

###Load all R script
global.sources <- list.files(c(global.library_path), pattern="*.R$", full.names=TRUE, ignore.case=TRUE)
print(global.sources)
sapply(global.sources, source, .GlobalEnv)

###Flush all console
flush.console()

###Get all ARGS data
args <- commandArgs(TRUE)

###Check arguments data
if(length(args) < 1) {  
  Utils.help()
}

###Get JSON data from file
jsonData <- fromJSON(file = toString(args[1]), method = "C", unexpected.escape = "error")

###Create output directory
dir.create(jsonData$outdir, showWarnings = FALSE, recursive = TRUE, mode = "0644")

###Get list action
arrAction <- as.vector(unlist(strsplit(jsonData$actions,",")),mode="list")

###Plot of  the number of RNAs that have FPKM > MAX_NUMBER
if (Utils.existParam(arrAction, "fpkm") == TRUE) {  
  ###Check FPKM args   
  if(Utils.isEmptyString(jsonData$fpkm_args) == TRUE)
  {
    print("Please input the FPKM min number")
    quit()
  }
  
  ###Create Simple Graph
  oSimpleGraph <- SimpleGraph(FALSE)
  
  ###Draw FPKM graph
  oSimpleGraph$drawFPKM(jsonData$inputs, jsonData$outdir, as.numeric(jsonData$fpkm_args))
}

###Plot HeatMap graph
if (Utils.existParam(arrAction, "heatmap") == TRUE) {
  ###Check FPKM args   
  if(Utils.isEmptyString(jsonData$heatmap_args) == TRUE)
  {
    print("Please input the gene list")
    quit()
  }
    
  ###Create Simple Graph
  oSimpleGraph <- SimpleGraph(FALSE)
  
  ###Draw FPKM graph
  oSimpleGraph$drawHeatMap(jsonData$inputs, jsonData$outdir, jsonData$heatmap_args)
} else {
  help()
}

###Finish the script EasyRNA
print("Finish!!!")
