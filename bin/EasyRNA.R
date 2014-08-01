#!/usr/local/bin/Rscript
###Created on Jul 13, 2014
###Easy RNA script tools
allLibs <- c("rjson", "ggplot2", "RColorBrewer", "Cairo", "VennDiagram")
lapply(allLibs, library, character.only = TRUE, quietly = TRUE, verbose = FALSE)

###Get current directory, library path and templates path
global.document_root <- getwd()
global.library_path <- sprintf("%s%s", global.document_root,"/lib")
global.template_path <- sprintf("%s%s", global.document_root,"/template")
global.bin_path <- sprintf("%s%s", global.document_root,"/bin")

###Create list matrix from sample data
global.list_samples <- list()

###Load all R script
global.sources <- list.files(c(global.library_path), pattern="*.R$", full.names=TRUE, ignore.case=TRUE)
sapply(global.sources, source, .GlobalEnv, verbose = FALSE)

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

###Get output information of images
global.output_resolution <- as.numeric(jsonData$output_resolution)
global.output_png_with <- as.numeric(jsonData$output_png_with)
global.output_png_height <- as.numeric(jsonData$output_png_height)
global.output_svg_with <- as.numeric(jsonData$output_svg_with)
global.output_svg_height <- as.numeric(jsonData$output_svg_height)
global.output_tiff_with <- as.numeric(jsonData$output_tiff_with)
global.output_tiff_height <- as.numeric(jsonData$output_tiff_height)
global.output_tiff_compression <- jsonData$output_tiff_compression

###Create output directory
dir.create(jsonData$outdir, showWarnings = FALSE, recursive = TRUE, mode = "0644")

###Get list action
arrAction <- as.vector(unlist(strsplit(jsonData$actions,",")),mode="list")

###Try to execute the processing
tryCatch({
  ###Plot of  the number of RNAs that have FPKM > MAX_NUMBER
  if (Utils.existParam(arrAction, "fpkm") == TRUE) {  
    ###Check FPKM args   
    if(Utils.isEmptyString(jsonData$fpkm_args) == TRUE)
    {
      print("Please input the FPKM min number")
      quit()
    }
    
    ###Debug information
    message("Start fpkm processing")
    
    ###Create Simple Graph
    oSimpleGraph <- SimpleGraph(FALSE)
    
    ###Draw FPKM graph
    oSimpleGraph$drawFPKM(jsonData$inputs, jsonData$outdir, as.numeric(jsonData$fpkm_args))
    
    ###Debug information
    message("End fpkm processing")
  }
  
  ###Plot HeatMap graph
  if (Utils.existParam(arrAction, "heatmap") == TRUE) {
    ###Check FPKM args   
    if(Utils.isEmptyString(jsonData$heatmap_args) == TRUE)
    {
      print("Please input the gene list")
      quit()
    }
    
    ###Debug information
    message("Start heatmap processing")
    
    ###Create Simple Graph
    oSimpleGraph <- SimpleGraph(FALSE)
    
    ###Draw HeatMap graph
    oSimpleGraph$drawHeatMap(jsonData$inputs, jsonData$outdir, jsonData$heatmap_args)
    
    ###Debug information
    message("End heatmap processing")
  }
  
  ###Plot Distribution graph
  if (Utils.existParam(arrAction, "distribution") == TRUE) {
    ###Check FPKM args   
    if(Utils.isEmptyString(jsonData$distribution_args) == TRUE)
    {
      print("Please input the ignoring FPKM number")
      quit()
    }
    
    ###Debug information
    message("Start distribution processing")
    
    ###Create Simple Graph
    oSimpleGraph <- SimpleGraph(FALSE)
    
    ###Draw Distribution graph
    oSimpleGraph$drawDistributionMap(jsonData$inputs, jsonData$outdir, as.numeric(jsonData$distribution_args))
    
    ###Debug information
    message("End distribution processing")
  }
  
  ###Plot Venn graph
  if (Utils.existParam(arrAction, "venn") == TRUE) {
    ###Check FPKM args   
    if(Utils.isEmptyString(jsonData$venn_args) == TRUE)
    {
      print("Please input the ignoring FPKM number")
      quit()
    }
    
    ###Debug information
    message("Start venn processing")
    
    ###Create Simple Graph
    oSimpleGraph <- SimpleGraph(FALSE)
    
    ###Draw Distribution graph
    oSimpleGraph$drawVennMap(jsonData$inputs, jsonData$outdir, as.numeric(jsonData$distribution_args))
    
    ###Debug information
    message("End venn processing")
  }
  
  ###Plot PCA graph
  if (Utils.existParam(arrAction, "pca") == TRUE) {  
    ###Create Simple Graph
    oSimpleGraph <- SimpleGraph(FALSE)
    
    ###Debug information
    message("Start pca processing")
    
    ###Draw PCA graph
    oSimpleGraph$drawPCAMap(jsonData$inputs, jsonData$outdir)
    
    ###Debug information
    message("End pca processing")
  } else {
    help()
  }
  
  ###Finish the script EasyRNA
  message("Finish!!!")
}, warning = function(condition) {
  message("Warning message:")
  message(condition)  
  return(NA)
}, error = function(condition) {
  message("Error message:")
  message(condition)  
  return(NA)
}, finally={
  global.list_samples <- NULL
})