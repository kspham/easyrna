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
    geneVector = c(),
    get = function(x) classTable[[x]],
    set = function(x, value) classTable[[x]] <<- value
  )
  
  ###Show class information
  classTable$showClassInfo = function(){
    print(classTable$infor)
  }
    
  ###Get counter number of gene list
  classTable$getFPKMFromGeneList <- function(cachingKey, quantFilePath, arrGenes){
    ###Get all frame data
    arrFrameData <- Utils.readDataFrame(quantFilePath, cachingKey)
    
    ###Create list FPKM data
    arrFPKM <- c()
    
    ###Get list FPKM data
    for (iLoop in 1:length(arrGenes)) {      
      rowData <- arrFrameData[arrGenes[[iLoop]],]
      arrFPKM <- append(arrFPKM, rowData$V3)
    }
    
    ###Return data
    return(arrFPKM)
  }
  
  ###Get counter number FPKM of gene list
  classTable$getGeneWithFPKMList <- function(cachingKey, quantFilePath){
    ###Get all frame data
    arrFrameData <- Utils.readDataFrame(quantFilePath, cachingKey)
    
    ###Set vector genes data    
    if(length(classTable$geneVector) == 0) {
      classTable$geneVector <- rownames(arrFrameData)
    }
                    
    ###Return data
    return(arrFrameData[,2])
  }
   
  ###Get counter number of RNAs that have FPKM > 5
  classTable$getFPKMCounter <- function(cachingKey, quantFilePath, iMaxNumber){
    ###Get all frame data
    arrFrameData <- Utils.readDataFrame(quantFilePath, cachingKey)
    
    ###Sum the max TMP
    iTotalLine <- sum(arrFrameData$V3 >= iMaxNumber)
    
    ###Return data
    return(iTotalLine)
  }
  
  ###Print data
  classTable$printData <- function(arrInputData) {    
    if(classTable$debug == TRUE) {      
      for (iLoop in 1:length(arrInputData)) {
        print(arrInputData[[iLoop]])
      }
    }
  }
  
  ###Process TSV for D3JS
  classTable$processFPKMD3JS <- function(outputFilePath, outputName, letterList, letterFrequency) {
    sCommand <- sprintf("python '%s/%s' -o '%s' -n %s -l %s -f %s", global.bin_path, "FPKM.py", outputFilePath, outputName, shQuote(letterList), shQuote(letterFrequency))
    system(sCommand, intern=TRUE)
  }
  
  ###Draw FPKM graph
  classTable$drawFPKM <- function(arrInputData, outputFilePath, iMaxNumber) {        
    ###Create vector for graph
    arrXLim <- c()
    arrYLim <- c()
    
    ###Loop comparing number to check FPKM
    for (iLoop in 1:length(arrInputData)) {
      ###Loop group in everyone comparing
      for (groupName in arrInputData[[iLoop]]) {
        ###Loop items in a group
        for (itemName in groupName$items) {
          cachingKey <- sprintf("%s_%s", Utils.removeSpaceInString(groupName$name), Utils.removeSpaceInString(itemName$name))
          iTotalFPKM <- classTable$getFPKMCounter(cachingKey, itemName$sf, iMaxNumber)          
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
    png(file=sprintf("%s/%s.%d.%s", outputFilePath, "fpkm", iMaxNumber, "png"), width = global.output_png_with, height = global.output_png_height, res = global.output_resolution, bg="transparent")
    print(imageRawData)
    dev.off()
    
    ###Save as TIFF file
    tiff(file=sprintf("%s/%s.%d.%s", outputFilePath, "fpkm", iMaxNumber, "tiff"), width = global.output_tiff_with, height = global.output_tiff_height, compression = global.output_tiff_compression, bg="transparent")
    print(imageRawData)
    dev.off()
    
    ###Save as SVG file
    svg(file=sprintf("%s/%s.%d.%s", outputFilePath, "fpkm", iMaxNumber, "svg"), width = global.output_svg_with, height = global.output_svg_height, bg="transparent")
    print(imageRawData)
    dev.off()
    
    ###Save as TSV for D3JS
    letterList <- paste(arrXLim, collapse=",")    
    letterFrequency <- paste(arrYLim, collapse=",")
    classTable$processFPKMD3JS(outputFilePath, sprintf("%s.%d", "fpkm", iMaxNumber), letterList, letterFrequency)
  }
  
  ###Draw HeatMap images  
  classTable$drawHeatMap <- function(arrInputData, outputFilePath, geneList) {
    ###Get gene list data
    arrGenes <- as.vector(unlist(strsplit(geneList,",")),mode="list")
                
    ###Loop comparing number to check FPKM
    for (iLoop in 1:length(arrInputData)) {
      ###Create vector for graph
      arrSampleRowName <- c()
      arrSampleColData <- list()
      arrImageName <- c()
            
      ###Loop group in everyone comparing
      for (groupName in arrInputData[[iLoop]]) {
        arrImageName <- append(arrImageName, Utils.removeSpaceInString(groupName$name))
        for (itemName in groupName$items) {
          cachingKey <- sprintf("%s_%s", Utils.removeSpaceInString(groupName$name), Utils.removeSpaceInString(itemName$name))
          arrSampleColData[[length(arrSampleColData)+1]] <- classTable$getFPKMFromGeneList(cachingKey, itemName$sf, arrGenes)
          arrSampleRowName <- append(arrSampleRowName, sprintf("%s (%s)", itemName$name, groupName$name))          
        }      
      }
      
      ###Create image name of two group
      imageName <- paste(arrImageName, collapse="_") 
      
      ###Get row and col number matrix graph data
      iTotalRow <- length(arrSampleRowName)
      iTotalCol <- length(arrGenes)
      
      ###Create matrix graph data
      graphData <- matrix(rnorm(iTotalRow * iTotalCol), ncol = iTotalCol, nrow=iTotalRow)
      rownames(graphData) <- arrSampleRowName
      colnames(graphData) <- arrGenes   
                        
      ###Replace with numbers data
      for(jLoop in 1:iTotalRow) {        
        for(kLoop in 1:iTotalCol){          
          graphData[jLoop, kLoop] <- arrSampleColData[jLoop][[1]][[kLoop]]
        }
      }
      
      ###Creates a colour palette
      scaleyellowred <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)
                 
      ###Save as PNG file cairo-png
      png(file=sprintf("%s/%s.%s", outputFilePath, imageName, "heatmap.png"), width = global.output_png_with, height = global.output_png_height, bg="transparent")
      heatmap(graphData, Rowv = NA, Colv = NA, col=scaleyellowred, margins=c(5,10))
      dev.off()
      
      ###Save as TIFF file cairo-png
      tiff(file=sprintf("%s/%s.%s", outputFilePath, imageName, "heatmap.tiff"), width = global.output_tiff_with, height = global.output_tiff_height, compression = global.output_tiff_compression, bg="transparent")
      heatmap(graphData, Rowv = NA, Colv = NA, col=scaleyellowred, margins=c(5,10))
      dev.off()
      
      ###Save as SVG file
      svg(file=sprintf("%s/%s.%s", outputFilePath, imageName, "heatmap.svg"), width = global.output_svg_with, height = global.output_svg_height, bg="transparent")
      heatmap(graphData, Rowv = NA, Colv = NA, col=scaleyellowred, margins=c(5,10))
      dev.off()
    }
  }
  
  ###Draw Distribution images  
  classTable$drawDistributionMap <- function(arrInputData, outputFilePath, iIgnoreNumber) {    
    ###Loop comparing number to check FPKM
    for (iLoop in 1:length(arrInputData)) {      
      ###Loop group in everyone comparing
      for (groupName in arrInputData[[iLoop]]) {        
        for (itemName in groupName$items) {
          ###Get all FPKM data
          cachingKey <- sprintf("%s_%s", Utils.removeSpaceInString(groupName$name), Utils.removeSpaceInString(itemName$name))
          arrSampleColData <- classTable$getGeneWithFPKMList(cachingKey, itemName$sf)
                
          ###Create label sequence data
          arrSampleRowLabel <- c()
          arrSampleRowValue <- c()
          arrSampleRowName <- c()
          
          ###Create step sequence data          
          arrStepData <- seq(from = min(arrSampleColData), to = (max(arrSampleColData) + 100), by = 100)
                    
          ###Loop to get data          
          for (iLoopStep in arrStepData) {
            iTotalElement <- sum((arrSampleColData >= iLoopStep) & (arrSampleColData >= iIgnoreNumber) & (arrSampleColData < (iLoopStep + 100)))
            if(iTotalElement > 0) {
              iLogTotalElement <- log(iTotalElement)
              arrSampleRowValue <- append(arrSampleRowValue, iLogTotalElement)
              arrSampleRowName <- append(arrSampleRowName, sprintf("[%d-%d]", iLoopStep, (iLoopStep + 100))) 
              arrSampleRowLabel <- append(arrSampleRowLabel, sprintf("log(%d) == %f", iTotalElement, iLogTotalElement))
            }            
          }
          
          ###Create plot data
          plotData <- data.frame(
            STEP=arrSampleRowName,
            FPKM=arrSampleRowValue,
            COUNTER=arrSampleRowLabel
          )   
                    
          ###Create graph data          
          imageRawData <- ggplot(data = plotData, aes(x = STEP, y = FPKM)) + 
                          geom_point(aes(colour = COUNTER), size = 6) +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) +
                          labs(x = sprintf("FPKM counter of %s", itemName$name), y = sprintf("Genes counter of %s", itemName$name))
          
          ###Save as PNG file cairo-png
          png(file=sprintf("%s/%s.%s.%d.%s", outputFilePath, cachingKey, "distribution", iIgnoreNumber, "png"), width = global.output_png_with, height = global.output_png_height, res = global.output_resolution, bg="transparent")
          print(imageRawData)
          dev.off()
          
          ###Save as TIFF file
          tiff(file=sprintf("%s/%s.%s.%d.%s", outputFilePath, cachingKey, "distribution", iIgnoreNumber, "tiff"), width = global.output_tiff_with, height = global.output_tiff_height, compression = global.output_tiff_compression, bg="transparent")
          print(imageRawData)
          dev.off()
          
          ###Save as SVG file
          svg(file=sprintf("%s/%s.%s.%d.%s", outputFilePath, cachingKey, "distribution", iIgnoreNumber, "svg"), width = global.output_svg_with, height = global.output_svg_height, bg="transparent")
          print(imageRawData)
          dev.off()
        }      
      }
    }
  }
  
  ###Draw Venn images  
  classTable$drawVennMap <- function(arrInputData, outputFilePath, iIgnoreNumber) {    
    ###Create venn sequence data    
    arrCategoryName <- c()
    arrListData <- list()
    
    ###Loop comparing number to check FPKM
    for (iLoop in 1:length(arrInputData)) {      
      ###Loop group in everyone comparing
      for (groupName in arrInputData[[iLoop]]) {
        groupTrimName <- Utils.removeSpaceInString(groupName$name)
        
        ###If existing this group before
        if(is.null(arrListData[[groupTrimName]]) == FALSE) {
          next
        }
        
        ###Create list sequence data
        arrTempData <- c()
        iCounter <- 0
        arrCategoryName <- append(arrCategoryName, groupName$name)
        arrListData[[groupTrimName]] <- c()
        
        ###Loop sample data to put in group
        for (itemName in groupName$items) {
          ###Get all FPKM data
          cachingKey <- sprintf("%s_%s", groupTrimName, Utils.removeSpaceInString(itemName$name))
          arrSampleColData <- classTable$getGeneWithFPKMList(cachingKey, itemName$sf)
                    
          ###Check length of category group name
          if(length(arrListData[[groupTrimName]]) == 0) {
            arrTempData <- c(arrTempData, arrSampleColData)
          }
          else {
            arrTempData <- arrTempData + arrSampleColData
          }
          
          ###Increase the counter
          iCounter <- iCounter + 1
        }    
        
        ###Mean data in group name
        for (tempValue in arrTempData) {
          iTotalMean <- (tempValue/3)
          if(iTotalMean >= iIgnoreNumber) {
            arrListData[[groupTrimName]] <- append(arrListData[[groupTrimName]], iTotalMean)
          }
        }        
      }
    }
    
    ###Create venn plot
    imageRawData <- venn.diagram(
      x = arrListData,
      category.names = arrCategoryName,
      fill = factor(arrCategoryName),
      filename = NULL,
      fontface = "bold",
      fontfamily = "sans",
      cat.fontface = "bold",
      euler.d = TRUE,
      margin = 0.05
    )
        
    ###Save as PNG file cairo-png
    png(file=sprintf("%s/%s.%d.%s", outputFilePath, "venn", iIgnoreNumber, "png"), width = global.output_png_with, height = global.output_png_height, res = global.output_resolution, bg="transparent")
    grid.draw(imageRawData)
    dev.off()
    
    ###Save as TIFF file
    tiff(file=sprintf("%s/%s.%d.%s", outputFilePath, "venn", iIgnoreNumber, "tiff"), width = global.output_tiff_with, height = global.output_tiff_height, compression = global.output_tiff_compression, bg="transparent")
    grid.draw(imageRawData)
    dev.off()
    
    ###Save as SVG file
    svg(file=sprintf("%s/%s.%d.%s", outputFilePath, "venn", iIgnoreNumber, "svg"), width = global.output_svg_with, height = global.output_svg_height, bg="transparent")
    grid.draw(imageRawData)
    dev.off()
  }
  
  ###Draw PCA images  
  classTable$drawPCAMap <- function(arrInputData, outputFilePath) {
    ###Create vector for graph
    arrSampleRowName <- c()
    arrSampleColData <- list()
    
    ###Loop comparing number to check FPKM
    for (iLoop in 1:length(arrInputData)) {      
      ###Loop group in everyone comparing
      for (groupName in arrInputData[[iLoop]]) {        
        for (itemName in groupName$items) {
          cachingKey <- sprintf("%s_%s", Utils.removeSpaceInString(groupName$name), Utils.removeSpaceInString(itemName$name))
          arrSampleColData[[length(arrSampleColData)+1]] <- classTable$getGeneWithFPKMList(cachingKey, itemName$sf)
          arrSampleRowName <- append(arrSampleRowName, itemName$name)          
        }      
      }
    }
    
    ###Get row and col number matrix graph data
    iTotalRow <- length(arrSampleRowName)
    iTotalCol <- length(classTable$geneVector)
    
    ###Create matrix graph data
    graphData <- matrix(rnorm(iTotalRow * iTotalCol), ncol = iTotalCol, nrow=iTotalRow)
    rownames(graphData) <- arrSampleRowName
    colnames(graphData) <- classTable$geneVector  
        
    ###Replace with numbers data
    for(jLoop in 1:iTotalRow) {        
      for(kLoop in 1:iTotalCol){          
        graphData[jLoop, kLoop] <- arrSampleColData[jLoop][[1]][[kLoop]]
      }
    }
        
    ###Off this code
    if(FALSE) {
      pca1 = prcomp(graphData, scale. = TRUE)
      print(pca1)
      head(pca1$rotation)
      head(pca1$x)
      scores = as.data.frame(pca1$x)
      
      ###Create graph data
      imageRawData <- ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
        geom_hline(yintercept = 0, colour = "gray65") +
        geom_vline(xintercept = 0, colour = "gray65") +
        geom_text(colour = "tomato", alpha = 0.8, size = 4) +
        ggtitle("PCA plot of RNA samples")
      
      ###Save as PNG file cairo-png
      png(file=sprintf("%s/%s", outputFilePath, "pca.png"), width = global.output_png_with, height = global.output_png_height, res = global.output_resolution, bg="transparent")
      print(imageRawData)
      dev.off()
      
      ###Save as SVG file
      svg(file=sprintf("%s/%s", outputFilePath, "pca.svg"), width = global.output_svg_with, height = global.output_svg_height, bg="transparent")
      print(imageRawData)
      dev.off()
    }
  }
  
  ###Initalize class
  classTable <- list2env(classTable)
  class(classTable) <- "SimpleGraph"
  return(classTable)
}