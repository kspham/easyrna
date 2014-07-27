#!/usr/local/bin/Rscript
###Created on Jul 13, 2014
###Utilities common

###Create help funciton
Utils.help <- function() {
  print("EasyRNA tools - Version 1.0")
  print("Author : Hoa.PT and Son.PK")
  print("Using  : Rscript EasyRNA.R <Json_File>") 
  quit()
}

###Check existing param in vector
Utils.existParam <- function(arrVector, paramName) {
  return(is.element(paramName, arrVector))
}

###Check empty string data
Utils.isEmptyString <- function(stringData) {
  return(is.null(stringData) || (stringData == "") || (length(stringData) == 0))
}

###Read sample data frame from file
Utils.readDataFrame <- function(quantFilePath, cachingKey) {    
  ###If is null or length = 0
  if(is.null(global.list_samples[[cachingKey]]) || (length(global.list_samples[[cachingKey]]) == 0)) {
    global.list_samples[[cachingKey]] <- read.table(quantFilePath, header = FALSE, row.names="V1", sep = "\t")      
  }
  
  ###Return data
  return(global.list_samples[[cachingKey]])
}

###Remove space in string
Utils.removeSpaceInString <- function(stringData) {
  return(gsub('([[:punct:]])|\\s+','_', stringData))
}