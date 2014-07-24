#!/usr/bin/env python
"""
Created on July 22, 2014
FPKM function
"""
__author__ = 'Linuxpham <thaihoabo@gmail.com>'
__contact__ = 'thaihoabo@gmail.com'
__version__ = "1.0"
__date__ = '2014-07-22'
import sys, os
import optparse, shutil

###Define the general information
DOCUMENT_ROOT = os.path.realpath(os.path.abspath("./"))
LIBRARY_PATH = os.path.realpath(DOCUMENT_ROOT + "/libs")
TEMPLATE_PATH = os.path.realpath(DOCUMENT_ROOT + "/templates")

###Parse command line options
parserInstance = optparse.OptionParser(usage="usage: python %prog [options]", version="FPKM 1.0")
parserInstance.add_option("-o", "--output", default="./", help="Output directory")
parserInstance.add_option("-n", "--name", default="fpkm", help="Name of output file")
parserInstance.add_option("-l", "--letter", default="", help="Letter data")
parserInstance.add_option("-f", "--frequency", default="", help="Frequency data")
parserInstance.parse_args()

###Add library system path
if LIBRARY_PATH not in sys.path:
    sys.path.insert(0, LIBRARY_PATH)    

###Get all arguments data
OUTPUT_DIRECTORY = str(parserInstance.values.output)
OUTPUT_NAME = str(parserInstance.values.name)
LETTER_DATA = str(parserInstance.values.letter)
FREQUENCY_DATA = str(parserInstance.values.frequency)

###Escape shell
def shellEscape(s):
   return s.replace("(","\\(").replace(")","\\)")
   #return s.replace("(","\\(").replace(")","\\)").replace(" ","\\ ")   

###Main function handler
def main():
    ###Debug information
    print("Start the FPKM processing...")
    
    ###Get source and destination D3JS
    jsSource = shellEscape(TEMPLATE_PATH + "/d3.v3.min.js")
    jsDestination = shellEscape(OUTPUT_DIRECTORY + "/d3.v3.min.js")  
    templateFilePath = shellEscape(TEMPLATE_PATH + "/fpkm.html")
    htmlFilePath = shellEscape(OUTPUT_DIRECTORY + '/fpkm.html')
    tsvFilePath = shellEscape(OUTPUT_DIRECTORY + '/' + OUTPUT_NAME + ".tsv")
        
    ###Copy d3.v3.min.js to output directory
    try:
        shutil.copy2(jsSource, jsDestination)
    except shutil.Error as e:
        print('Error: %s' % e)
    except IOError as e:
        print('Error: %s' % e.strerror)   

    ###Open fpkm.html file
    oFile = open (templateFilePath, "r")

    ###Read HTML data
    fpkmHTML = oFile.read()
    fpkmHTML = fpkmHTML.replace("fpkm.tsv", OUTPUT_NAME + ".tsv")
    
    ###Close read file
    oFile.close()
    
    ###Write to output directory
    oWriteFile = open(htmlFilePath, 'w')
    oWriteFile.write(fpkmHTML)
    oWriteFile.close()
        
        
    ###Get list letter and frequency
    arrLetter = LETTER_DATA.split(",")
    arrFrequency = FREQUENCY_DATA.split(",")
    
    ###Write TSV file to output directory
    oWriteFile = open(tsvFilePath, 'w')
    oWriteFile.write("letter")
    oWriteFile.write("\t")
    oWriteFile.write("frequency")
    oWriteFile.write("\n")
        
    ###Loop to write file
    for iLoop, letterName in enumerate(arrLetter):
        oWriteFile.write(letterName)
        oWriteFile.write("\t")
        oWriteFile.write(arrFrequency[iLoop])
        oWriteFile.write("\n")
    
    ###Close read file
    oWriteFile.close()
    
    ###Debug information
    print("End the FPKM processing...")

###Default start all handlers    
if __name__ == '__main__':
    sys.exit(main())