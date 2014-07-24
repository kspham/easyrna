#!/usr/bin/env python
"""
Created on July 22, 2014
EasyRNA function
"""
__author__ = 'Linuxpham <thaihoabo@gmail.com>'
__contact__ = 'thaihoabo@gmail.com'
__version__ = "1.0"
__date__ = '2014-07-22'
import sys, os
import optparse, subprocess

###Define the general information
DOCUMENT_ROOT = os.path.realpath(os.path.abspath("./"))
LIBRARY_PATH = os.path.realpath(DOCUMENT_ROOT + "/lib")
BIN_PATH = os.path.realpath(DOCUMENT_ROOT + "/bin")

###Parse command line options
parserInstance = optparse.OptionParser(usage="usage: python %prog [options]", version="EasyRNA 1.0")
parserInstance.add_option("-i", "--input", default="./example/data.json", help="Input JSON file")
parserInstance.parse_args()

###Add library system path
if LIBRARY_PATH not in sys.path:
    sys.path.insert(0, LIBRARY_PATH) 
    sys.path.insert(0, BIN_PATH)   

###Get all arguments data
INPUT_FILE = str(parserInstance.values.input)

###Escape shell
def shellEscape(s):
   return s.replace("(","\\(").replace(")","\\)")

###Main function handler
def main():
    ###Debug information
    print("Start the EasyRNA processing with JSON file : " + INPUT_FILE)
    
    ###Create command in R
    command = "Rscript '" + shellEscape(BIN_PATH) + "/EasyRNA.R' " + INPUT_FILE
    print(command)
    ###Get all output data
    outData, _ = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, close_fds=True).communicate()
    
    ###Get all response data    
    for lineData in outData.splitlines():                
        outStringData = str(lineData)                         
        print(outStringData)
    
    ###Debug information
    print("End the EasyRNA processing")

###Default start all handlers    
if __name__ == '__main__':
    sys.exit(main())