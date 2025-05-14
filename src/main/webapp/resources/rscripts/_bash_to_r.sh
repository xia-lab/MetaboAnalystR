# This script is called from Java directly without using Rserve
# This works in both Linux and Mac. The input is a Rscript file containing R commands for complex tasks
# note, the PATH contain the paths to those programs (R, bash, PDF)
#! /bin/bash 

PATH="/usr/texbin:/Library/TeX/texbin:/usr/bin:/usr/local/bin:${PATH}"
export PATH
Rscript $@