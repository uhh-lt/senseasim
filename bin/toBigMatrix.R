#!/usr/bin/env Rscript

##
#
# Rscript --vanilla toBigMatrix.R iris.tsv
#
##
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if(length(args) <= 0)
  stop("At least one argument must be supplied (input file).", call. = FALSE)

library(senseasim)

# define filenames
vsfile <- args[1]
separator <- { if(length(args) > 1) args[2] else ' ' }
vsm$build_bigmatrix_from_txt(filename = vsfile, separator = separator)
