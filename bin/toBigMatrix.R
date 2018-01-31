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

library(rio)
library(bigmemory)
library(tictoc)
options(bigmemory.allow.dimnames=TRUE)

# define filenames
vsfile <- args[1]
separator <- { if(length(args) > 1) args[2] else ' ' }
bckngpath <- dirname(vsfile)
bckngfile <- paste0(basename(vsfile), '.bin')
bckngdesc <- paste0(bckngfile, '.desc')
message(sprintf('[%s-%d-%s] Converting Vector Space Matrix: \n  input: \'%s\' \n  path:  \'%s\' \n  bin:   \'%s\'  \n  desc:  \'%s\' ', 
                gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), 
                vsfile, bckngpath, bckngfile, bckngdesc))

if(!file.exists(vsfile))
  stop(sprintf('[%s-%d-%s] input file does not exist. Aborting.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S")))

if(file.exists(file.path(bckngpath, bckngdesc)))
  stop(sprintf('[%s-%d-%s] descriptor file exists. Aborting.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S")))

# read matrix, convert to bigmatrix and store descriptor and binary backing file
tic('Elapsed')
tic('Finished loading.')
message(sprintf('[%s-%d-%s] Loading...', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S")))
df <- import(vsfile, sep=separator, header=F, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
colnames(df) <- NULL # remove colnames
tic('Fixed missing rowname values.')
missing_names <- which(is.na(df[,1]) | is.null(df[,1]) | df[,1] == '') # first column is rownames, find missing values
df[missing_names, 1] <- sapply(missing_names, function(ri) paste0('missing_row_',ri)) # fix missing values
toc()
rownames(df) <- df[,1] # first column is rownames
df <- df[,-1] # remove first column
toc()
message(sprintf('[%s-%d-%s] Data size: %s', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), format(object.size(df), units = "auto")))
message('Memory usage:')
gc(reset=T) # show some memory usage
tic('Finished converting.')
message(sprintf('[%s-%d-%s] Converting...', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S")))
bm <- as.big.matrix(df, backingfile = bckngfile, backingpath = bckngpath, descriptorfile = bckngdesc, shared = T)
toc()
rm(df)
message('Memory usage:')
gc(reset=T) # show some memory usage
toc()
