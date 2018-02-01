#'
#'
#' Prepare and retrieve cache data
#'
#'

# suppressPackageStartupMessages(library(jsonlite))

cache <- new.env(parent = .GlobalEnv)

with(cache, {

  # Utilities ----
  data_dir <- function() Sys.getenv('DATA_HOME', unset = dirname('~/.'))
  data_temp_dir <- function() Sys.getenv('DATA_TEMP', unset = dirname(tempdir()))

  #'
  #' Helper function, get the case representation u(ppercase) or l(owercase) for each character in a string
  #'
  get_case_representation <- function(term) paste(mapply(function(c) if(c == toupper(c)) 'u' else 'l', strsplit(term,'')[[1]]), collapse = '')

  get_filename <- function(term, POS, dirname, prefix = '', postfix = '.rds')
    file.path(dirname, paste0(prefix, tolower(term), '_', get_case_representation(term), '$', tolower(POS), '_', get_case_representation(POS), postfix))

  load <- function(filename, loadfun){
    if (file.exists(filename)) {
      # load
      message(sprintf('[%s-%d-%s] loading \'%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), filename))
      result <- readRDS(filename)
      return(result)
    }
    # else
    result <- loadfun()
    if(!is.null(result)){
      message(sprintf('[%s-%d-%s] saving result to \'%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), filename))
      saveRDS(result, file = filename)
    }
    return(result)
  }

}) # end with(...)

