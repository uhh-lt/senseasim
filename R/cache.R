#'
#'
#' Prepare and retrieve cache data
#'
#'

cache <- new.env(parent = .GlobalEnv)

with(cache, {

  # Utilities ----
  data_dir <- function() Sys.getenv('DATA_HOME', unset = dirname('~/.'))
  data_temp_dir <- function() Sys.getenv('DATA_TEMP', unset = dirname(tempdir()))

  #'
  #' Helper function, get the case representation U(ppercase) or l(owercase) for each character in a string
  #'
  get_case_representation <- function(term) paste(mapply(function(c) if(c == toupper(c)) 'U' else 'l', strsplit(toString(term),'')[[1]]), collapse = '')

  get_filename <- function(term, POS, dirname, prefix = '', postfix = '.rds')
    file.path(dirname, paste0(prefix, tolower(term), '_', get_case_representation(term), '$', tolower(POS), '_', get_case_representation(POS), postfix))

  load <- function(filename, computefun, save.null = T){
    if (!file.exists(filename)) {
      # compute and save
      result <- .load.locked(filename, computefun, save.null)
    } else {
      # load
      util$message(sprintf('loading \'%s\'.', filename))
      result <- readRDS(filename)
    }
    return(result)
  }

  .load.locked <-  function(filename, computefun, save.null = T) {
    # compute the result and save it. This must not be done in parallel!
    lock__ = flock::lock(paste0(filename,'.lock'));
    if(!file.exists(filename)){
      result <- computefun()
      if(is.null(result)) {
        if(save.null) {
          util$message(sprintf('Saving result to \'%s\'.', filename))
          saveRDS(result, file = filename)
        }
      }else{
        util$message(sprintf('Saving result to \'%s\'.', filename))
        saveRDS(result, file = filename)
      }
    } else {
      # load
      util$message(sprintf('loading \'%s\'.', filename))
      result <- readRDS(filename)
    }
    flock::unlock(lock__)
    return(result)
  }


}) # end with(...)

