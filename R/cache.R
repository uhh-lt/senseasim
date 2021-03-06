#'
#'
#' Prepare and retrieve cache data
#'
#'

cache <- new.env(parent = .GlobalEnv)

with(cache, {

  # Utilities ----
  data_dir <- function() normalizePath(Sys.getenv('DATA_HOME', unset = '~/.'))
  data_temp_dir <- function() normalizePath(Sys.getenv('DATA_TEMP', unset = tempdir()))

  #'
  #' Helper function, get the case representation U(ppercase) or l(owercase) for each character in a string
  #'
  get_case_representation <- function(term) paste(mapply(function(c) if(c == toupper(c)) 'U' else 'l', strsplit(toString(term),'')[[1]]), collapse = '')

  get_filename <- function(term, POS, dirname, prefix = '', postfix = '.rds')
    file.path(dirname, paste0(prefix, tolower(term), '_', get_case_representation(term), '$', tolower(POS), '_', get_case_representation(POS), postfix))

  load <- function(filename, computefun, save.null = F){
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

      result <- tryCatch(
        expr = {
          r <- computefun()
          if(is.null(r)) {
            if(save.null) {
              util$message(sprintf('Saving result to \'%s\'.', filename))
              saveRDS(r, file = filename)
            }
          } else if(is.logical(r) && is.na(r)){
            util$message('Result is NA, skip saving.')
          }
          else {
            util$message(sprintf('Saving result to \'%s\'.', filename))
            saveRDS(r, file = filename)
          }
          r
        },
        error = function(err) {
          util$message('Computefun threw error, skip saving result.')
          return(NA)
        }
      )

    } else {
      # load
      util$message(sprintf('loading \'%s\'.', filename))
      result <- readRDS(filename)
    }
    flock::unlock(lock__)
    return(result)
  }


}) # end with(...)

