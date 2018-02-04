#'
#'
#' get some default compute cluster definitions
#'
#'

cclDef <- new.env(parent = .GlobalEnv)

with(cclDef, {

  local <- function(cores=detectCores() - 1, outfile='parallel-R.log') {
    message(sprintf('starting local cluster with %d cores.', cores))
    cl <- parallel::makeCluster(cores, type='FORK', outfile=outfile)
    return(cl)
  }

})

