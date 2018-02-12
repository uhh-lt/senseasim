#'
#'
#' get some default compute cluster definitions
#'
#'

cclDef <- new.env(parent = .GlobalEnv)

with(cclDef, {

  local <- function(cores=detectCores() - 1, outfile='parallel-R.log') {
    message(sprintf('[%s-%d-%s] starting local cluster with %d cores.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), cores))
    message(sprintf('[%s-%d-%s] saving log to \'%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), outfile))
    cl <- parallel::makeCluster(cores, type='FORK', outfile=outfile)
    return(cl)
  }

})

