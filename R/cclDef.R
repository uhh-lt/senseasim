#'
#'
#' get some default compute cluster definitions
#'
#'

cclDef <- new.env(parent = .GlobalEnv)

with(cclDef, {

  make.default <- function(cl = parallel::detectCores()-1){
    # create a cluster of n-1 cores of n beeing the system core number
    if(is.numeric(cl)){
      if(cl == -1){
        cl <- parallel::detectCores()-1
      }else{
        cl <- max(1, cl) # handle values <= 0 (other than -1)
      }
      cl <- cclDef$local(cores=cl)
    }
    # else assume cl is already a cluster object
    return(cl)
  }

  local <- function(cores=parallel::detectCores()-1, outfile='parallel-R.log') {
    message(sprintf('[%s-%d-%s] starting local cluster with %d cores.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), cores))
    message(sprintf('[%s-%d-%s] saving log to \'%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), outfile))
    cl <- parallel::makeCluster(cores, type='FORK', outfile=outfile)
    return(cl)
  }

  #'
  #'
  #' Apply parallelized function (simplified)
  #'
  #'
  lapply.par <- function(X, fun, ccl = NULL, exportitems = c(), exportitemsenvir = .GlobalEnv, initializationfun = function(){}, finalizationfun = function(){}) {
    ccl <- cclDef$make.default(ccl)
    # export variables and run initialization procedures
    parallel::clusterExport(ccl, c('X','fun', 'initializationfun', 'finalizationfun'), envir=environment())
    parallel::clusterExport(ccl, exportitems, envir=exportitemsenvir)
    parallel::clusterEvalQ(ccl, { initializationfun() })

    result <- parallel::parLapply(ccl, X, fun)

    parallel::clusterEvalQ(ccl, { finalizationfun() })
    parallel::stopCluster(ccl)

    return(result)
  }


})

