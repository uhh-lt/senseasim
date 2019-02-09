#'
#'
#' get some default compute cluster definitions
#'
#'

cclDef <- new.env(parent = .GlobalEnv)

with(cclDef, {

  make.default <- function(cl = parallel::detectCores()-1){
    # create a cluster of n-1 cores, with n beeing the system core number
    if(is.numeric(cl)){
      if(cl == -1){
        cl <- parallel::detectCores()-1
      }else{
        cl <- max(1, cl) # handle values <= 0 (other than -1)
      }
      cl <- local(cores=cl)
    }
    # else assume cl is already a cluster object
    return(cl)
  }

  local <- function(cores=parallel::detectCores()-1, outfile='parallel-R.log') {
    util$message(sprintf('starting local cluster with %d cores.', cores))
    util$message(sprintf('saving log to \'%s\'.', outfile))
    cl <- parallel::makeCluster(cores, type='PSOCK', outfile=outfile)
    return(cl)
  }

  #'
  #'
  #' Apply parallelized function (simplified)
  #'
  #'
  lapply.par <- function(X, FUN, ccl = NULL, exportitems = c(), exportitemsenvir = .GlobalEnv, initializationfun = function(){}, finalizationfun = function(){}) {
    ccl <- make.default(ccl)
    # export variables and run initialization procedures
    parallel::clusterExport(ccl, c('X','FUN', 'initializationfun', 'finalizationfun'), envir=environment())
    parallel::clusterExport(ccl, exportitems, envir=exportitemsenvir)
    parallel::clusterEvalQ(ccl, { initializationfun() })
    result <- tryCatch(
      expr = {
        parallel::parLapply(ccl, X, FUN)
      },
      finally = {
        util$message('Shutting down cluster!')
        parallel::clusterEvalQ(ccl, { finalizationfun() })
        parallel::stopCluster(ccl)
      }
    )
    return(result)
  }


})

