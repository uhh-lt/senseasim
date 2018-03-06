#'
#'
#' Evaluation
#'
#'

eval <- new.env(parent = .GlobalEnv)

with(eval, {

  .eval_configs <- function() list(
    default = list(
      initfun = function() sensevectors$init(),
      scorefun = function(w1,w2,POS1,POS2) {
        score = senseasim$score(
          term1 = w1,
          term2 = w2,
          POS1 = POS1,
          POS2 = POS2,
          vsm_modelname = sensevectors$.defaults$vsm_model,
          jbt_sense_api = sensevectors$.defaults$jbt_sense_api,
          topn_sense_terms = sensevectors$.defaults$topn_sense_terms,
          shift_lambda = sensevectors$.defaults$shift_lambda,
          simfun = senseasim$cos
        )
        return(data.frame(score$maxscore, stringsAsFactors = F))
      }
    )
  )

  init <- function() {
    lapply(.eval_configs(), function(cfg) cfg$initfun())
  }

  run <- function(inputfile = file.path(cache$data_dir(), 'sim-multilang', 'sim_bench_merged.tsv'), outputfile, samples = c(1,2,3,4,5)){
    require(dplyr)
    init()
    benchmark <<- data.table::fread(inputfile, sep='\t', header=T, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")

    # prepare test example indexes
    if(length(samples) > 1 || samples > 1){
      message(sprintf('[%s-%d-%s]: using sample indices: \'%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), paste(samples, collapse = ',')))
      ind <<- samples
    }
    else{
      message(sprintf('[%s-%d-%s]: using %.2f %% sample size.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), samples * 100))
      ind <<- sample.int(nrow(benchmark), nrow(benchmark)*samples,F) # interpret samples as percentage value
      ind <<- sort(ind) # sort for better traceability
      message(sprintf('[%s-%d-%s]: using %d of %d sample indices: \'%s,...\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), length(ind), nrow(benchmark), paste(ind[1:min(length(ind), 10)], collapse = ',')))
    }

    # run eval
    scores <- lapply(ind, function(i) {
      row <- benchmark[i,]
      df <- lapply(.eval_configs(), function(cfg) {
        cbind(row, cfg$scorefun(row$word1, row$word2, row$pos1, row$pos2))
      }) %>% bind_rows
      return(df)
    }) %>% bind_rows
    return(scores)
  }

  # init_cluster <- function(cl, inputfile, outputfile) {
  #   words <<- data.table::fread(inputfile, sep=' ', header=F, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
  #   parallel::clusterExport(cl, c('words','sensevectors'), envir = .GlobalEnv)
  #   parallel::clusterExport(cl, c('outputfile'), envir = environment())
  #
  #   parallel::clusterEvalQ(cl, {
  #     # initialization actions
  #     local_outputfile <<- paste0(outputfile, Sys.getpid())
  #     sensevectors$init()
  #     message(sprintf('[%s-%d-%s] saving to \'%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), local_outputfile))
  #   })
  # }
  #
  # run_parallel <- function(inputfile, outputfile, cl = NULL) {
  #   # measure computing time
  #   tictoc::tic()
  #
  #   # if cluster is null create a cluster of n-1 cores of n beeing the system core number
  #   if(is.null(cl)) {
  #     cl <- cclDef$local(cores=parallel::detectCores()-1)
  #   }
  #   if(is.numeric(cl)){
  #     if(cl < 1){
  #       cl = parallel::detectCores()-1
  #     }
  #     cl <- cclDef$local(cores=cl)
  #   }
  #
  #   init_cluster(cl, inputfile, outputfile)
  #
  #   # apply in parallel
  #   r <- parallel::parLapply(cl, 1:nrow(words), function(i) {
  #     term <- words[i,1]
  #     POS <- words[i,2]
  #     get_and_write_sensevectors(term, POS, local_outputfile)
  #     return(T)
  #   })
  #
  #   # shutdown cluster
  #   message(sprintf('[%s-%d-%s] shutting down cluster.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S')))
  #   parallel::stopCluster(cl)
  #
  #   tictoc::toc()
  # }

})
