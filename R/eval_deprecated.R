#'
#'
#' Evaluation
#'
#'

eval_DEPRECATED <- new.env(parent = .GlobalEnv)

with(eval_DEPRECATED, {

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
          senseinventoryname = sensevectors$.defaults$senseinventoryname,
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

  run <- function(inputfile = file.path(cache$data_dir(), 'sim', 'sim_bench_merged.tsv'), samples = c(1,2,3,4,5), outputfile = NULL, ccl = NULL){
    require(dplyr)
    benchmark <- data.table::fread(inputfile, sep='\t', header=T, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")

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

    # this function will be applied to every index in `ind`
    rowfun <- function(i) {
      row <- benchmark[i,]
      df <- lapply(names(.eval_configs()), function(cfgname) {
        cfg <- .eval_configs()[[cfgname]]
        cfg_df <- cfg$scorefun(row$word1, row$word2, row$pos1, row$pos2)
        cfg_df$config <- cfgname
        return(cbind(row, cfg_df))
      }) %>% bind_rows
      return(df)
    }

    # run eval
    tictoc::tic()
    if(is.null(ccl)) { # run locally / single threaded in current session
      init()
      scores <- lapply(ind, rowfun) %>% bind_rows
    } else { # run in parallel mode
      scores <- cclDef$lapply.par(
        X = ind,
        FUN = rowfun,
        ccl = ccl,
        exportitems = c('benchmark','rowfun','sensevectors', 'eval'),
        exportitemsenvir = environment(),
        initializationfun = function() { eval$init() }
      ) %>% bind_rows
    }
    tictoc::toc()

    return(scores)
  }
})
