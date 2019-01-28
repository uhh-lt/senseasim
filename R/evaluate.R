
evaluate <- new.env(parent = .GlobalEnv)

with(evaluate, {

  .init <- function() {
    senseasim$.init()
  }

  .load_dataset <- function(filename) data.table::fread(filename, sep=';', header=T, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=T, quote="")

  #'
  #' read SemR-11 dataset
  #'
  #' NOTE: remove directional formating characters from arabic datasets (left-to-right) (bytes U+202A U+202C):
  #'    cat ar-ws353.dataset | sed "s/$(echo -ne '\xe2\x80\xaa')//g" |  sed "s/$(echo -ne '\xe2\x80\xac')//g" > ar-ws353.dataset_
  #'
  .datasets <- function(path = file.path(cache$data_dir(), 'sim-multilang','SemR-11')) {
    dt <- data.table::data.table(filename = list.files(path, pattern = '^.+[.]dataset$', full.names = T))
    dt[,datasetid:=.I]
    dt$basename <- basename(dt$filename)
    dt$lang <- gsub('^([^-]+)-([^.]+).dataset$', '\\1', dt$basename)
    dt$type <- gsub('^([^-]+)-([^.]+).dataset$', '\\2', dt$basename)
    dt$loaddata <- lapply(dt$filename, function(f) function() .load_dataset(f))
    return(dt)
  }

  .scorefuns <- list(
    rand = list(fun = function(vsmodelname, senseinventoryname, w1, w2, POS1, POS2) {
      rand <- runif(1)
      r_full <- list(
        rand = rand,
        t1.is.unk =  F,
        t2.is.unk =  F,
        t1 = w1,
        t2 = w2
      )
      r_concise <- list(
        rand = rand,
        t1.is.unk = r_full$t1.is.unk,
        t2.is.unk = r_full$t2.is.unk
      )
      return(list(
        concise = r_concise,
        full    = r_full
      ))
    }, valuenames = c('rand')),
    standardcos = list(fun=function(vsmodelname, senseinventoryname, w1, w2, POS1, POS2){
      sim <- vsm$models[[vsmodelname]]()$sim(w1, w2)
      return(list(
        concise = sim,
        full    = sim
      ))
    }, valuenames = c('sim')),
    sensasim = list(fun = function(vsmodelname, senseinventoryname, w1, w2, POS1, POS2) {
      r_full <- senseasim$score(term1 = w1, POS1 = 'N', term2 = w2, POS2 = 'N', vsmodelname, senseinventoryname, topn_sense_terms = 5, shift_lambda = 0.5)
      r_concise <- r_full$maxscore
      r_concise$t1.nsenses <- r_full$t1_info$nsenses
      r_concise$t1.is.unk <- r_full$t1_info$index[1,]$unknown
      r_concise$t2.nsenses <- r_full$t2_info$nsenses
      r_concise$t2.is.unk <- r_full$t2_info$index[1,]$unknown
      r_concise$avgscore <- r_full$avgscore
      return(list(
        concise = r_concise,
        full    = r_full
      ))
    }, valuenames = c('max_sim', 'avgscore'))
  )

  .generate_evaluations <- function() {
    d <- .datasets()
    dn <- lapply(seq_len(nrow(d)), function(i){
      di <- d[i, ]
      # get the best vector space model name for lang
      vsmodelname <- vsm$.get_best_modelname_for_lang(di$lang)
      if(is.null(vsmodelname))
        return(di)
      # get all sense inventories for lang
      sinventories <- inventory$.modelnames_for_lang(di$lang)
      # get the first element of each inventory type if it exists
      sinventoriesfiltered <- c(
        tryCatch(grep('_jbtsense_', sinventories, value = T)[[1]], error = function(e) c())
      )
      # use only inventories where the model is based on the found vsmodelname
      vsmodelavailable <- vsm$.models_available()[[vsmodelname]]
      sinventories <- grep(paste0('_',vsmodelavailable$basename,'_'), sinventories, value = T)
      # add the jbtsim and vsmsim type inventory
      sinventoriesfiltered <- c(
        sinventoriesfiltered,
        tryCatch(grep('_jbtsim_', sinventories, value = T)[[1]], error = function(e) c()),
        tryCatch(grep('_vsmsim_', sinventories, value = T)[[1]], error = function(e) c())
      )
      # add all senseinventory related information to the data table
      di$vsmodelname <- vsmodelname
      di <- di[rep(1, times=length(sinventoriesfiltered))]
      di$senseinventory <- sinventoriesfiltered
      return(di)
    })
    dn <- c(dn, fill=T)
    dn <- do.call(rbind, dn)
    dn[,eid:=.I]
    # copy each row and add the scorefun
    scorefunsrep <- rep(names(.scorefuns), times=nrow(dn))
    dn <- dn[rep(seq_len(nrow(dn)), each=length(.scorefuns))]
    dn$scorefun <- scorefunsrep
    dn[,evalid:=.I]
    return(dn)
  }

  .evaluate.row <- function(evalrow, evali=1, evaln=1, par=NULL){
    evaluation_ <- evalrow[,-'loaddata', with=F]
    util$message(sprintf('Current eval: [%d/%d] \n%s', evali, evaln,  paste0('\t', colnames(evaluation_), ': ', evaluation_, collapse = '\n')))
    # load
    dataset <- evalrow$loaddata[[1]]()

    # for each evalconfig
    outfile <- paste0(evalrow$filename, '-scored-', evalrow$vsmodelname, '__', evalrow$senseinventory, '__', evalrow$scorefun)
    res <- .evaluate(dataset, evalrow$vsmodelname, evalrow$senseinventory, evalrow$scorefun, par, outfile)
    # result is a list of datapoint scores and correlation scores

    # combine result with evaluation row
    if(!is.data.frame(res$correlations))
      res$correlations <- data.frame(res$correlations, stringsAsFactors=F)
    evaluation_result <- cbind(evalrow, res$correlations)

    if('t1.nsenses' %in% colnames(res$scores) && 't2.nsenses' %in% colnames(res$scores))
      evaluation_result$numsenses_avg <- mean(c(res$scores$t1.nsenses, res$scores$t2.nsenses))
    else
      evaluation_result$numsenses_avg <- 1
    return(evaluation_result)
  }

  .evaluate <- function(dataset, vsmodelname, senseinventoryname, scorefunname = names(.scorefuns)[[1]], par, outfile) {
    if(file.exists(paste0(outfile, '.tsv'))){
      scoredt <- data.table::fread(paste0(outfile, '.tsv'), sep='\t', header=T, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=T, quote="")
    }
    else{
      scorefun <- .scorefuns[[scorefunname]]$fun
      rowfun <- function(i) {
        row <- dataset[i,]
        util$message(sprintf('Processing row %s/%s: <%s,%s> with (%s, %s, %s)', i, nrow(dataset), row$word1, row$word2, vsmodelname, senseinventoryname, scorefunname))
        result <- scorefun(vsmodelname, senseinventoryname, row$word1, row$word2, if(!is.null(row$pos1)) row$pos1 else 'N', if(!is.null(row$pos2)) row$pos2 else 'N')
        # combine row with the concise result
        if(!is.data.frame(result$concise))
          result$concise <- data.frame(result$concise, stringsAsFactors=F)
        row <- cbind(row, result$concise)
        # return combined row and the full result (for information purposes)
        return(list(
          dtrow = row,
          full = result$full
        ))
      }

      # run the evaluation (either locally or in paralell)
      ind <- seq_len(nrow(dataset))
      if(is.null(par) || par <= 1)
        scores <- lapply(X = ind, FUN = rowfun)
      else {
        scores <- cclDef$lapply.par(
          ccl = par,
          exportitems = ls(envir = environment()),
          exportitemsenvir = environment(),
          initializationfun = function(){ library(senseasim); senseasim$.init(); },
          X = ind,
          FUN = rowfun
        )
      }
      # combine concise results
      scoredt <- c(lapply(lapply(scores, '[[', 'dtrow'), data.table::data.table, fill=T))
      scoredt <- do.call(rbind, scoredt)
      # save results
      # save full results as rds
      saveRDS(scores, file = paste0(outfile,'.rds')) # from each sublist select the 'full' entry
      # save concise results as data.table in tsv format
      write.table(scoredt, quote=F, sep ='\t', row.names=F, col.names=T, file=paste0(outfile, '.tsv'))
    }

    # compute correlationscores for every valuename
    correlations <- lapply(.scorefuns[[scorefunname]]$valuenames, function(valuename) {
      .correlation(scoredt, xname='score', yname=valuename)
    })
    names(correlations) <- paste0(scorefunname, '__', .scorefuns[[scorefunname]]$valuenames)

    # return the computed scores
    return(list(
      scores = scoredt,
      correlations = correlations
    ))
  }

  .correlation <- function(dt, xname, yname, remove.unk = T){
    x <- dt[, get(xname)]
    y <- dt[, get(yname)]
    if(remove.unk && 't1.is.unk' %in% colnames(dt) && 't2.is.unk' %in% colnames(dt)){
      x <- dt[!(t1.is.unk | t2.is.unk), get(xname)]
      y <- dt[!(t1.is.unk | t2.is.unk), get(yname)]
    }
    cscore <- cor(x=x, y=y, use='pairwise.complete.obs', method='spearman')
    return(cscore)
  }

  #' @param dsetfilterfun NULL or filterfunction
  #' @test e.g. eval only de datasets with runeval(par = NULL, evalfilter = function(dset) dset$lang == 'en'))
  .run.eval <- function(par = NULL, evalfilter = function(e) T) {
    .init()
    evaluations <- .generate_evaluations()
    evalind <- seq_len(nrow(evaluations))
    if(!is.null(evalfilter))
      evalind <- Filter(function(i) evalfilter(evaluations[i,]), evalind)

    # for each evaluation score to be computed
    results <- lapply(seq_along(evalind), function(eval_i_ind) {
      eval_i <- evalind[[eval_i_ind]]
      evaluation <- evaluations[eval_i,]
      evaluation_result <- .evaluate.row(evaluation, eval_i_ind, length(evalind), par)
      return(evaluation_result)
    });

    results <- c(results, fill=T)
    results <- do.call(rbind, results)

    resultsfile <- paste0('results-', Sys.info()[['nodename']], '-', format(Sys.time(), "%Y%m%d-%H%M%S"))
    saveRDS(results, file = paste0(resultsfile, '.rds'))
    write.table(results[,-'loaddata', with=F], quote=F, sep ='\t', row.names=F, col.names=T, file=paste0(resultsfile, '.tsv'))


    return(results)
  }

  .try.run.eval <- function(par = NULL, evalfilter = function(e) T) {
    tryCatch(
      expr = {
        r <- .run.eval(par, evalfilter)
        util$sendmessage(subject = util$as.messagestring('Computation Finished! <EOM>'))
        return(r)
      },
      error = function(err) {
        util$sendmessage(subject = util$as.messagestring('Computation Failed!'), content = toString(err))
        return(err)
      }
    )
  }


  .merge.results <- function(results) {
    #
    if(!is.data.frame(results))
      if(is.character(results) && file.exists(results))
        if(grepl('[.]rds$', results))
          results <- readRDS(results)
        else if(grepl('[.]tsv$', results))
          results <- data.table::fread(file = results, quote = '', sep = '\t', header = T) # write.table(results[,-'loaddata', with=F], quote=F, sep ='\t', row.names=F, col.names=T, file=paste0(resultsfile, '.tsv'))
        else
          stop('Unknown suffix.')
      else
        stop('Unknown result format.')
    #
    evalidcolidx <- which(colnames(results) == 'evalid')
    scorecolumns <- colnames(results)[(evalidcolidx+1):ncol(results)]
    merged <- results[,1:evalidcolidx]
    merged <- unique(merged, by = 'eid')
    for(scorecolumn in scorecolumns){
      subdata <- results[, list(get('eid'), get(scorecolumn))]
      colnames(subdata) <- c('eid', scorecolumn)
      subdata <- subdata[!is.na(get(scorecolumn)), ] # remove NAs
      merged <- merge(merged, subdata, by='eid')
    }
   return(merged)
  }

  .interpret.results <- function(mergedresults){
    evalidcolidx <- which(colnames(mergedresults) == 'evalid')
    scorecolumns <- colnames(mergedresults)[(evalidcolidx+1):ncol(mergedresults)]
    interp <- lapply(scorecolumns, function(scorecolumn) {
      avg <- mean(mergedresults[,get(scorecolumn)])
      stddev <- sd(mergedresults[,get(scorecolumn)])
      data.table::data.table(
        mean = avg,
        stddev = stddev
      )
    })
    interp <- do.call(rbind, interp)
    interp$method <- scorecolumns
  }

})


# library(senseasim)
# senseasim$.init()
# d <- evaluate$.generate_sensevector_variants()
# d <- d[!is.na(senseinventory),]
# evaluate$.evaluate(d$load[[1]](), d$vsmodelname[[1]], d$senseinventory, par=2)

