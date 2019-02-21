
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

  .scorefuns <-  { list(
    #
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
    }, valuenames = c('rand'), depends=list(vsm=F, inventory=F)),
    #
    standardcos = list(fun=function(vsmodelname, senseinventoryname, w1, w2, POS1, POS2){
      sim <- vsm$models[[vsmodelname]]()$sim(w1, w2)
      return(list(
        concise = sim,
        full    = sim
      ))
    }, valuenames = c('sim'), depends=list(vsm=T, inventory=F)),
    #
    sensasim_t5_l0.5 = list(fun = function(vsmodelname, senseinventoryname, w1, w2, POS1, POS2) {
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
    }, valuenames = c('max_sim', 'avgscore'), depends=list(vsm=T, inventory=T)),
    #
    sensasim_t500_l0 = list(fun = function(vsmodelname, senseinventoryname, w1, w2, POS1, POS2) {
      r_full <- senseasim$score(term1 = w1, POS1 = 'N', term2 = w2, POS2 = 'N', vsmodelname, senseinventoryname, topn_sense_terms = 500, shift_lambda = 0)
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
    }, valuenames = c('max_sim', 'avgscore'), depends=list(vsm=T, inventory=T))
  ) }

  .generate_evaluations_old <- function(all_matches=F) {
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
        tryCatch({matches<-grep('_jbtsense_', sinventories, value = T); if (all_matches) matches else matches[[1]];}, error = function(e) c())
      )
      # use only inventories where the model is based on the found vsmodelname
      vsmodelavailable <- vsm$.models_available()[[vsmodelname]]
      sinventories <- grep(paste0('_',vsmodelavailable$basename,'_'), sinventories, value = T)
      # add the jbtsim and vsmsim type inventory
      sinventoriesfiltered <- c(
        sinventoriesfiltered,
        tryCatch({matches<-grep('_jbtsim_', sinventories, value = T); if (all_matches) matches else matches[[1]]}, error = function(e) c()),
        tryCatch({matches<-grep('_vsmsim_', sinventories, value = T); if (all_matches) matches else matches[[1]]}, error = function(e) c())
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

  .generate_evaluations <- function(all_matches=T) {
    vsmodels_available <- vsm$.models_available()
    d <- .datasets()
    en <- lapply(seq_len(nrow(d)), function(i) {
      di <- d[i, ]
      eis <- lapply(names(.scorefuns), function(scorefunname) {
        if(!.scorefuns[[scorefunname]]$depends$vsm){
          # generate row
          e <- di
          e$scorefun <- scorefunname
          e$mdesc <- e$scorefun
          e$outfile <- paste0(di$filename, '-scored-', e$mdesc)
          return(e)
        } else {
          # get the vector space models for lang
          vsmodelnames <- vsm$.modelnames_for_lang(di$lang)
          eiv <- lapply(vsmodelnames, function(vsmodelname) {
            vsmodelbasename <- vsmodels_available[[vsmodelname]]$basename
            if(!.scorefuns[[scorefunname]]$depends$inventory){
              # generate row
              e <- di
              e$scorefun <- scorefunname
              e$vsmodel <- vsmodelname
              e$mdesc <- paste0(e$scorefun, '__', e$vsmodel)
              e$outfile <- paste0(e$filename, '-scored-', e$mdesc)
              return(e)
            } else {
              # get all sense inventories for lang
              sinventories <- inventory$.modelnames_for_lang(di$lang)
              # get the first element of each inventory type if it exists
              sinventoriesfiltered <- c(
                tryCatch({matches<-grep('_jbtsense_', sinventories, value = T); if (all_matches) matches else matches[[1]];}, error = function(e) c())
              )
              # use only inventories where the model is based on the found vsmodelname
              sinventories <- grep(paste0('_',vsmodelbasename,'_'), sinventories, value = T)
              # add the jbtsim and vsmsim type inventory
              sinventoriesfiltered <- c(
                sinventoriesfiltered,
                tryCatch({matches<-grep('_jbtsim_', sinventories, value = T); if (all_matches) matches else matches[[1]]}, error = function(e) c()),
                tryCatch({matches<-grep('_vsmsim_', sinventories, value = T); if (all_matches) matches else matches[[1]]}, error = function(e) c())
              )
              eii <- lapply(sinventoriesfiltered, function(sinventory) {
                # generate row
                e <- di
                e$scorefun <- scorefunname
                e$vsmodel <- vsmodelname
                e$inventory <- sinventory
                e$mdesc <- paste0(e$scorefun, '__', e$vsmodel, '__', e$inventory)
                e$outfile <- paste0(e$filename, '-scored-', e$mdesc)
                return(e)
              })
              eii <- data.table::rbindlist(eii, use.names=T, fill=T)
              return(eii)
            }
          })
          eiv <- data.table::rbindlist(eiv, use.names=T, fill=T)
          return(eiv)
        }
      })
      eis <- data.table::rbindlist(eis, use.names=T, fill=T)
      return(eis)
    })
    en <- data.table::rbindlist(en, use.names=T, fill=T)
    en$eid <- seq_len(nrow(en))
    return(en)
  }

  .evaluate.row <- function(evalrow, evali=1, evaln=1, par=NULL, catcherrors=T){
    evaluation_ <- evalrow[,-'loaddata', with=F]
    util$message(sprintf('Current eval: [%d/%d] \n%s', evali, evaln,  paste0('\t', colnames(evaluation_), ': ', evaluation_, collapse = '\n')))
    # load
    dataset <- evalrow$loaddata[[1]]()

    if(catcherrors)
      res <- tryCatch(
        expr = {
          .evaluate(dataset, evalrow$scorefun, evalrow$vsmodel, evalrow$inventory, evalrow$outfile, par)
        },
        error = function(err) {
          util$message(sprintf('Eval [%d/%d] failed:  \n%s', evali, evaln, err))
          return(NA)
        }
      )
    else
      res <- .evaluate(dataset, evalrow$scorefun, evalrow$vsmodel, evalrow$inventory, evalrow$outfile, par)

    if(!is.list(res))
      return(evalrow)

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

  .evaluate <- function(dataset, scorefunname, vsmodelname, senseinventoryname, outfile, par) {
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
          initializationfun = function(){
            library(senseasim)
            #Sys.setenv(http_proxy='http://172.16.2.30:8080')
            #Sys.setenv(https_proxy='https://172.16.2.30:8080')
          },
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
  .run.eval <- function(par = NULL, evaluations = NULL, evalfilter = NULL, only_best_inventory=F, catchevalerrors =T) {
    .init()

    if(is.null(evaluations))
      evaluations <- .generate_evaluations(!only_best_inventory)

    evalind <- seq_len(nrow(evaluations))
    if(!is.null(evalfilter))
      evalind <- Filter(function(i) evalfilter(evaluations[i,]), evalind)

    # for each evaluation score to be computed
    results <- lapply(seq_along(evalind), function(eval_i_ind) {
      eval_i <- evalind[[eval_i_ind]]
      evaluation <- evaluations[eval_i,]
      evaluation_result <- .evaluate.row(evaluation, eval_i_ind, length(evalind), par, catchevalerrors)
      return(evaluation_result)
    });

    results <- c(results, fill=T)
    results <- do.call(rbind, results)

    resultsfile <- paste0('results-', Sys.info()[['nodename']], '-', format(Sys.time(), "%Y%m%d-%H%M%S"))
    saveRDS(results, file = paste0(resultsfile, '.rds'))
    write.table(results[,-'loaddata', with=F], quote=F, sep ='\t', row.names=F, col.names=T, file=paste0(resultsfile, '.tsv'))

    return(results)
  }

  .try.run.eval <- function(par = NULL, evaluations=NULL, evalfilter = function(e) T, only_best_inventory=F) {
    tryCatch(
      expr = {
        r <- .run.eval(par, evaluations, evalfilter, only_best_inventory)
        util$sendmessage(subject = util$as.messagestring('Computation Finished! <EOM>'))
        return(r)
      },
      error = function(err) {
        util$sendmessage(subject = util$as.messagestring('Computation Failed!'), content = toString(err))
        return(err)
      }
    )
  }

  .try.run.fteval <- function(par = NULL, evalfilter = function(e) T, only_best_inventory=F){
    ftfilter <- function(e) (is.na(e$vsmodel) || grep('_ft_cc_', e$vsmodel)) && evalfilter(e)
    .try.run.eval(par, NULL, ftfilter, only_best_inventory)
  }

  .results.get <- function(results){
    #
    if(is.data.frame(results)){
      resultsdt <- results
    } else {
      if(is.character(results) && file.exists(results)) {
        if(grepl('[.]tsv$', results)) {
          resultsdt <- data.table::fread(file = results, quote = '', sep = '\t', header = T, data.table = T) # write.table(results[,-'loaddata', with=F], quote=F, sep ='\t', row.names=F, col.names=T, file=paste0(resultsfile, '.tsv'))
        } else {
          resultsdt <- readRDS(results)
        }
      } else {
        stop('Unknown result format.')
      }
    }
    if(!'mdesc' %in% colnames(resultsdt)){ # estimate mdesc from output filename and move the column right next to it
      resultsdt$mdesc <- gsub('.*-scored-', '', resultsdt$outfile)
      outfilecolidx <- which(colnames(resultsdt) == 'outfile')
      neworder <- c(colnames(resultsdt)[1:(outfilecolidx-1)], 'mdesc', colnames(resultsdt)[outfilecolidx:(ncol(resultsdt)-1)])
      data.table::setcolorder(resultsdt, neworder)
    }
    return(resultsdt)
  }

  # .merge.results <- function(results) {
  #   resultsdt <- .get.results(results)
  #   evalidcolidx <- which(colnames(resultsdt) == 'evalid')
  #   scorecolumns <- colnames(resultsdt)[(evalidcolidx+1):ncol(resultsdt)]
  #   scorecolumns <- scorecolumns[which(scorecolumns != 'numsenses_avg')]
  #   merged <- resultsdt[,1:evalidcolidx]
  #   merged <- unique(merged, by='eid')
  #   for(scorecolumn in scorecolumns){
  #     subdata <- resultsdt[, c('eid', scorecolumn), with=F]
  #     subdata <- subdata[!is.na(get(scorecolumn)), ] # remove NAs
  #     merged <- merge(merged, subdata, by='eid')
  #   }
  #   merged <- merged[,-c('scorefun'), with=F]
  #   return(merged)
  # }

  .results.reformat <- function(results) {
    resultsdt <- .results.get(results)
    eidcolidx <- which(colnames(resultsdt) == 'eid')
    typecolidx <- which(colnames(resultsdt) == 'type')
    b<-(eidcolidx+1); e<-ncol(resultsdt)

    datasetids <- unique(resultsdt$datasetid)


    # for each dataset get the sub dt and squeeze all results into only one column
    newrows <- lapply(datasetids, function(did) {

      rdtdid <- resultsdt[datasetid == did]

      # get the base information for the dataset
      newdtrow <- rdtdid[1,1:typecolidx]


      # get the indices where the value is not NA
      notna <- which(!is.na(rdtdid[, b:e]),arr.ind = T)
      # for each of those indices
      for(i in seq_len(nrow(notna))){
        # get the rowname / the method descriptor
        mname <- rdtdid[notna[[i,1]],]$mdesc
        # get the colname / the value name
        vname <- colnames(rdtdid)[[(eidcolidx+notna[[i,2]])]]
        val <- rdtdid[notna[[i,1]], get(vname)]
        # remove the scorefunname
        vname <- gsub('^.*__', '', vname)
        scorename <- paste(mname, '__', vname)
        col <- list(); col[[scorename]] <- val
        # add a new column to newdtrow with scorename and value
        newdtrow <- cbind(newdtrow, data.frame(col))
      }
      return(newdtrow)
    })

    newresultdt <- data.table::rbindlist(newrows,  use.names=T, fill=T)

  }


  .interpret.results <- function(results){
    resultsdt <- .get.results(results)
    mergedresults <- .merge.results(resultsdt)
    evalidcolidx <- which(colnames(mergedresults) == 'evalid')
    scorecolumns <- colnames(mergedresults)[(evalidcolidx+1):ncol(mergedresults)]
    scorecolumns <- scorecolumns[which(scorecolumns != 'numsenses_avg')]
    interp <- lapply(scorecolumns, function(scorecolumn) {
      avg <- mean(unlist(mergedresults[, scorecolumn, with=F]))
      stddev <- sd(unlist(mergedresults[, scorecolumn, with=F]))
      data.table::data.table(
        mean = avg,
        stddev = stddev
      )
    })
    interp <- do.call(rbind, interp)
    interp$method <- scorecolumns
    return(interp)
  }

})

# e <- evaluate$.generate_evaluations(all_matches = T)
# ef <- e[(lang == 'en' | lang == 'de') & type=='mc' & (is.na(vsmodel) | grepl('_ft_cc_', vsmodel) | grepl('_ft_simplewiki_', vsmodel)),]
# evaluate$.run.eval(par=1, ef)



