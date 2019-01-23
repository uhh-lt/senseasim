
evaluate <- new.env(parent = .GlobalEnv)

with(evaluate, {

  .init <- function() {
    senseasim$.init()
    .datasets()
  }

  #'
  #' read SemR-11 dataset
  #'
  #' NOTE: remove directional formating characters from arabic datasets (left-to-right) (bytes U+202A U+202C):
  #'    cat ar-ws353.dataset | sed "s/$(echo -ne '\xe2\x80\xaa')//g" |  sed "s/$(echo -ne '\xe2\x80\xac')//g" > ar-ws353.dataset_
  #'
  .datasets <- function(path = file.path(cache$data_dir(), 'sim-multilang','SemR-11')) {
    dt <- data.table::data.table(filename = list.files(path, pattern = '^.+[.]dataset$', full.names = T))
    dt[,id:=.I]
    dt$basename <- basename(dt$file)
    dt$lang <- gsub('^([^-]+)-([^.]+).dataset$', '\\1', dt$basename)
    dt$type <- gsub('^([^-]+)-([^.]+).dataset$', '\\2', dt$basename)
    dt$load <- lapply(dt$filename, function(f) function() .load_dataset(f))
    return(dt)
  }

  .load_dataset <- function(filename){
    data.table::fread(filename, sep=';', header=T, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
  }

  .generate_sensevector_variants <- function() {
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
      # add all information to the data table
      di$vsmodelname <- vsmodelname
      di <- di[rep(1, times=length(sinventoriesfiltered))]
      di$senseinventory <- sinventoriesfiltered
      return(di)
    })
    dn <- c(dn, fill=T)
    dn <- do.call(rbind, dn)
    return(dn)
  }

  .scorefuns <- list(
    rand = function(vsmodelname, senseinventoryname, w1, w2) {
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
    },
    standardcos = function(vsmodelname, senseinventoryname, w1, w2){
      sim <- vsm$models[[vsmodelname]]()$sim(w1, w2)
      return(list(
        concise = sim,
        full    = sim
      ))
    },
    sensasim = function(vsmodelname, senseinventoryname, w1, w2){
      senseasim$score(term1 = w1, POS1 = 'N', term2 = w2, POS2 = 'N', vsmodelname, senseinventoryname, topn_sense_terms = 5, shift_lambda = 0.5)
      sim <- vsm$models[[vsmodelname]]()$sim(w1, w2)
      return(list(
        concise = sim,
        full    = sim
      ))
    }

  )

  .evaluate <- function(dataset, vsmodelname, senseinventoryname, scorefunname = names(.scorefuns)[[1]], par=NULL, outfile=NULL) {
    scorefun <- .scorefuns[[scorefunname]]
    rowfun <- function(i) {
      row <- dataset[i,]
      util$message(sprintf('Processing row %s/%s: <%s,%s> with (%s, %s, %s)', i, nrow(dataset), row$word1, row$word2, vsmodelname, senseinventoryname, scorefunname))
      result <- scorefun(vsmodelname, senseinventoryname, row$word1, row$word2)
      # combine row with the concise result which is a dataframe row
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
        exportitems = c('dataset', 'vsmodelname', 'senseinventoryname', 'rowfun'),
        exportitemsenvir = environment(),
        initializationfun = function(){ library(senseasim); senseasim$.init(); },
        X = ind,
        FUN = rowfun
      )
    }
    # save full results as rds
    #saveRDS(lapply(x, '[[', 'full')) # from each sublist select the 'full' entry


    # save concise results as data.table in tsv format
    #do.call(rbind, lapply(scores, '[[', 'dtrow'))


    return(scores)
  }

  #' @param dsetfilterfun NULL or filterfunction, e.g. function(dset) dset$filename == 'ar-ws353.dataset'
  #' @test e.g. debug with eval(par = NULL, dsetfilterfun = function(dset) dset$filename == 'ar-ws353.dataset')
  #' @test e.g. eval only de datasets with ft$eval(par = NULL, dsetfilterfun = function(dset) grepl('[.]de[.]', dset$filename))
  eval <- function(par = NULL, dsetfilterfun = function(dset) !is.na(dset$modelname), run = list('rand')) {

    if(is.null(par) || par <= 1) # run locally
      .init()

    suppressPackageStartupMessages(require(dplyr))
    datasets <- .datasets()
    dsetind <- seq_len(nrow(datasets))
    if(!is.null(dsetfilterfun)){
      dsetind <- Filter(function(i) dsetfilterfun(datasets[i,]), dsetind)
    }

    # for each dataset
    lapply(dsetind, function(i_d) {
      dataset <- datasets[i_d,]
      util$message(sprintf('Current dataset: [%d]\n%s', i_d, paste0('\t', colnames(dataset), ': ', dataset, collapse = '\n')))
      # load
      benchmark <- dataset$load[[1]]()
      modelname <- dataset$modelname
      models <- .models()

      # for each evalconfig
      lapply(names(.eval_configs()), function(evalconfigname) {
        util$message(sprintf("Current config: '%s'.", evalconfigname))
        # skip if not in the list of evals to run
        if(!(evalconfigname %in% run)) {
          util$message(sprintf("Skipping config '%s'.", evalconfigname))
          return(F)
        }
        # skip if outputfile already exists
        outfile <- paste0(dataset$file, '-scored-', evalconfigname, '.tsv')
        if(file.exists(outfile)) {
          util$message(sprintf("Outputfile '%s' already exists! Skipping config '%s'.", outfile, evalconfigname))
          return(F)
        }
        rdsfile <- gsub('[.][^.]+$', '.rds', outfile) # replace the suffix with rds

        evalconfig <- .eval_configs()[[evalconfigname]](modelname, models)
        # define the function to apply for each row in the benchmark file
        rowfun <- function(i) {
          util$message(sprintf('Processing row %s/%s', i, nrow(benchmark)))
          row <- benchmark[i,]
          util$message(sprintf('Processing row %s/%s: <%s,%s>', i, nrow(benchmark), row$word1, row$word2))
          result <- evalconfig$score(row$word1, row$word2)
          # combine row with the concise result which is a dataframe row
          row <- cbind(row, result$concise)
          # return combined row and the full result (for information purposes)
          return(list(
            dfrow = row,
            full = result$full
          ))
        }

        # run the evalutaion
        ind <- seq_len(nrow(benchmark))
        if(is.null(par) || par <= 1) {
          evalconfig$init()
          scores <- lapply(X = ind, FUN = rowfun)
        } else {
          scores <- cclDef$lapply.par(
            ccl = par,
            exportitems = c('evalconfigname','evalconfig', 'benchmark', 'dataset','modelname', 'models', 'rowfun'),
            exportitemsenvir = environment(),
            initializationfun = function(){ library(senseasim); ft$.init(); evalconfig$init() },
            X = ind,
            FUN = rowfun
          )
        }

        # save the results
        saveRDS(scores, file = rdsfile) # save the full results
        scores <- lapply(scores, "[[", 1) %>% bind_rows # select only the concise result and make a dataframe
        write.table(scores, quote = F, sep = '\t', row.names = F, col.names = T, file = outfile)

      })
      return(T)
    });
  }

  tryeval <- function(par = NULL, dsetfilterfun = function(dset) !is.na(dset$modelname), run = list('rand')) {
    tryCatch(
      expr = {
        r <- eval(par, dsetfilterfun, run)
        util$sendmessage(subject = util$as.messagestring('Computation Finished! <EOM>'))
        return(r)
      },
      error = function(err) {
        util$sendmessage(subject = util$as.messagestring('Computation Failed!'), content = toString(err))
        return(err)
      }
    )
  }

})


# library(senseasim)
# senseasim$.init()
# d <- evaluate$.generate_sensevector_variants()
# d <- d[!is.na(senseinventory),]
# evaluate$.evaluate(d$load[[1]](), d$vsmodelname[[1]], d$senseinventory, par=2)

