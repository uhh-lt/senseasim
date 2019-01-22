
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
  .datasets <- function(path = NULL) {
    if(is.null(path))
      path <- file.path(cache$data_dir(), 'sim-multilang','SemR-11')
    dt <- data.table::data.table(filename = list.files(path, pattern = '^.+[.]dataset$', full.names = T), stringsAsFactors = F)
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

  .generate_eval_file <- function(){
    d <- .datasets()
    dn <- lapply(seq_len(nrow(dt)), function(i){
      di <- d[i, ]
      # get the best vector space model name for lang
      vsmodelname <- vsm$.get_best_modelname_for_lang(di$lang)
      if(is.null(vsmodelname))
        return(di)
      # get all sense inventories for lang
      sinventories <- inventory$.modelnames_for_lang(di$lang)
      # get the first element of each inventory type if it exists
      sinventoriesfiltered <- c(
        tryCatch(grep('_jbtsense_', sinventories, value = T)[[1]], error = function(e) list())
      )
      # use only inventories where the model is based on the found vsmodelname
      vsmodelavailable <- vsm$.models_available()[[vsmodelname]]
      sinventories <- grep(paste0('_',vsmodelavailable$basename,'_'), sinventories, value = T)
      # add the jbtsim and vsmsim type inventory
      sinventoriesfiltered <- c(
        sinventoriesfiltered,
        tryCatch(grep('_jbtsim_', sinventories, value = T)[[1]], error = function(e) list()),
        tryCatch(grep('_vsmsim_', sinventories, value = T)[[1]], error = function(e) list())
      )
      # add all information to the data table
      di$vsmodelname <- vsmodelname
      di <- di[rep(1, times=length(sinventoriesfiltered))]
      di$inventory <- sinventoriesfiltered
      return(di)
    })
    dn$fill=T
    dn <- do.call(rbind, dn)
  }

  .eval_configs <- function() { list() }

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

