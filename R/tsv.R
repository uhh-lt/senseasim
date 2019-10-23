tsv <- new.env(parent = .GlobalEnv)

with(tsv, {

  models <- list()

  .models_loaded <- list()

  .INITIALIZED <- F
  .init <- function(reinitialize = F) {
    if(!.INITIALIZED || reinitialize){
      .models_loaded <<- list()
      models_available <- .models_available()
      models <<- .get_models(T, models_available)
      .INITIALIZED <- T
    }
  }

  .models_available <- function(){c(
    #
    .generate_cc_from_dir(paste0(cache$data_dir(),'/senseinventories/fasttext/induced'))
    #
  )}

  .generate_cc_from_dir <- function(location, keyw=F){
    if(!dir.exists(location))
      return(list())

    modelfiles <- list.files(path = location, pattern = '*inventory.tsv$', full.names = T, recursive = T)
    models <- lapply(modelfiles, function(modelfile) {
      fname <- basename(modelfile)
      newmodelname <- gsub('\\s*', '', gsub('^([^.]+).([^.]+).(\\d+).*(top\\d+)\\.inventory.tsv$', '\\2_ft_\\1_\\3_\\4', fname))
      newmodel <- list(
        location = modelfile,
        lang = gsub('^([^_]+)_(.*)$', '\\1', newmodelname),
        basename = gsub('^([^_]+)_(.*)$', '\\2', newmodelname),
        init = function() .load(modelfile = modelfile, keyw = keyw, lines=Inf, transform=tolower),
        senses = function(word, POS = NA) .getsenses(newmodelname, word),
        transform = function(w) tolower(w)
      )
      modelaslist <- list(newmodel)
      names(modelaslist) <- newmodelname
      return(modelaslist)
    })
    models <- unlist(models, recursive = F, use.names = T)
    return(models)
  }

  .load <- function(modelfile, keyw=F, lines=Inf, transform=function(w) w){ #
    util$message(sprintf('Loading TSV Sense Inventory from \'%s\'.', modelfile))
    newmodel <- newEmptyObject()
    newmodel$.data <- data.table::fread(modelfile, sep='\t', header=T, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=T, quote="", nrows=lines)
    newmodel$.data$cid <- NULL
    newmodel$.data$word <- transform(gsub(' +', '_', trimws(newmodel$.data$word)))
    newmodel$keywordsonly <- keyw
    if(keyw){
      newmodel$.data$keyword <- gsub(' +', '_', trimws(newmodel$.data$keyword))
      newmodel$.senselists <- function(w) {
        rows <- newmodel$.data[word==w]
        if(nrow(rows) == 0)
          return(list())
        return(rows$keyword)
      }
    }else{
      newmodel$.data$clusterwords <- lapply(strsplit(newmodel$.data$cluster, ',', fixed = T), function(words) gsub(' ', '_', trimws(words)))
      newmodel$.senselists <- function(w) {
        rows <- newmodel$.data[word==w]
        if(nrow(rows) == 0)
          return(list())
        return(rows$clusterwords)
      }
    }
    newmodel$.data$cluster <- NULL
    return(newmodel)
  }

  .getsenses <- function(modelname, word, POS = NA){
    m <- .getmodel(modelname)
    w <- m$transform(word)
    senses <- m$.senselists(w)
    return(senses)
  }

  .getmodel <- function(modelname, models=.models_available()){
    print(modelname)
    if(!(modelname %in% names(.models_loaded))){
      model <- models[[modelname]]
      loaded_model <- model$init()
      loaded_model$lang <- model$lang
      loaded_model$location <- model$location
      loaded_model$basename <- model$basename
      loaded_model$senses <- model$senses
      loaded_model$transform <- model$transform
      loaded_model$name <- modelname
      # add to list of loaded models
      .models_loaded[[length(.models_loaded)+1]] <<- loaded_model
      names(.models_loaded)[[length(.models_loaded)]] <<- modelname
      return(loaded_model)
    }
    return(.models_loaded[[modelname]])
  }

  .get_models <- function(lazyloading = T, models = .models_available()) {
    m <- sapply(names(models), function(modelname) {
      if(!lazyloading)
        model <- .getmodel(modelname, models)
      return(function(){
        if(lazyloading)
          model <- .getmodel(modelname, models)
        return(model)
      })
    })
    return(m)
  }

})

