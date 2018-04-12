inventory <- new.env(parent = .GlobalEnv)

with(inventory, {

  .inventories_loaded <- list()

  .inventories_available <- function() list (
    de.1 = list(load = function() .load_vwsi_inventory('de.1', file.path(cache$data_dir(), 'senseinventories/fasttext/vwsi - cc.de.300.word_vectors.top50.inventory.tsv')) ),
    de.2 = list(load = function() .load_vwsi_inventory('de.2', file.path(cache$data_dir(), 'senseinventories/fasttext/vwsi - cc.de.300.word_vectors.top100.inventory.tsv')) ),
    de.3 = list(load = function() .load_vwsi_inventory('de.3', file.path(cache$data_dir(), 'senseinventories/fasttext/vwsi - cc.de.300.word_vectors.top200.inventory.tsv')) ),
    de.1k = list(load = function() .load_vwsi_inventory('de.1k', file.path(cache$data_dir(), 'senseinventories/fasttext/vwsi - cc.de.300.word_vectors.top50.inventory.tsv'), T) ),
    de.2k = list(load = function() .load_vwsi_inventory('de.2k', file.path(cache$data_dir(), 'senseinventories/fasttext/vwsi - cc.de.300.word_vectors.top100.inventory.tsv'), T) ),
    de.3k = list(load = function() .load_vwsi_inventory('de.3k', file.path(cache$data_dir(), 'senseinventories/fasttext/vwsi - cc.de.300.word_vectors.top200.inventory.tsv'), T) )
  )

  .add_to_loaded_models <- function (newmodel){
    .inventories_loaded[[length(.inventories_loaded)+1]] <<- newmodel
    names(.inventories_loaded)[[length(.inventories_loaded)]] <<- newmodel$name
  }

  .load_vwsi_inventory <- function(modelname, fdesc, keyw = F) {
    if(modelname %in% names(.inventories_loaded)){
      util$message(sprintf('Model \'%s\' already loaded.', modelname))
      return(T)
    }
    util$message(sprintf('Loading Model \'%s\'.', modelname))
    newmodel <- newEmptyObject()
    newmodel$name <- modelname
    newmodel$source <- fdesc
    newmodel$.data <- data.table::fread(fdesc, sep='\t', header=T, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
    newmodel$.data$cid <- NULL
    newmodel$.data$clusterwords <- lapply(strsplit(newmodel$.data$cluster, ',', fixed = T), function(words) gsub(' ', '_', trimws(words)))
    newmodel$.data$word <- gsub(' +', '_', trimws(newmodel$.data$word))
    newmodel$.data$keyword <- gsub(' +', '_', trimws(newmodel$.data$keyword))
    newmodel$.data$cluster <- NULL

    words <- unique(newmodel$.data$word)
    names(words) <- words
    senselists <- lapply(words, function(word){
      idxs <- which(newmodel$.data$word == word)
      if(keyw){
        wordsenselists <- lapply(newmodel$.data[idxs,]$keyword, list)
      }else{
        wordsenselists <- newmodel$.data[idxs,]$clusterwords
      }
      return(wordsenselists)
    })

    newmodel$senses <- senselists
    .add_to_loaded_models(newmodel)
    return(T)
  }

  .senseinventoryfunctions <- function(lazyloading = T){
    available <- .inventories_available()
    inventoryfunctions <- sapply(names(available), function(inventoryname) {
      if(!lazyloading)
        available[[inventoryname]]$load()
      return(function(word, POS = NA, vsmodelname = NA){
        if(lazyloading)
          if(!(inventoryname %in% names(.inventories_loaded)))
            available[[inventoryname]]$load()
        senselists <- .inventories_loaded[[inventoryname]]$senses
        if(word %in% names(senselists))
          return(senselists[[word]])
        util$message(sprintf("Term '%s' not found in inventory '%s'.", word, inventoryname))
        return(list())
      })
    })
    return(inventoryfunctions)
  }

})

