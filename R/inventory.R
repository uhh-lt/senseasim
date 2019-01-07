inventory <- new.env(parent = .GlobalEnv)

with(inventory, {

  .inventories_loaded <- list()

  .inventories_available <- function() list(
    #
    #
    en_jbtsense_stanfordNew_finer = list(
      init = function() util$message('No loading neccessary.'),
      senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = 'en_jbt_stanfordNew', finer = T, isas = F)
    ),
    #
    en_jbtsense_stanfordNew = list(
      init = function() util$message('No loading neccessary.'),
      senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = 'en_jbt_stanfordNew', finer = F, isas = F)
    ),
    #
    #
    cluster__glove_6B_50d_1K__sim500cluster_cw = list(
      init = function() vsm$load_default_matrices(models_to_load = list('glove_6B_50d_1K')),
      senses = function(term, POS = NA)
        wsi$induceby.simcluster(
          term,
          modelname = 'glove_6B_50d_1K',
          topn.similar.terms = 500,
          simfun = senseasim$cos,
          simfun.name = 'cos',
          simfun.issymmetric = T,
          thresh = 0.66,
          minsize = 0,
          cluster.fun = function(X) { clust$cw(X, allowsingletons = F) },
          cluster.fun.name = 'cw_nosingletons')$itemlists
    ),
    #
    cluster__EN_100k_lsa__sim500cluster_cw = list(
      init = function() vsm$load_default_matrices(models_to_load = list('EN_100k_lsa')),
      senses = function(term, POS = NA)
        wsi$induceby.simcluster(
          term,
          modelname = 'EN_100k_lsa',
          topn.similar.terms = 500,
          simfun = senseasim$cos,
          simfun.name = 'cos',
          simfun.issymmetric = T,
          thresh = 0.66,
          minsize = 0,
          cluster.fun = function(X) { clust$cw(X, allowsingletons = F, remove_self_loops = T) },
          cluster.fun.name = 'cw_nosingletons')$itemlists
    ),
    #
    #
    dummy = list(
      init = function() util$message('Dummy load.'),
      senses = function(word, POS = NA) list(list(word))
    )
  )

  .get <- function(inventoryname) {
    if(!(inventoryname %in% names(.inventories_loaded))){
      loadedinventory <- .inventories_available()[[inventoryname]]
      loadedinventory$init()
      loadedinventory$name <- inventoryname
      .inventories_loaded[[length(.inventories_loaded)+1]] <<- loadedinventory
      names(.inventories_loaded)[[length(.inventories_loaded)]] <<- inventoryname
      return(loadedinventory)
    }
    return(.inventories_loaded[[inventoryname]])
  }

  sense_functions <- function(lazyloading = T) {
    sense_functions <- sapply(names(.inventories_available()), function(inventoryname) {
      if(!lazyloading)
        inventory <- .get(inventoryname)
      return(function(word, POS = NA){
        if(lazyloading)
          inventory <- .get(inventoryname)
        senselist <- inventory$senses(word, POS)
        if(!is.null(senselist) && length(senselist) > 0)
          return(senselist)
        util$message(sprintf("Term '%s' not found in inventory '%s'.", word, inventoryname))
        return(NULL)
      })
    })
    return(sense_functions)
  }

})

