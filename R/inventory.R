inventory <- new.env(parent = .GlobalEnv)

with(inventory, {

  .inventories_loaded <- list()

  .inventories_available <- function() list(
    #
    #
    en_jbtsense_stanfordNew_finer = list(
      lang = 'en',
      init = function() util$message('No loading neccessary.'),
      senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = 'en_jbt_stanfordNew', finer = T, isas = F)
    ),
    #
    en_jbtsense_stanfordNew = list(
      lang = 'en',
      init = function() util$message('No loading neccessary.'),
      senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = 'en_jbt_stanfordNew', finer = F, isas = F)
    ),
    #
    #
    cluster__glove_6B_50d_1K__sim500cluster_cw = list(
      lang = 'en',
      init = function() vsm$load_default_matrices(models_to_load = list('glove_6B_50d_1K')),
      senses = function(term, POS = NA)
        wsi$induceby.simcluster.vsm(
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
      lang = 'en',
      init = function() vsm$load_default_matrices(models_to_load = list('EN_100k_lsa')),
      senses = function(term, POS = NA)
        wsi$induceby.simcluster.vsm(
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

  .get_best_for_lang <- function(lang) {
    # TODO: implement
    .inventories_loaded[[inventoryname]]
    return(NULL)
  }

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

  .generate_from_jbtmodels <- function() {
    # generate jbt models that have sense models
    for(jbtmodelname in names(jbt$.jbt_models)) {
      jbtmodel <- jbt$.jbt_models[[jbtmodelname]]
      # senses if available
      if(jbtmodel$sensemodel){
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsense_${jbtmodel$name}')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          init = function() util$message('No loading neccessary.'),
          senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = jbtmodel$name, finer = F, isas = F)
        )
      }
      # finer senses if available
      if(jbtmodel$finersensemodel){
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsense_${jbtmodel$name}_finer')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          init = function() util$message('No loading neccessary.'),
          senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = jbtmodel$name, finer = T, isas = F)
        )
      }
      # senses by clustering jbt similar terms 'cluster__glove_6B_50d_1K__sim500cluster_cw'
      newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbt_${jbtmodel$name}__${vsmmodelname}__sim500cluster_cw')
      newjbtinventory <- list(
        lang = jbtmodel$lang,
        init = function() vsm$load_default_matrices(models_to_load = list('glove_6B_50d_1K')),
        senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = jbtmodel$name, finer = T, isas = F)
      #   wsi$induceby.simcluster.vsm(
      #     term,
      #     modelname = 'glove_6B_50d_1K',
      #     topn.similar.terms = 500,
      #     simfun = senseasim$cos,
      #     simfun.name = 'cos',
      #     simfun.issymmetric = T,
      #     thresh = 0.66,
      #     minsize = 0,
      #     cluster.fun = function(X) { clust$cw(X, allowsingletons = F) },
      #     cluster.fun.name = 'cw_nosingletons')$itemlists
      # )
      )
    }
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

