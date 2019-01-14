inventory <- new.env(parent = .GlobalEnv)

with(inventory, {

  .inventories_loaded <- list()

  .inventories_available <- function() list(
    #
    #
    en_jbtsense_stanfordNew_finer = list(
      lang = 'en',
      init = function() util$message('No loading of inventory neccessary.'),
      senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = 'en_jbt_stanfordNew', finer = T, isas = F)
    ),
    #
    en_jbtsense_stanfordNew = list(
      lang = 'en',
      init = function() util$message('No loading of inventory neccessary.'),
      senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = 'en_jbt_stanfordNew', finer = F, isas = F)
    ),
    #
    #
    cluster__glove_6B_50d__sim500cluster_cw = list(
      lang = 'en',
      init = function() vsm$load_default_matrices(models_to_load = list('glove_6B_50d')),
      senses = function(term, POS = NA)
        wsi$induceby.simcluster.vsm(
          term,
          modelname = 'en_glove_6B_50d',
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
      init = function() vsm$load_default_matrices(models_to_load = list('en_100k_lsa')),
      senses = function(term, POS = NA)
        wsi$induceby.simcluster.vsm(
          term,
          modelname = 'en_100k_lsa',
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

  .get <- function(inventoryname, inventories = .inventories_available()) {
    if(!(inventoryname %in% names(.inventories_loaded))){
      loadedinventory <- inventories[[inventoryname]]
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
    result <- lapply(names(jbt$.jbt_models), function(jbtmodelname) {
      inventories_for_jbtmodel <- list()
      jbtmodel <- jbt$.jbt_models[[jbtmodelname]]
      print(jbtmodelname)
      # finer senses if available
      if(jbtmodel$finersensemodel){
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsense_${jbtmodel$name}_finer')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          init = function() util$message('No loading neccessary.'),
          senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = jbtmodelname, finer = T, isas = F)
        )
        inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      }
      # senses if available
      if(jbtmodel$sensemodel){
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsense_${jbtmodel$name}')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          init = function() util$message('No loading neccessary.'),
          senses = function(term, POS) jbt$get_JBT_senses(term, POS, jbt_modelname = jbtmodelname, finer = F, isas = F)
        )
        inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      }
      # senses by clustering jbt similar terms 'cluster__glove_6B_50d__sim500cluster_cw'
      vsmodelname <- 'glove_6B_50d'
      newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbt_${jbtmodel$name}__${vsmodelname}__sim500cluster_mcl')
      newjbtinventory <- list(
        lang = jbtmodel$lang,
        init = function() {  print(jbtmodelname); vsm$load_default_matrices(models_to_load = list(vsmodelname))},
        senses = function(term, POS) { print(jbtmodelname); wsi$induceby.simcluster.jbt(
          term = term,
          POS = POS,
          jbtmodelname = jbtmodelname,
          vsmodelname = vsmodelname,
          topn.similar.terms = 500,
          simfun = senseasim$cos,
          simfun.name = 'cos',
          simfun.issymmetric = T,
          thresh = 0.66,
          minsize = 0,
          cluster.fun = function(X) { clust$mcl(X, allowsingletons = F) },
          cluster.fun.name = 'mcl_nosingletons')$itemlists}
      )
      inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      return(inventories_for_jbtmodel)
    })
    result <- unlist(result, recursive = F, use.names = T)
    return(result)
  }

  sense_functions <- function(lazyloading = T, inventories = .inventories_available()) {
    sense_functions <- sapply(names(inventories), function(inventoryname) {
      if(!lazyloading)
        inventory <- .get(inventoryname, inventories)
      return(function(word, POS = NA){
        if(lazyloading)
          inventory <- .get(inventoryname, inventories)
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

