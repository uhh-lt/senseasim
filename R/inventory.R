inventory <- new.env(parent = .GlobalEnv)

with(inventory, {

  models <- list()

  .inventories_loaded <- list()

  .INITIALIZED <- F
  .init <- function(reinitialize = F) {
    if(!.INITIALIZED || reinitialize){
      vsm$.init()
      jbt$.init()
      models_available <- c(.inventories_available(), .generate_from_jbtmodels())
      models <<- .get_models(T, models_available)
      .INITIALIZED <- T
    }
  }

  .inventories_available <- function() list(
    #
    en_jbtsense_stanfordNew_finer = list(
      lang = 'en',
      init = function() { },
      senses = function(term, POS) jbt$models[['en_jbt_stanfordNew']]()$senses(term, POS, finer = T, isas = F)
    ),
    #
    en_jbtsense_stanfordNew = list(
      lang = 'en',
      init  = function() { },
      senses = function(term, POS) jbt$models[['en_jbt_stanfordNew']]()$senses(term, POS, finer = F, isas = F)
    ),
    #
    #
    #
    cluster__glove_6B_50d__sim500cluster_cw = list(
      lang = 'en',
      init = function() { },
      senses = function(term, POS = NA)
        wsi$induceby.simcluster.vsm(
          term,
          vsmodel = vsm$models[['en_glove_6B_50d']](),
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
      init = function() { },
      senses = function(term, POS = NA)
        wsi$induceby.simcluster.vsm(
          term,
          vsmodel = vsm$models[['en_100k_lsa']](),
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

  .generate_from_jbtmodels <- function() {
    # generate jbt models that have sense models
    result <- lapply(names(jbt$models), function(jbtmodelname) {
      inventories_for_jbtmodel <- list()
      jbtmodel <- jbt$models[[jbtmodelname]]()
      # finer senses if available
      if(jbtmodel$finersensemodel){
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsense_${jbtmodel$name}_finer')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          init = function() {},
          senses = function(term, POS) jbtmodel$senses(term, POS, finer = T, isas = F)
        )
        inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      }
      # senses if available
      if(jbtmodel$sensemodel){
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsense_${jbtmodel$name}')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          init = function() { },
          senses = function(term, POS) jbtmodel$senses(term, POS, finer = F, isas = F)
        )
        inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      }
      vsmodelname <- vsm$.get_best_modelname_for_lang(jbtmodel$lang)
      if(!is.null(vsmodelname)){
        vsmodel <- vsm$models[[vsmodelname]]
        # senses by clustering jbt similar terms 'cluster__glove_6B_50d__sim500cluster_cw'
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbt_${jbtmodel$name}__${vsmodelname}__sim500cluster_mcl')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          init = function() { },
          senses = function(term, POS) wsi$induceby.simcluster.jbt(
            term = term,
            POS = POS,
            jbtmodel = jbtmodel,
            vsmodel = vsmodel(),
            topn.similar.terms = 500,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 0.66,
            minsize = 0,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F) },
            cluster.fun.name = 'mcl_nosingletons')$itemlists
        )
        inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      }
      return(inventories_for_jbtmodel)
    })
    result <- unlist(result, recursive = F, use.names = T)
    return(result)
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

  .get_models <- function(lazyloading = T, inventories = .inventories_available()) {
    models <- sapply(names(inventories), function(inventoryname) {
      if(!lazyloading)
        model <- .get(inventoryname, inventories)
      return(function(){
        if(lazyloading)
          model <- .get(inventoryname, inventories)
        return(model)
      })
    })
    return(models)
  }

})



