inventory <- new.env(parent = .GlobalEnv)

with(inventory, {

  models <- list()

  .inventories_loaded <- list()

  .INITIALIZED <- F
  .init <- function(reinitialize = F) {
    if(!.INITIALIZED || reinitialize){
      vsm$.init()
      jbt$.init()
      models_available <- .models_available()
      models <<- .get_models(T, models_available)
      .INITIALIZED <- T
    }
  }

  .models_available <- function() { c(
    # manually define some inventories
    list(
      #
      en_jbtsense_stanfordNew_finer = list(
        lang = 'en',
        senses = function(term, POS) jbt$models[['en_jbt_stanfordNew']]()$senses(term, POS, finer = T, isas = F)
      ),
      #
      en_jbtsense_stanfordNew = list(
        lang = 'en',
        senses = function(term, POS) jbt$models[['en_jbt_stanfordNew']]()$senses(term, POS, finer = F, isas = F)
      ),
      #
      #
      #
      cluster__glove_6B_50d__sim500cluster_cw = list(
        lang = 'en',
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsm$models[['en_glove_6B_50d']](),
            topn.similar.terms = 500,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'median',
            minsize = 0,
            cluster.fun = function(X) { clust$cw(X, allowsingletons = F) },
            cluster.fun.name = 'cw_nosingletons')$itemlists
      ),
      #
      cluster__EN_100k_lsa__sim500cluster_cw = list(
        lang = 'en',
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsm$models[['en_100k_lsa']](),
            topn.similar.terms = 500,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'mean',
            minsize = 0,
            cluster.fun = function(X) { clust$cw(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'cw_nosingletons')$itemlists
      ),
      #
      #
      #
      dummy = list(
        lang = 'en',
        senses = function(word, POS = NA) list(list(word))
      )
    ),
    # automatically add inventories
    .generate_from_jbtmodels(),
    .generate_from_vsm()
  )}

  .modelnames_for_lang <- function(lang) {
    matching_models <- grep(paste0('^', lang, '_'), names(models), value=T)
    return(matching_models)
  }

  .get_best_modelname_for_lang <- function(lang) {
    matching_models <- .modelnames_for_lang(lang)
    if(length(matching_models) > 0){
      return(matching_models[[1]])
    }
    return(NULL)
  }

  .generate_from_jbtmodels <- function() {
    # generate jbt models that have sense models
    result <- lapply(names(jbt$models), function(jbtmodelname) {
      inventories_for_jbtmodel <- list()
      jbtmodel <- jbt$models[[jbtmodelname]]()
      # finer senses if available
      if(jbtmodel$finersensemodel){
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsense__${jbtmodel$apiname}_finer')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) jbtmodel$senses(term, POS, finer = T, isas = F)
        )
        inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      }
      # senses if available
      if(jbtmodel$sensemodel){
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsense__${jbtmodel$apiname}')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) jbtmodel$senses(term, POS, finer = F, isas = F)
        )
        inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      }
      # for each vsm model and jbt model in the same language generate an inventory
      for(vsmodelname in vsm$.modelnames_for_lang(jbtmodel$lang)){
        vsmodel <- vsm$models[[vsmodelname]]
        # senses by clustering jbt similar terms
        newjbtinventoryname <- stringr::str_interp('${jbtmodel$lang}_jbtsim__${jbtmodel$apiname}__${vsmodelname}__sim500cluster_mcl')
        newjbtinventory <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) wsi$induceby.simcluster.jbt(
            term = term,
            POS = POS,
            jbtmodel = jbtmodel,
            vsmodel = vsmodel(),
            topn.similar.terms = 500,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'mean',
            minsize = 0,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'mcl_tmean_nosingletons_noloops')$itemlists
        )
        inventories_for_jbtmodel[[newjbtinventoryname]] <- newjbtinventory
      }
      return(inventories_for_jbtmodel)
    })
    result <- unlist(result, recursive = F, use.names = T)
    return(result)
  }

  .generate_from_vsm <- function(vsmodels_available = vsm$.models_available()){
    models <- lapply(names(vsmodels_available), function(vsmodelname) {
      lang <- vsmodels_available[[vsmodelname]]$lang
      vsmbasename <- vsmodels_available[[vsmodelname]]$basename
      newmodelname <- stringr::str_interp('${lang}_vsmsim__${vsmbasename}__sim500cluster_mcl')
      newmodel = list(
        lang = lang,
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsm$models[[vsmodelname]](),
            topn.similar.terms = 500,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'mean',
            minsize = 0,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'mcl_tmean_nosingletons_noloops')$itemlists
      )
      modelaslist <- list(newmodel)
      names(modelaslist) <- newmodelname
      return(modelaslist)
    })
    models <- unlist(models, recursive = F, use.names = T)
    return(models)
  }

  .get <- function(inventoryname, inventories = .inventories_available()) {
    if(!(inventoryname %in% names(.inventories_loaded))){
      loadedinventory <- inventories[[inventoryname]]
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



