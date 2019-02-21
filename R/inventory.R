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
      man_jbtsense_stanfordNew_finer = list(
        lang = 'en',
        senses = function(term, POS) jbt$models[['en_jbt_stanfordNew']]()$senses(term, POS, finer = T, isas = F)
      ),
      #
      man_cluster__glove_6B_50d__sim500cluster_cw = list(
        lang = 'en',
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = 'en_glove_6B_50d',
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
      man_cluster__EN_100k_lsa__sim500cluster_cw = list(
        lang = 'en',
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = 'en_100k_lsa',
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
      man_dummy = list(
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
      vsmodels_available = vsm$.models_available()
      vsmmodelsforlang <- vsm$.modelnames_for_lang(jbtmodel$lang)
      inventories_for_jbtsim <- lapply(vsmmodelsforlang, function(vsmodelname) {
        vsmbasename <- vsmodels_available[[vsmodelname]]$basename
        # senses by clustering jbt similar terms
        newmodel_mcl1 <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) wsi$induceby.simcluster.jbt(
            term = term,
            POS = POS,
            jbtmodel = jbtmodel,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 0.8,
            minsize = 3,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'mcl_400_t0.8_nosingletons_noloops')$itemlists
        )

        newmodel_mcl2 <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) wsi$induceby.simcluster.jbt(
            term = term,
            POS = POS,
            jbtmodel = jbtmodel,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'mean',
            minsize = 3,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'mcl_400_tmean_nosingletons_noloops')$itemlists
        )

        newmodel_mcl3 <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) wsi$induceby.simcluster.jbt(
            term = term,
            POS = POS,
            jbtmodel = jbtmodel,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'prune.sym.n10',
            minsize = 3,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'mcl_400_tprune.sym.n10_nosingletons_noloops')$itemlists
        )

        newmodel_cw1 <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) wsi$induceby.simcluster.jbt(
            term = term,
            POS = POS,
            jbtmodel = jbtmodel,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 0.8,
            minsize = 3,
            cluster.fun = function(X) { clust$cw(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'cw_400_t0.8_nosingletons_noloops')$itemlists
        )

        newmodel_cw2 <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) wsi$induceby.simcluster.jbt(
            term = term,
            POS = POS,
            jbtmodel = jbtmodel,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'mean',
            minsize = 3,
            cluster.fun = function(X) { clust$cw(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'cw_400_tmean_nosingletons_noloops')$itemlists
        )

        newmodel_cw3 <- list(
          lang = jbtmodel$lang,
          senses = function(term, POS) wsi$induceby.simcluster.jbt(
            term = term,
            POS = POS,
            jbtmodel = jbtmodel,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'prune.sym.n10',
            minsize = 3,
            cluster.fun = function(X) { clust$cw(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'cw_400_tprune.sym.n10_nosingletons_noloops')$itemlists
        )

        modelsaslist <- list()
        modelsaslist[[stringr::str_interp('${jbtmodel$lang}_jbtsim__${jbtmodel$apiname}__${vsmbasename}__sim400cluster_mcl_t0.8')]] <- newmodel_mcl1
        modelsaslist[[stringr::str_interp('${jbtmodel$lang}_jbtsim__${jbtmodel$apiname}__${vsmbasename}__sim400cluster_mcl_tmean')]] <- newmodel_mcl2
        modelsaslist[[stringr::str_interp('${jbtmodel$lang}_jbtsim__${jbtmodel$apiname}__${vsmbasename}__sim400cluster_mcl_tprune.sym.n10')]] <- newmodel_mcl3
        modelsaslist[[stringr::str_interp('${jbtmodel$lang}_jbtsim__${jbtmodel$apiname}__${vsmbasename}__sim400cluster_cw_t0.8')]] <- newmodel_cw1
        modelsaslist[[stringr::str_interp('${jbtmodel$lang}_jbtsim__${jbtmodel$apiname}__${vsmbasename}__sim400cluster_cw_tmean')]] <- newmodel_cw2
        modelsaslist[[stringr::str_interp('${jbtmodel$lang}_jbtsim__${jbtmodel$apiname}__${vsmbasename}__sim400cluster_cw_tprune.sym.n10')]] <- newmodel_cw3

        return(modelsaslist)
      })
      inventories_for_jbtsim <- unlist(inventories_for_jbtsim, recursive = F, use.names = T)
      inventories_for_jbtmodel <- c(inventories_for_jbtmodel, inventories_for_jbtsim)
      return(inventories_for_jbtmodel)
    })
    result <- unlist(result, recursive = F, use.names = T)
    return(result)
  }

  .generate_from_vsm <- function(vsmodels_available = vsm$.models_available()){
    models <- lapply(names(vsmodels_available), function(vsmodelname) {
      lang <- vsmodels_available[[vsmodelname]]$lang
      vsmbasename <- vsmodels_available[[vsmodelname]]$basename
      newmodel_mcl1 = list(
        lang = lang,
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 0.8,
            minsize = 3,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'mcl_400_t0.8_nosingletons_noloops')$itemlists
      )
      newmodel_cw1 = list(
        lang = lang,
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 0.8,
            minsize = 3,
            cluster.fun = function(X) { clust$cw(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'cw_400_t0.8_nosingletons_noloops')$itemlists
      )
      newmodel_mcl2 = list(
        lang = lang,
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'mean',
            minsize = 3,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'mcl_400_tmean_nosingletons_noloops')$itemlists
      )
      newmodel_cw2 = list(
        lang = lang,
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'mean',
            minsize = 3,
            cluster.fun = function(X) { clust$cw(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'cw_400_tmean_nosingletons_noloops')$itemlists
      )
      newmodel_mcl3 = list(
        lang = lang,
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'prune.sym.n10',
            minsize = 3,
            cluster.fun = function(X) { clust$mcl(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'mcl_400_tprune.sym.n10_nosingletons_noloops')$itemlists
      )
      newmodel_cw3 = list(
        lang = lang,
        senses = function(term, POS = NA)
          wsi$induceby.simcluster.vsm(
            term,
            vsmodel = vsmodelname,
            topn.similar.terms = 400,
            simfun = senseasim$cos,
            simfun.name = 'cos',
            simfun.issymmetric = T,
            thresh = 'prune.sym.n10',
            minsize = 3,
            cluster.fun = function(X) { clust$cw(X, allowsingletons = F, remove_self_loops = T) },
            cluster.fun.name = 'cw_400_tprune.sym.n10_nosingletons_noloops')$itemlists
      )

      modelsaslist <- list()
      modelsaslist[[stringr::str_interp('${lang}_vsmsim__${vsmbasename}__sim400cluster_mcl_t0.8')]] <- newmodel_mcl1
      modelsaslist[[stringr::str_interp('${lang}_vsmsim__${vsmbasename}__sim400cluster_mcl_tmean')]] <- newmodel_mcl2
      modelsaslist[[stringr::str_interp('${lang}_vsmsim__${vsmbasename}__sim400cluster_mcl_tprune.sym.n10')]] <- newmodel_mcl3
      modelsaslist[[stringr::str_interp('${lang}_vsmsim__${vsmbasename}__sim400cluster_cw_t0.8')]] <- newmodel_cw1
      modelsaslist[[stringr::str_interp('${lang}_vsmsim__${vsmbasename}__sim400cluster_cw_tmean')]] <- newmodel_cw2
      modelsaslist[[stringr::str_interp('${lang}_vsmsim__${vsmbasename}__sim400cluster_cw_tprune.sym.n10')]] <- newmodel_cw3

      return(modelsaslist)
    })
    models <- unlist(models, recursive = F, use.names = T)
    return(models)
  }

  .get <- function(inventoryname, inventories = .models_available()) {
    if(!(inventoryname %in% names(.inventories_loaded))){
      loadedinventory <- inventories[[inventoryname]]
      loadedinventory$name <- inventoryname
      .inventories_loaded[[length(.inventories_loaded)+1]] <<- loadedinventory
      names(.inventories_loaded)[[length(.inventories_loaded)]] <<- inventoryname
      return(loadedinventory)
    }
    return(.inventories_loaded[[inventoryname]])
  }

  .get_models <- function(lazyloading = T, inventories = .models_available()) {
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



