vsm <- new.env(parent = .GlobalEnv)

with(vsm, {

  .models_loaded <- list()

  .models <- function() list(
    w2v_gnews_300   = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/w2v/GoogleNews-vectors-negative300.txt'),
      transformer = function(w) w
    ),
    glove_6B_50d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/glove/glove.6B.50d.txt'),
      transformer    = function(w) tolower(w)
    ),
    glove_6B_50d_1K = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/glove/glove.6B.50d.1K.txt'),
      transformer    = function(w) tolower(w)
    ),
    glove_6B_300d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/glove/glove.6B.300d.txt'),
      transformer    = function(w) tolower(w)
    ),
    sympat300d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/sympatEmb/sp_plus_embeddings_300.txt'),
      transformer    = function(w) tolower(w)
    ),
    sympat500d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/sympatEmb/sp_plus_embeddings_500.txt'),
      transformer    = function(w) tolower(w)
    ),
    sympat10000d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/sympatEmb/sp_plus_embeddings_10000.txt'),
      transformer    = function(w) tolower(w)
    ),
    paragramSL = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/paragram/paragram_300_sl999/paragram_300_sl999.txt'),
      transformer    = function(w) tolower(w)
    ),
    paragramWS = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/paragram/paragram_300_ws353/paragram_300_ws353.txt'),
      transformer    = function(w) tolower(w)
    ),
    EN_100k_hal_lsa = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/lsafun/EN_100k'),
      transformer    = function(w) tolower(w)
    ),
    EN_100k_lsa = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/lsafun/EN_100k_lsa'),
      transformer    = function(w) tolower(w)
    )
  )


  .sensemodels <- function(){
    return(list(
      'adagram' = list(paste0(Sys.getenv(c('DATA_HOME')),'/adagram/clean_lemma_model_alpha_05.txt'), function(w) tolower(w)),
      'autoextend' = list(paste0(Sys.getenv(c('DATA_HOME')),'/autoextend/lexemes.txt'), function(w) tolower(w))
    ))
  }

  load_default_matrices <- function(models_to_load = list('EN_100k_lsa')) {
    # load latent vectors
    for(modelname in names(.models())){
      if(modelname %in% models_to_load){
        result_list <- load_matrix(modelname, .models())
      }
    }
    for(modelname in names(.sensemodels())){
      if(modelname %in% models_to_load){
        result_list <- load_matrix(modelname, .sensemodels())
      }
    }
    message(sprintf('[%s-%d-%s] Matrix size: %s', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), format(object.size(.models_loaded), units = "auto")))
    return(T)
  }

  load_matrix <- function(modelname, models) {
    if(modelname %in% names(.models_loaded)){
      message(sprintf('[%s-%d-%s] model \'%s\' already loaded. \nDo you need to run \'toBigMatrix.R\'?', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), modelname))
      return(T)
    }

    add_to_loaded_models <- function (newmodel){
      .models_loaded[[length(.models_loaded)+1]] <<- newmodel
      names(.models_loaded)[[length(.models_loaded)]] <<- newmodel$name
    }

    # bigmatrix descriptorfile
    fdesc <- {
      if(modelname %in% names(models))
        paste0(models[[modelname]][[1]],'.bin.desc')
      else
        stop(sprintf('modelname is unknonwn \'%s\'.', modelname))
    }

    if(!file.exists(fdesc))
      stop(sprintf('[%s-%d-%s] loading Vector Space Matrix from \'%s\' failed, file does not exists. \nDo you need to run \'toBigMatrix.R\'?', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), fdesc))

    # else read vector space matrix as bigmatrix
    message(sprintf('[%s-%d-%s] loading Vector Space Matrix \'%s\'', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), modelname))
    newmodel <- newEmptyObject()
    require(bigmemory); options(bigmemory.allow.dimnames = TRUE)
    newmodel$M <- bigmemory::attach.big.matrix(obj = basename(fdesc), path = dirname(fdesc), bigmemory.allow.dimnames = TRUE)
    newmodel$vocab <- rownames(newmodel$M)
    newmodel$name <- modelname
    newmodel$transform <- models[[modelname]][[2]]
    rownames(newmodel$M) <- NULL
    add_to_loaded_models(newmodel)

    return(T)
  }

  get_vector <- function(term, modelname) {
    model <- .models_loaded[[modelname]]
    mterm <- model$transform(term)
    idx <- which(model$vocab == mterm)
    if(length(idx) > 0){
      return(model$M[idx,])
    }
    return(matrix(NA, nrow=ncol(M))) # create a NA valued matrix with one vector and the dim of M)
  }

})
