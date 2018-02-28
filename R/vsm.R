vsm <- new.env(parent = .GlobalEnv)

with(vsm, {

  .models_loaded <- list()

  .models <- function() list(
    w2v_gnews_300   = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/w2v/GoogleNews-vectors-negative300.txt'),
      transformer = function(w) w,
      unk = 'UNK'
    ),
    glove_6B_50d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/glove/glove.6B.50d.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    glove_6B_50d_1K = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/glove/glove.6B.50d.1K.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    glove_6B_300d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/glove/glove.6B.300d.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    sympat300d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/sympatEmb/sp_plus_embeddings_300.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    sympat500d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/sympatEmb/sp_plus_embeddings_500.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    sympat10000d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/sympatEmb/sp_plus_embeddings_10000.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    paragramSL = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/paragram/paragram_300_sl999/paragram_300_sl999.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    paragramWS = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/paragram/paragram_300_ws353/paragram_300_ws353.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    EN_100k_hal_lsa = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/lsafun/EN_100k'),
      transformer    = function(w) tolower(w),
      unk = 'unknown'
    ),
    EN_100k_lsa = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/lsafun/EN_100k_lsa'),
      transformer    = function(w) tolower(w),
      unk = 'unknown'
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
        paste0(models[[modelname]]$local_location,'.bin.desc')
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
    newmodel$unk <- list(mterm = models[[modelname]]$unk, idx = which(newmodel$vocab == models[[modelname]]$unk))
    newmodel$transform <- function(term) { get_vocab_term(term, models[[modelname]]$transformer, newmodel) }
    rownames(newmodel$M) <- NULL
    colnames(newmodel$M) <- NULL
    add_to_loaded_models(newmodel)

    return(T)
  }

  get_vector <- function(term, modelname, .as_column = F) {
    model <- .models_loaded[[modelname]]
    mterm <- model$transform(term)
    if(length(mterm$idx) > 0){
      v <- model$M[mterm$idx,]
      v <- matrix(nrow = 1, data = v, dimnames = list(mterm$mterm), byrow = T)
    }else{
      v <- matrix(NA, nrow=1, ncol=ncol(M), dimnames = list(mterm$mterm)) # create a NA valued matrix with one vector and the dim of M)
    }
    if(.as_column){
      v <- t(v)
    }
    return(v)
  }

  get_vocab_term <- function(term, tfun, model){
    mterm <- tfun(term)
    idx <- which(model$vocab == mterm)
    if(length(idx) > 0) {
      return(list('mterm' = mterm, 'idx' = idx))
    }else{
      return(model$unk)
    }
  }

})
