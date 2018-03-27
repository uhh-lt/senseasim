vsm <- new.env(parent = .GlobalEnv)

with(vsm, {

  .models_loaded <- list()

  .models <- function() list(
    w2v_gnews_300   = list(
      local_location = paste0(cache$data_dir(), '/w2v/GoogleNews-vectors-negative300.txt'),
      transformer = function(w) w,
      unk = 'unknown'
    ),
    glove_6B_50d = list(
      local_location = paste0(cache$data_dir(),'/glove/glove.6B.50d.txt'),
      transformer    = function(w) tolower(w),
      unk = 'unknown'
    ),
    glove_6B_50d_1K = list(
      local_location = paste0(cache$data_dir(),'/glove/glove.6B.50d.1K.txt'),
      transformer    = function(w) tolower(w),
      unk = 'the'
    ),
    glove_6B_300d = list(
      local_location = paste0(cache$data_dir(),'/glove/glove.6B.300d.txt'),
      transformer    = function(w) tolower(w),
      unk = 'unknown'
    ),
    sympat300d = list(
      local_location = paste0(Sys.getenv(c('DATA_HOME')),'/sympatEmb/sp_plus_embeddings_300.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    sympat500d = list(
      local_location = paste0(cache$data_dir(),'/sympatEmb/sp_plus_embeddings_500.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    sympat10000d = list(
      local_location = paste0(cache$data_dir(),'/sympatEmb/sp_plus_embeddings_10000.txt'),
      transformer    = function(w) tolower(w),
      unk = 'UNK'
    ),
    paragramSL = list(
      local_location = paste0(cache$data_dir(),'/paragram/paragram_300_sl999/paragram_300_sl999.txt'),
      transformer    = function(w) tolower(w),
      unk = 'unknown'
    ),
    paragramWS = list(
      local_location = paste0(cache$data_dir(),'/paragram/paragram_300_ws353/paragram_300_ws353.txt'),
      transformer    = function(w) tolower(w),
      unk = 'unknown'
    ),
    EN_100k_hal_lsa = list(
      local_location = paste0(cache$data_dir(),'/lsafun/EN_100k'),
      transformer    = function(w) tolower(w),
      unk = 'unknown'
    ),
    EN_100k_lsa = list(
      local_location = paste0(cache$data_dir(),'/lsafun/EN_100k_lsa'),
      transformer    = function(w) tolower(w),
      unk = 'unknown'
    ),
    ft_de = list(
      local_location = paste0(cache$data_dir(),'/fasttext/cc.de.300.vec.gz'),
      transformer    = function(w) w,
      unk = 'unknown'
    )
  )

  .sensemodels <- function() list(
    adagram = list(
      local_location = paste0(cache$data_dir(),'/adagram/clean_lemma_model_alpha_05.txt'),
      transformer    = function(w) tolower(w),
      unk = 'the'
    ),
    autoextend = list(
      local_location = paste0(cache$data_dir(),'/autoextend/lexemes.txt'),
      transformer = function(w) tolower(w),
      unk = 'the'
    )
  )


  build_bigmatrix_from_txt <- function(filename, separator = ' ') {
    # get the filenames
    bckngpath <- dirname(filename)
    bckngfile <- paste0(basename(filename), '.bin')
    bckngdesc <- paste0(bckngfile, '.desc')
    bckngrownames <- file.path(bckngpath, paste0(bckngfile, '.rownames'))


    message(sprintf('[%s-%d-%s] Trying to convert Vector Space Matrix: \n  input: \'%s\' \n  path:  \'%s\' \n  bin:   \'%s\'  \n  desc:  \'%s\' ',
                    gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"),
                    filename, bckngpath, bckngfile, bckngdesc))

    if(file.exists(file.path(bckngpath, bckngdesc))) {
      util$message('Descriptor file exists. Skipping.')
      return(T)
    }

    if(!file.exists(filename)) {
      util$message('Input file does not exist. Aborting.')
      return(F)
    }

    # read matrix, convert to bigmatrix and store descriptor and binary backing file
    tictoc::tic('Elapsed')
    tictoc::tic('Finished loading.')

    message(sprintf('[%s-%d-%s] Loading...', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S")))

    if(endsWith(filename, '.gz')){
      df <- data.table::fread(sprintf('cat %s | gzip -d', filename), sep=separator, header=F, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
    }else{
      df <- data.table::fread(filename, sep=separator, header=F, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
    }

    colnames(df) <- NULL # remove colnames
    tictoc::tic('Fixed missing rowname values.')
    message(sprintf('[%s-%d-%s] Fixing missing values...', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S")))
    missing_names <- which(is.na(df[,1]) | is.null(df[,1]) | df[,1] == '') # first column is vocabulary, find missing values
    df[missing_names, 1] <- sapply(missing_names, function(ri) paste0('missing_row_',ri)) # fix missing values
    message(sprintf('[%s-%d-%s] Removed %d vectors with missing rownames.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), length(missing_names)))
    tictoc::toc()

    tictoc::tic('Fixed unique rownames.')
    message(sprintf('[%s-%d-%s] Fixing unique rownames...', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S")))
    rows_unique <- which(!duplicated(df[,1]))
    message(sprintf('[%s-%d-%s] Removing %d vectors with non-unique rownames...', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), nrow(df) - length(rows_unique)))
    df <- df[rows_unique,]
    tictoc::toc()

    vocab <- df[,1] # first column is vocabulary
    df <- df[,-1] # remove first column
    rownames(df) <- NULL

    tictoc::toc()

    message(sprintf('[%s-%d-%s] Data size: %s', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), format(object.size(df), units = "auto")))
    message('Memory usage:')
    print.table(gc(reset=T)) # show some memory usage

    tictoc::tic('Finished converting.')
    message(sprintf('[%s-%d-%s] Converting to bigmatrix...', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S")))
    m <- as.matrix(df)
    bm <- bigmemory::as.big.matrix(m, backingfile = bckngfile, backingpath = bckngpath, descriptorfile = bckngdesc, shared = T)
    # save vocabulary file
    writeLines(vocab, bckngrownames)
    tictoc::toc()

    # make some assertions
    assertthat::are_equal(length(vocab), nrow(bm))

    # free memory
    rm(df)
    message('Memory usage:')
    print.table(gc(reset=T)) # show some memory usage
    tictoc::toc()

    return(T)
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
      message(sprintf('[%s-%d-%s] model \'%s\' already loaded.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), modelname))
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

    if(!file.exists(fdesc)) {
      if(!build_bigmatrix_from_txt(models[[modelname]]$local_location)){
        util$message(sprintf('Loading Vector Space Matrix from \'%s\' failed, file does not exists.', fdesc))
        return(F)
      }
    }

    # else read vector space matrix as bigmatrix
    message(sprintf('[%s-%d-%s] loading Vector Space Matrix \'%s\'', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), modelname))
    newmodel <- newEmptyObject()
    newmodel$M <- bigmemory::attach.big.matrix(obj = basename(fdesc), path = dirname(fdesc))
    newmodel$vocab <- readLines(gsub('[.]desc$', '.rownames', fdesc))
    assertthat::are_equal(nrow(newmodel$M), length(newmodel$vocab))
    newmodel$name <- modelname
    newmodel$unk <- list(mterm = models[[modelname]]$unk, idx = which(newmodel$vocab == models[[modelname]]$unk))
    newmodel$transform <- function(term) { get_vocab_term(term, models[[modelname]]$transformer, newmodel) }
    add_to_loaded_models(newmodel)

    return(T)
  }

  get_vector <- function(term, modelname, .as_column = F) {
    model <- .models_loaded[[modelname]]
    mterm <- model$transform(term)
    v <- .get_vector(mterm, model)
    return(v)
  }

  .get_vector <- function(mterm, model, .as_column = F) {
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

  similarity <- function(term1, term2, modelname, simfun = senseasim$cos) {
    model <- .models_loaded[[modelname]]
    mterm1 <- model$transform(term1)
    mterm2 <- model$transform(term2)
    v1 <- .get_vector(mterm1, model)
    v2 <- .get_vector(mterm2, model)
    sim <- simfun(v1,v2)
    return(list(
      t1 = mterm1$mterm,
      t2 = mterm2$mterm,
      t1.is.unk = mterm1$idx == model$unk$idx,
      t2.is.unk = mterm2$idx == model$unk$idx,
      sim = sim
    ))
  }

})
