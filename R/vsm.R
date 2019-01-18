vsm <- new.env(parent = .GlobalEnv)

with(vsm, {

  models <- list()

  .models_loaded <- list()

  .init <- function(){
    models_available <- .models_available()
    models <<- .get_models(T, models_available)
    # names(models) <<- names(models_available)
  }

  .models_available <- function() list(
    en_glove_6B_50d = list(
      lang = 'en',
      init = function() .txt.load_matrix (
        filelocation = paste0(cache$data_dir(),'/glove/glove.6B.50d.txt'),
        unk = 'unknown',
        transformer = function(w) tolower(w)
      ),
      getvector = function(word_or_index) .txt.get_vector(word_or_index, 'en_glove_6B_50d', .as_column=F),
      getterm = function(index) .txt.get_term(index, 'en_glove_6B_50d')
    ),
    en_glove_6B_50d_1K = list(
      lang = 'en',
      init = function() .txt.load_matrix (
        filelocation = paste0(cache$data_dir(),'/glove/glove.6B.50d.1K.txt'),
        unk = 'the',
        transformer = function(w) tolower(w)
      ),
      getvector = function(word_or_index) .txt.get_vector(word_or_index, 'en_glove_6B_50d_1K', .as_column=F),
      getterm = function(index) .txt.get_term(index, 'en_glove_6B_50d_1K')
    )
  )

  .modelnames_for_lang <- function(lang) {
    matching_models <- grep(paste0('^',lang,'_'), names(.models_available()), value=T)
    return(matching_models)
  }

  .get_best_modelname_for_lang <- function(lang) {
    matching_models <- .modelnames_for_lang(lang)
    if(length(matching_models) > 0){
      return(matching_models[[1]])
    }
    return(NULL)
  }

  .txt.build_bigmatrix_from_txt <- function(filename, separator = ' ') {
    # get the filenames
    bckngpath <- dirname(filename)
    bckngfile <- paste0(basename(filename), '.bin')
    bckngdesc <- paste0(bckngfile, '.desc')
    bckngrownames <- file.path(bckngpath, paste0(bckngfile, '.rownames'))
    lockfile <- file.path(bckngpath, paste0(bckngfile, '.lock'))
    lock__ <- flock::lock(lockfile)

    message(sprintf('[%s-%d-%s] Trying to convert Vector Space Matrix: \n  input: \'%s\' \n  path:  \'%s\' \n  bin:   \'%s\'  \n  desc:  \'%s\' ',
                    gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"),
                    filename, bckngpath, bckngfile, bckngdesc))

    if(file.exists(file.path(bckngpath, bckngdesc))) {
      util$message('Descriptor file exists. Skipping.')
      flock::unlock(lock__)
      return(T)
    }

    if(!file.exists(filename)) {
      util$message('Input file does not exist. Aborting.')
      flock::unlock(lock__)
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

    flock::unlock(lock__)
    return(T)
  }

  .txt.load_matrix <- function(filelocation, unk, transformer) {
    # bigmatrix descriptorfile
    fdesc <- paste0(filelocation,'.bin.desc')

    if(!file.exists(fdesc)) {
      if(!.txt.build_bigmatrix_from_txt(filelocation)){
        util$message(sprintf('Loading Vector Space Matrix from \'%s\' failed, file does not exists.', fdesc))
        return(F)
      }
    }

    # else read vector space matrix as bigmatrix
    util$message(sprintf('loading Vector Space Matrix \'%s\'', filelocation))
    newmodel <- newEmptyObject()
    newmodel$M <- bigmemory::attach.big.matrix(obj = basename(fdesc), path = dirname(fdesc))
    newmodel$vocab <- readLines(gsub('[.]desc$', '.rownames', fdesc))
    assertthat::are_equal(nrow(newmodel$M), length(newmodel$vocab))
    newmodel$unk <- list(term = unk, idx = which(newmodel$vocab == unk))
    newmodel$transform <- function(term) { .txt.get_vocabulary_term(term, transformer, newmodel) }
    newmodel$vdim <- ncol(newmodel$M)
    return(newmodel)
  }

  .txt.get_term <- function(idx, modelname) {
    txtmodel <- .models_loaded[[modelname]]
    if(idx < 1 && idx > length(txtmodel$vocab))
      return(NA)
    return(txtmodel$vocab[[idx]])
  }

  .txt.get_vector <- function(term_or_idx, modelname, .as_column = F) {
    model <- .models_loaded[[modelname]]
    if(is.character(term_or_idx))
      mterm <- model$transform(term_or_idx)
    else{
      if(term_or_idx > nrow(model$M) || term_or_idx < 1)
        mterm <- model$unk
      else
        mterm <- list(term = model$term(term_or_idx), idx = term_or_idx)
    }

    # get the vector
    if(length(mterm$idx) > 0)
      v <- matrix(nrow = 1, data = model$M[mterm$idx,], dimnames = list(mterm$term), byrow = T)
    else
      v <- matrix(NA, nrow=1, ncol=ncol(M), dimnames = list(mterm$term)) # create a NA valued matrix with one vector and the dim of M)
    if(.as_column)
      v <- t(v)
    return(v)
  }

  .txt.get_vocabulary_term <- function(term, tfun, model){
    mterm <- tfun(term)
    idx <- which(model$vocab == mterm)
    if(length(idx) > 0) {
      return(list(term = mterm, idx = idx))
    }else{
      return(model$unk)
    }
  }

  .similarity <- function(term1, term2, vsmodel, simfun = senseasim$cos) {
    v1 <- vsmodel$vector(term1)
    v2 <- vsmodel$vector(term2)
    sim <- simfun(v1,v2)
    return(list(
      t1 = rownames(v1)[[1]],
      t2 = rownames(v2)[[1]],
      t1.is.unk = rownames(v1)[[1]] == vsmodel$unk$term,
      t2.is.unk = rownames(v2)[[1]] == vsmodel$unk$term,
      sim = sim
    ))
  }

  .getvectors <- function(vsmodel, ...){
    terms <- list(...)
    if(is.list(terms[[1]]) || is.vector(terms[[1]]))
      terms <- as.list(unlist(terms, recursive = T))
    if (length(terms) > 1)
      vectors <- do.call(rbind, lapply(terms, vsmodel$vector))
    else
      vectors <- vsmodel$vector(terms[[1]])
    return(vectors)
  }

  .getmodel <- function(vsmodelname, vsmodels = .models_available()) {
    if(!(vsmodelname %in% names(.models_loaded))){
      vsmodel <- vsmodels[[vsmodelname]]
      loadedvsmodel <- vsmodel$init()
      loadedvsmodel$lang <- vsmodel$lang
      loadedvsmodel$vector <- vsmodel$getvector
      loadedvsmodel$term <- vsmodel$getterm
      loadedvsmodel$name <- vsmodelname
      # convenience functions
      loadedvsmodel$sim <- function(t1, t2, simfun = senseasim$cos) .similarity(t1, t2, loadedvsmodel, simfun)
      loadedvsmodel$vectors <- function(...) .getvectors(loadedvsmodel, ...)
      # add to list of loaded models
      .models_loaded[[length(.models_loaded)+1]] <<- loadedvsmodel
      names(.models_loaded)[[length(.models_loaded)]] <<- vsmodelname
      return(loadedvsmodel)
    }
    return(.models_loaded[[vsmodelname]])
  }

  .get_models <- function(lazyloading = T, vsmodels = .models_available()) {
    m <- sapply(names(vsmodels), function(vsmodelname) {
      if(!lazyloading)
        vsmodel <- .getmodel(vsmodelname, vsmodels)
      return(function(){
        if(lazyloading)
          vsmodel <- .getmodel(vsmodelname, vsmodels)
        return(vsmodel)
      })
    })
    return(m)
  }

})
