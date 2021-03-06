
sensevectors <- new.env(parent = .GlobalEnv)

with(sensevectors, {

  .INITIALIZED <- F
  .init <- function(reinitialize = F) {
    if(!.INITIALIZED || reinitialize){
      vsm$.init()
      inventory$.init()
      .INITIALIZED <- T
    }
  }

  get_sense_vectors <- function(term, POS, vsmodel, senseinventory, topn_sense_terms = 5, shift_lambda = .5, simfun = senseasim$cos, simweight = F) {
    # prepare the result object
    R <- newEmptyObject()
    R$status <- list()

    if(is.character(vsmodel))
      vsmodel <- vsm$models[[vsmodel]]()
    if(is.character(senseinventory))
      senseinventory <- inventory$models[[senseinventory]]()

    # prepare backup return values
    R$v <- matrix(NA, ncol = 1, nrow = vsmodel$vdim, dimnames = list(NULL, paste0(term,'#')))
    R$v_shift <- R$v

    # get the sense lists
    R$termSenseInventory <- senseinventory$senses(term, POS)
    R$nsenses <- length(R$termSenseInventory)
    R$status[[length(R$status)+1]] <- sprintf('found %d non-empty senses for term=\'%s#%s\'', R$nsenses, term, POS)
    util$message(R$status[[length(R$status)]])

    # consistency check
    if(is.null(R$termSenseInventory) | length(R$termSenseInventory) < 1) {
      # if sense inventory is empty issue a warning
      R$status[[length(R$status)+1]] <- 'Attention: sense inventory is empty.'
      util$message(R$status[[length(R$status)]])
    }

    # make a proper index as dataframe, where we can e.g. select all entries from sense 2 with R$index[which(R$index$sense == 2),]
    mterm <- vsmodel$transform(term)
    mterm$original <- term
    mterm$original_clean <- term
    mterm$sense <- 0
    mterm$unknown <- (mterm$term == vsmodel$unk$term)
    R$index <- data.frame(mterm, stringsAsFactors = F)

    # consistency check
    if(mterm$unknown) {
      # if the term is unknown to the vs model issue a warning
      R$status[[length(R$status)+1]] <- sprintf('Attention: term vector for \'%s\' is unknown.', term)
      util$message(R$status[[length(R$status)]])
    }

    for(i in seq_along(R$termSenseInventory)) {
      list_of_sense_terms <- R$termSenseInventory[[i]]
      sense_terms <- list_of_sense_terms[1:min(length(list_of_sense_terms), topn_sense_terms)]
      # get the correct term representation for the current matrix
      for(sense_term in sense_terms) {
        sense_term_clean <- gsub('\\s+','', gsub('#.*','',gsub(':.*','',sense_term))) # clean terms, either isas (':') or senses ('#'), clear POS and remove whitespaces
        mterm <- vsmodel$transform(sense_term_clean)
        if(nchar(mterm$term) < 1)
          next()
        mterm$original <- sense_term
        mterm$original_clean <- sense_term_clean
        mterm$sense <- i
        mterm$unknown <- (mterm$term == vsmodel$unk$term)
        R$index <- rbind(R$index, data.frame(mterm, stringsAsFactors = F))
      }
    }

    # get unique terms
    R$unique_i <- which(!duplicated(R$index$idx))
    uniqueindex <- R$index[R$unique_i,]
    uniqueindex_ <- uniqueindex[!(uniqueindex$unknown | uniqueindex$sense == 0),] # remove unknowns and the term itself

    # if processed sense inventory is empty we can't return anything
    if(length(uniqueindex_) <= 0) {
      R$status[[length(R$status)+1]] <- sprintf('No known sense terms for \'%s %s\'. Skip processing.', term, POS)
      util$message(R$status[[length(R$status)]])
      return(R)
    }

    # get the vectors as submatrix
    M <- vsmodel$vectors(uniqueindex$term)
    if(simweight){
      # get the individual similarities as vector
      sims <- sapply(seq_len(nrow(M)), function(i) simfun(M[1,], M[i,]) )
      # weight the vectors with their cosine sim
      M <- M * sims
    }

    # get the submatrices for each sense and produce the sense vectors
    R$v <- matrix(nrow = 0, ncol = vsmodel$vdim)
    R$v_shift <- matrix(nrow = 0, ncol = vsmodel$vdim)
    for(i in seq_len(R$nsenses)) { # (seq_len ignores sense 0, which is the term itself)
      # get the average vector
      sense_terms <- unique(R$index$term[which(R$index$sense == i & !R$index$unknown)])
      if(length(sense_terms) <= 0) {
        R$status[[length(R$status)+1]] <- sprintf('No known sense terms for sense %d of \'%s %s\'. Cannot produce sensevector... skipping.', i, term, POS)
        util$message(R$status[[length(R$status)]])
        next
      }
      s <- matrix(nrow=1, colMeans(M[sense_terms,,drop=F]))
      # now shift
      if(shift_lambda <= 0){
        s_shift <- s # term vector has no influence
      }else{
        v <- M[R$index$term[[1]],,drop=F] # get the term vector
        if(shift_lambda >= 1)
          s_shift <- v # sense vector has no influence, replicate v
        else
          s_shift <- (shift_lambda * v) + ((1-shift_lambda) * s) # otherwise apply the shift
      }
      rownames(s) <- rownames(s_shift) <- paste0(term, '#', i, ':', paste0(sense_terms, collapse = ','))
      R$v <- rbind(R$v, s)
      R$v_shift <- rbind(R$v_shift, s_shift)
    }
    return(R)
  }

  io.write_vectors_txt <- function(vectors, f=NULL){
    if(is.null(f)){
      f <- stdout()
    }
    # lock
    lockfile <- if(is.character(f)) paste0(f, '.lock') else '~/stdout.lock'
    lck = flock::lock(lockfile)
    for(name in rownames(vectors)){
      cat(name, paste(vectors[name,], collapse=' '), '\n', file = f, fill = FALSE, append=TRUE)
    }
    # release lock
    flock::unlock(lck)
  }

  io.get_and_write_sensevectors <- function(term, POS, fout) {
    vectors <- get_sense_vectors(term, POS)$v_shift
    io.write_vectors_txt(vectors, fout)
  }

  io.read_stdin <- function( lfun ) {
    input <- file('stdin', 'r')
    while(TRUE) {
      row <- readLines(input, n=1)
      if ( length(row) <= 0 ) {
        break
      }
      lfun(row)
    }
  }

  run <- function(inputfile=NULL, outputfile=NULL){
    .init()
    if(is.character(inputfile)) {
      words <- data.table::fread(inputfile, sep=' ', header=F, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
      r <- lapply(seq_len(nrow(words)), function(i) {
        term <- words[i,1]
        POS <- words[i,2]
        io.get_and_write_sensevectors(term, POS, outputfile)
        return(T)
      })
    }
    else {
      io.read_stdin(function(line) {
        row = strsplit(line,'\\s+',fixed = F)[[1]]
        term <- row[1]
        POS <- row[2]
        io.get_and_write_sensevectors(term, POS, outputfile)
      })
    }
  }

  .cluster.init <- function(cl, inputfile, outputfile) {
    words <<- data.table::fread(inputfile, sep=' ', header=F, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
    parallel::clusterExport(cl, c('words'), envir = .GlobalEnv)
    parallel::clusterExport(cl, c('outputfile'), envir = environment())

    parallel::clusterEvalQ(cl, {
      library(senseasim)
      # initialization actions
      local_outputfile <<- paste0(outputfile, Sys.getpid())
      sensevectors$.init()
      util$message(sprintf('saving to \'%s\'.', local_outputfile))
    })
  }

  cluster.run <- function(inputfile, outputfile, cl = NULL) {
    # measure computing time
    tictoc::tic()

    # if cluster is null create a cluster of n-1 cores of n beeing the system core number
    cl <- if(is.null(cl)) { cclDef$make.default() } else{ cclDef$make.default(cl) }

    outputfile <- paste0(outputfile, format(Sys.time(), '%Y%m%d%H%M%S'))
    .cluster.init(cl, inputfile, outputfile)

    # apply in parallel
    r <- parallel::parLapply(cl, seq_len(nrow(words)), function(i) {
      term <- words[i,1]
      POS <- words[i,2]
      io.get_and_write_sensevectors(term, POS, local_outputfile)
      return(T)
    })

    # shutdown cluster
    util$message('shutting down cluster.')
    parallel::stopCluster(cl)

    tictoc::toc()
  }

})
