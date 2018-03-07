
sensevectors <- new.env(parent = .GlobalEnv)

with(sensevectors, {

  .defaults <- list(
    vsm_model = 'EN_100k_lsa',
    topn_sense_terms = 5,
    shift_lambda = .5,
    senseinventoryname = 'jbt_stanfordnewfine'
  )

  .senseinventories <- list(
    #
    jbt_stanfordnewfine = function(term, POS)
      jbt$get_JBT_senses(term, POS,
                         isas = F,
                         modelname = 'stanfordnew_fine'),
    #
    sim500cluster_cw = function(term, POS)
      wsi$induceby.simcluster(term,
        modelname = .defaults$vsm_model,
        topn.similar.terms = 500,
        simfun = senseasim$cos,
        simfun.name = 'cos',
        simfun.issymmetric = T,
        thresh = 0.66,
        cluster.fun = function(X) { clust$cw(X, allowsingletons = F) },
        cluster.fun.name = 'cw_nosingletons')$itemlists
    #
  )

  init <- function() {
    vsm$load_default_matrices(c(.defaults$vsm_model))
  }

  get_sense_vectors <- function(term, POS, vsm_modelname = .defaults$vsm_model, senseinventoryname = .defaults$senseinventoryname, topn_sense_terms = .defaults$topn_sense_terms, shift_lambda = .defaults$shift_lambda) {
    # prepare the result object
    R <- newEmptyObject()
    R$params <- as.list(match.call())
    R$status <- list()

    model <- vsm$.models_loaded[[vsm_modelname]]
    # prepare backup return values
    R$v <- matrix(NA, ncol = 1, nrow = ncol(model$M), dimnames = list(NULL, paste0(term,'#')))
    R$v_shift <- R$v

    # get the sense lists
    senseinventoryfun <- .senseinventories[[senseinventoryname]]
    R$termSenseInventory <- senseinventoryfun(term, POS)
    R$nsenses <- length(R$termSenseInventory)
    R$status[[length(R$status)+1]] <- sprintf('found %d non-empty senses for term=\'%s#%s\'', R$nsenses, term, POS)
    message(sprintf('[%s-%d-%s] %s.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), R$status[[length(R$status)]]))

    # make a proper index as dataframe, where we can e.g. select all entries from sense 2 with R$index[which(R$index$sense == 2),]
    mterm <- model$transform(term)
    mterm$original <- term
    mterm$original_clean <- term
    mterm$sense <- 0
    mterm$unknown <- mterm$idx == model$unk$idx
    R$index <- data.frame(mterm, stringsAsFactors = F)

    # get the sub-matrix which contains the term vectors of all terms within the topn of the sense lists
    if(is.null(R$termSenseInventory) | length(R$termSenseInventory) < 1) {
      # if sense inventory is empty make a warning
      R$status[[length(R$status)+1]] <- 'Attention: sense inventory is empty.'
      message(sprintf('[%s-%d-%s] %s.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), R$status[[length(R$status)]]))
    }

    for(i in seq_along(R$termSenseInventory)) {
      list_of_jb_terms <- R$termSenseInventory[[i]]
      sense_terms <- list_of_jb_terms[1:min(length(list_of_jb_terms), topn_sense_terms)]
      # get the correct term representation for the current matrix
      for(sense_term in sense_terms) {
        sense_term_clean <- gsub('\\s+','', gsub('#.*','',gsub(':.*','',sense_term))) # clean terms, either isas (':') or senses ('#'), clear POS and remove whitespaces
        mterm <- model$transform(sense_term_clean)
        mterm$original <- sense_term
        mterm$original_clean <- sense_term_clean
        mterm$sense <- i
        mterm$unknown <- mterm$idx == model$unk$idx
        R$index <- rbind(R$index, data.frame(mterm, stringsAsFactors = F))
      }
    }

    # get unique term indices
    R$unique_i <- which(!duplicated(R$index$idx))
    uniqueindex <- R$index[R$unique_i,]
    uniqueindex <- uniqueindex[!uniqueindex$unknown,]

    # if mapped sense inventory is empty we can't return anything
    if(length(uniqueindex) <= 0) {
      R$status[[length(R$status)+1]] <- sprintf('No known sense terms for \'%s %s\'. Skip processing.', term, POS)
      message(sprintf('[%s-%d-%s] %s.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), R$status[[length(R$status)]]))
      return(R)
    }

    # get the submatrix (note: M contains row vectors)
    M <- model$M[uniqueindex$idx,]
    M <- matrix(M, nrow=ncol(model$M), dimnames = list(NULL, uniqueindex$mterm), byrow = T)

    # get the submatrices for each sense and produce the sense vectors
    R$v <- matrix(ncol = 0, nrow = nrow(M))
    R$v_shift <- matrix(ncol = 0, nrow = nrow(M))
    for(i in seq_len(R$nsenses)){
      # get the average vector
      sterms <- unique(R$index$mterm[which(R$index$sense == i & !R$index$unknown)])
      if(length(sterms) <= 0) {
        R$status[[length(R$status)+1]] <- sprintf('No known sense terms for sense %d of \'%s %s\'. Producing NA vector.', i, term, POS)
        message(sprintf('[%s-%d-%s] %s.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), R$status[[length(R$status)]]))
        s <- matrix(NA, ncol=1, nrow=ncol(model$M)) # create a NA valued matrix with one vector and the dim of M
      } else {
        s <- matrix(ncol=1, rowMeans(M[,sterms,drop=F]), byrow = T)
      }
      # now shift
      if(shift_lambda <= 0){
        # term vector has no influence
        s_shift <- s
      }else{
        # get the term vector
        v <- M[,1,drop=F]
        if(shift_lambda >= 1) {
          # sense vector has no influence, replicate v
          s_shift <- v
        }else{
          # otherwise apply the shift
          s_shift <- (shift_lambda * v) + ((1-shift_lambda) * s)
        }
      }
      colnames(s) <- colnames(s_shift) <- paste0(term, '#', paste0(sterms, collapse = ','))
      R$v <- cbind(R$v, s)
      R$v_shift <- cbind(R$v_shift, s_shift)
    }

    return(R)
  }

  write_vectors_txt <- function(vectors, f=NULL){
    if(is.null(f)){
      f <- stdout()
    }
    # lock
    # lockfile <- if(is.character(f)) paste0(f, '.lock') else '~/stdout.lock'
    # lck = flock::lock(lockfile)
    for(name in colnames(vectors)){
      cat(name, paste(vectors[,name], collapse=' '), '\n', file = f, fill = FALSE, append=TRUE)
    }
    # release lock
    # flock::unlock(lck)
  }

  get_and_write_sensevectors <- function(term, POS, fout) {
    vectors <- get_sense_vectors(term, POS)$v_shift
    write_vectors_txt(vectors, fout)
  }

  read_stdin <- function( lfun ) {
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
    init()
    if(is.character(inputfile)) {
      words <- data.table::fread(inputfile, sep=' ', header=F, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
      r <- lapply(seq_len(nrow(words)), function(i) {
        term <- words[i,1]
        POS <- words[i,2]
        get_and_write_sensevectors(term, POS, outputfile)
        return(T)
      })
    }
    else {
      read_stdin(function(line) {
        row = strsplit(line,'\\s+',fixed = F)[[1]]
        term <- row[1]
        POS <- row[2]
        get_and_write_sensevectors(term, POS, outputfile)
      })
    }
  }

  init_cluster <- function(cl, inputfile, outputfile) {
    words <<- data.table::fread(inputfile, sep=' ', header=F, stringsAsFactors=F, check.names=F, encoding='UTF-8', data.table=F, quote="")
    parallel::clusterExport(cl, c('words','sensevectors'), envir = .GlobalEnv)
    parallel::clusterExport(cl, c('outputfile'), envir = environment())

    parallel::clusterEvalQ(cl, {
      # initialization actions
      local_outputfile <<- paste0(outputfile, Sys.getpid())
      sensevectors$init()
      message(sprintf('[%s-%d-%s] saving to \'%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), local_outputfile))
    })
  }

  run_parallel <- function(inputfile, outputfile, cl = NULL) {
    # measure computing time
    tictoc::tic()

    # if cluster is null create a cluster of n-1 cores of n beeing the system core number
    cl <- if(is.null(cl)) { cclDef$make.default() } else{ cclDef$make.default(cl) }

    init_cluster(cl, inputfile, outputfile)

    # apply in parallel
    r <- parallel::parLapply(cl, seq_len(nrow(words)), function(i) {
      term <- words[i,1]
      POS <- words[i,2]
      get_and_write_sensevectors(term, POS, local_outputfile)
      return(T)
    })

    # shutdown cluster
    message(sprintf('[%s-%d-%s] shutting down cluster.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S')))
    parallel::stopCluster(cl)

    tictoc::toc()
  }

})



