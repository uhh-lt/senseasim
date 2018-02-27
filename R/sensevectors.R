
sensevectors <- new.env(parent = .GlobalEnv)

with(sensevectors, {

  .defaults <- list(
    jbt_sense_api = 'stanfordnew_fine',
    vsm_model = 'EN_100k_lsa',
    topn_sense_terms = 5
  )

  init <- function() {
    vsm$load_default_matrices(c(.defaults$vsm_model))
  }

  get_sense_vectors <- function(term, POS) {
    jb_sense_lists <- Filter(function(l) length(l) > 0, jbt$get_JBT_senses(term, POS, isas = F,  model_template = jbt$.sense_models[[.defaults$jbt_sense_api]], modelname = .defaults$jbt_sense_api))
    message(sprintf('[%s-%d-%s] found %d non-empty senses for term=\'%s#%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), '%m%d-%H%M%S'), length(jb_sense_lists), term, POS))
    vectors <- get_sense_vectors_from_jbtsenseLists(term, POS, jb_sense_lists, .defaults$vsm_model, .defaults$topn_sense_terms)
    return(vectors)
  }

  get_sense_vectors_from_jbtsenseLists <- function(term, POS, jb_sense_lists, vsm_modelname, topn_sense_terms = 1000) {
    # prepare for column names
    term_ <- paste(term, POS, sep='#')
    if(is.null(jb_sense_lists) | length(jb_sense_lists) < 1){
      vectors = matrix(NA, nrow=ncol(vsm$.models_loaded[[vsm_modelname]]$M)) # create a NA valued matrix with one vector and the dim of M)
      colnames(vectors) <- paste0(term_,' ', '_')
      return(vectors)
    }
    colnames_ = c()
    sense_vectors <- sapply(jb_sense_lists, function(list_of_jb_terms) {
      sense_terms <- list_of_jb_terms[1:min(length(list_of_jb_terms), topn_sense_terms)]
      vectors <- get_vectors_from_jbtterms(sense_terms, vsm_modelname)
      # the average vector is the sense vector
      average_vector <- {
        if(is.null(dim(vectors))) # NULL or only one vector
          vectors
        else
          colMeans(vectors) # get_vectors_from_jbtterms return row vectors, this is why we take colMeans here, sapply converts them to column vectors
      }
      # remember the column name

      colname_ <- paste(term_, paste(sense_terms, collapse=','), collapse=' ')
      colnames_ <<- c(colnames_, colname_)
      return(average_vector)
    })
    colnames(sense_vectors) <- colnames_
    return(sense_vectors)
  }

  get_vectors_from_jbtterms <- function(jbtterms, vsm_modelname) {
    model <- vsm$.models_loaded[[vsm_modelname]]
    # clear POS from result terms
    terms <- sapply(jbtterms, function(x) gsub('\\s+','', gsub('#.*','',gsub(':.*','',x)))) # clean terms, either isas (':') or senses ('#') and remove whitespaces
    mterms <- sapply(terms, model$transform) # get the correct term represtentation for the current matrix
    # select the vectors that are in the vs matrix
    covered_mterms_indices <- which(model$vocab %in% mterms)
    # get the submatrix
    if(length(covered_mterms_indices) <= 0){
      # if no known term in the sense cluster use the unknown vector???
      message(sprintf("[%s-%d-%s] sense terms '%s' are unknown.", gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(),format(Sys.time(), "%m%d-%H%M%S"), paste(mterms,collapse = ', ')))
      return(matrix(NA, ncol=ncol(model$M))) # create a NA valued matrix with one vector and the dim of M
    }
    vectors <- model$M[covered_mterms_indices,]
    return(vectors)
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
    vectors <- get_sense_vectors(term, POS)
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
      words <- rio::import(inputfile, sep=' ', fill=T, header=F)
      r <- lapply(1:nrow(words), function(i) {
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
    words <<- rio::import(inputfile, sep=' ', fill=T, header=F)
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
    if(is.null(cl)) {
      cl <- cclDef$local(cores=parallel::detectCores()-1)
    }
    if(is.numeric(cl)){
      if(cl < 1){
        cl = parallel::detectCores()-1
      }
      cl <- cclDef$local(cores=cl)
    }

    init_cluster(cl, inputfile, outputfile)

    # apply in parallel
    r <- parallel::parLapply(cl, 1:nrow(words), function(i) {
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



