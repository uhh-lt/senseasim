#'
#'
#' Induce senses
#'
#'
#'

wsi <- new.env(parent = .GlobalEnv)

with(wsi, {

  #'
  #' @return a dataframe with similarity values and indexes in modelname
  #'
  vs.similarities <- function(term, modelname, simfun = senseasim$cos, simfun.name = 'cos') {
    message(sprintf('[%s-%d-%s]: Preparing similarity values for term \'%s\' and matrix \'%s\'. ', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), term, modelname))
    model <- vsm$.models_loaded[[modelname]]
    if(is.integer(term)) {
      mterm <- model$vocab[[term]]
      idx <- term
    } else {
      mterm <- model$transform(term)
      idx <- mterm$idx
      mterm <- mterm$mterm
    }

    fname <- cache$get_filename(mterm, '', dirname = cache$data_temp_dir(), prefix = paste0('sim__', modelname, '__', simfun.name, '__'))
    sim <- cache$load(filename = fname, loadfun = function() {
      # get top n most similar words in terms of
      v <- model$M[idx,]
      sim <- sapply(seq_len(nrow(model$M)), function(i) simfun(model$M[i,], v))
      # free some memory
      rm(v)
      # order the result and store the dataframe
      ordr <- order(sim, decreasing = T) # gets the indexes
      sim <- sim[ordr]
      names(sim) <- model$vocab[ordr]
      sim <- as.data.frame(sim)
      sim$idx <- ordr
      return(sim)
    })
    return(sim)
  }

  #'
  #'
  #'
  vs.similarity.matrix <- function(terms, modelname, n = 500, identifier = NULL, simfun = senseasim$cos, simfun.name = 'cos', simfun.issymmetric = T){
    model <- vsm$.models_loaded[[modelname]]
    n <- min(n, nrow(model$M))
    terms <- terms[1:n]
    if(is.integer(terms)) {
      idx <- terms
    } else {
      idx <- sapply(terms, function(term) model$transform(term)$idx)
    }

    if(is.null(identifier)) {
      identifier <- paste0(terms, collapse = ';')
    }
    message(sprintf('[%s-%d-%s]: Preparing similarity matrix of top %s most similar terms for term \'%s\' and matrix \'%s\'. ', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), n, identifier, modelname))

    fname <- cache$get_filename(identifier, '', dirname = cache$data_temp_dir(), prefix = paste0('simmat__', modelname, '__', simfun.name,  '__n', n, '__'))
    SIM <- cache$load(filename = fname, loadfun = function() {
      if(simfun.issymmetric) {
        i <- seq_len(n-1)
        # compute only lower triangular matrix
        SIM <- as.matrix(sapply(i, function(k) { v_k <- model$M[idx[[k]],]; c(rep(NA, k), sapply(seq(k+1,n), function(l) { v_l <- model$M[idx[[l]],]; simfun(v_k, v_l) } )) } ))
        diag(SIM) <- 1 # set diagonal entries to 1
        SIM <- cbind(SIM,rep(1,n)) # add last column vector
        SIM[upper.tri(SIM)] <- t(SIM)[upper.tri(SIM)] # copy lower triangle to upper triangle in the right order!
      }else{
        SIM <- as.matrix(sapply(seq_len(n), function(k) { v_k <- model$M[idx[[k]],]; sapply(seq_len(n), function(l) { v_l <- model$M[idx[[l]],]; simfun(v_k, v_l) } ) } ))
      }
      # set names
      rownames(SIM) <- model$vocab[idx]
      colnames(SIM) <- rownames(SIM)
      return(SIM)
    })
    return(SIM)
  }

  #'
  #' get a similarity graph based on transitivity
  #'
  similarity.graph.transitive <- function(term, modelname, n = 200, m = 50, simfun = senseasim$cos, simfun.name = 'cos'){
    model <- vsm$.models_loaded[[modelname]]
    n <- min(n, nrow(model$M))
    m <- min(m, nrow(model$M))

    mterm <- model$transform(term)
    util$message(sprintf('Preparing similarity graph of top %s most similar terms for term \'%s\' and matrix \'%s\' and expand by the top %s most similar terms.', n, term, modelname, m))
    fname <- cache$get_filename(mterm$mterm, '', dirname = cache$data_temp_dir(), prefix = paste0('simgraphtrans__', modelname, '__', simfun.name,  '__n', n, '__m', m, '__'))

    A <- cache$load(filename = fname, loadfun = function() {
      # get n most simialar terms to mterm
      sim1 <- wsi$vs.similarities(mterm$idx, modelname, simfun = simfun, simfun.name = simfun.name)
      sim1 <- sim1[1:n,]

      # for each similar word compute top m transitive similarities
      simtrans <- lapply( seq_len(n), function(i) wsi$vs.similarities(sim1[i,]$idx, modelname, simfun = simfun, simfun.name = simfun.name)[1:m,] )

      # prepare adjacency matrix, measure intersection of elements in transitive sims
      # TODO: try different values, e.g. average cosine? cosine of average vector?
      A <- matrix(0, nrow = n, ncol =n, dimnames = list(rownames(sim1), rownames(sim1)))
      for(i in seq_len(n)){
        sim_i <- simtrans[[i]]$idx
        for(j in seq_len(n)){
          if(j <= i) {
            if(i == j)
              A[i,j] <- length(sim_i)
            else
              A[i,j] <- A[j,i]
            next
          }
          sim_j <- simtrans[[j]]$idx
          sim_ij <- intersect(sim_i, sim_j)
          A[i,j] <- length(sim_ij)
        }
      }
      return(A)
    })
    return(A)
  }

  #'
  #' induce senses by clustering the similarity matrix
  #'
  induceby.simcluster <- function(term, modelname, topn.similar.terms = 500, simfun = senseasim$cos, simfun.name = 'cos', simfun.issymmetric = T, thresh = 0.66, minsize = 5,cluster.fun = function(X) { clust$cw(X, allowsingletons = F) }, cluster.fun.name = 'cw_nosingletons'){
    model <- vsm$.models_loaded[[modelname]]
    mterm <- model$transform(term)
    fname <- cache$get_filename(mterm$mterm, '', dirname = cache$data_temp_dir(), prefix = paste0('inducedbysimcluster__', modelname, '__', simfun.name,  '__n', topn.similar.terms, '__', thresh, '__', cluster.fun.name, '__'))
    result <- cache$load(filename = fname, loadfun = function() {
      sims <- vs.similarities(mterm$mterm, modelname, simfun = simfun, simfun.name = simfun.name)
      SIM <- vs.similarity.matrix(sims$idx, modelname, n = topn.similar.terms, identifier = mterm$mterm, simfun = simfun, simfun.name = simfun.name, simfun.issymmetric = simfun.issymmetric)
      # SIM is already pruned to top n but term is still in there, so remove it (and it should be the very most similar term!)
      SIM <- SIM[2:topn.similar.terms,2:topn.similar.terms]
      # prune by threshold, i.e. everything below will be set to zero
      SIM[which(SIM < thresh)] <- 0
      labels <- cluster.fun(SIM)
      aslists <- clust$as.cluster.lists(labels)
      result <- list(labels = labels, itemlists = aslists)
      return(result)
    })
    if(is.numeric(minsize) & minsize > 1){
      result$itemlists <- Filter(function(l) length(l) >= minsize, result$itemlists)
    }
    return(result)
  }

})
