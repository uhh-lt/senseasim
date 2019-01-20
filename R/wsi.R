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
  vs.similarities <- function(term_or_idx, vsmodel, simfun = senseasim$cos, simfun.name = 'cos') {
    util$message(sprintf('Preparing similarity values for term \'%s\' and matrix \'%s\'. ', term_or_idx, vsmodel$name))

    fname <- cache$get_filename(term_or_idx, '', dirname = cache$data_temp_dir(), prefix = paste0('sim__', vsmodel$name, '__', simfun.name, '__'))
    sim <- cache$load(filename = fname, computefun = function() {
      # get top n most similar words in terms of
      v <- vsmodel$vector(term_or_idx)
      sim <- sapply(seq_len(length(vsmodel$vocab)), function(i) simfun(vsmodel$vector(i), v))
      # order the result and store the dataframe
      ordr <- order(sim, decreasing = T) # gets the indexes
      sim <- sim[ordr]
      names(sim) <- lapply(ordr, vsmodel$term)
      sim <- as.data.frame(sim)
      sim$idx <- ordr
      return(sim)
    })
    return(sim)
  }

  #'
  #'
  #'
  vs.similarity.matrix <- function(terms, vsmodel, n = 500, identifier = NULL, simfun = senseasim$cos, simfun.name = 'cos', simfun.issymmetric = T){
    n <- min(n, if (is.array(terms) || is.data.frame(terms)) nrow(terms) else length(terms))
    terms <- terms[1:n]

    if(is.null(identifier))
      identifier <- digest::digest(terms)

    util$message(sprintf('Preparing similarity matrix of %s terms for \'%s\' based on matrix \'%s\'. ', n, identifier, vsmodel$name))

    fname <- cache$get_filename(identifier, '', dirname = cache$data_temp_dir(), prefix = paste0('simmat__', vsmodel$name, '__', simfun.name,  '__n', n, '__'))
    M <- vsmodel$vectors(terms)
    SIM <- cache$load(filename = fname, computefun = function() {
      if(simfun.issymmetric) {
        i <- seq_len(n-1)
        # compute only lower triangular matrix
        SIM <- as.matrix(sapply(i, function(k) { v_k <- M[k,]; c(rep(NA, k), sapply(seq(k+1,n), function(l) { v_l <- M[l,]; simfun(v_k, v_l) } )) } ))
        diag(SIM) <- 1 # set diagonal entries to 1
        SIM <- cbind(SIM,rep(1,n)) # add last column vector
        SIM[upper.tri(SIM)] <- t(SIM)[upper.tri(SIM)] # copy lower triangle to upper triangle in the right order!
      }else{
        SIM <- as.matrix(sapply(seq_len(n), function(k) { v_k <- M[k,]; sapply(seq_len(n), function(l) { v_l <- M[l,]; simfun(v_k, v_l) } ) } ))
      }
      # set names
      rownames(SIM) <- rownames(M)
      colnames(SIM) <- rownames(SIM)
      return(SIM)
    })
    return(SIM)
  }

  #'
  #' get a similarity graph based on transitivity
  #'
  similarity.graph.transitive <- function(term, vsmodel, n = 200, m = 50, simfun = senseasim$cos, simfun.name = 'cos') {
    n <- min(n, length(vsmodel$vocab))
    m <- min(m, length(vsmodel$vocab))

    util$message(sprintf('Preparing similarity graph of top %s most similar terms for term \'%s\' and matrix \'%s\' and expand by the top %s most similar terms.', n, term, vsmodel$name, m))
    fname <- cache$get_filename(term, '', dirname = cache$data_temp_dir(), prefix = paste0('simgraphtrans__', vsmodel$name, '__', simfun.name,  '__n', n, '__m', m, '__'))

    A <- cache$load(filename = fname, computefun = function() {
      # get n most simialar terms to term
      sim1 <- wsi$vs.similarities(term, vsmodel, simfun = simfun, simfun.name = simfun.name)
      sim1 <- sim1[1:n,]

      # for each similar word compute top m transitive similarities
      simtrans <- lapply( seq_len(n), function(i) wsi$vs.similarities(sim1[i,]$idx, vsmodel, simfun = simfun, simfun.name = simfun.name)[1:m,] )

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
  induceby.simcluster.jbt <- function(term, POS, jbtmodel, vsmodel, topn.similar.terms = 500, simfun = senseasim$cos, simfun.name = 'cos', simfun.issymmetric = T, thresh = 0.66, minsize = 5, cluster.fun = function(X) { clust$cw(X, allowsingletons = F) }, cluster.fun.name = 'cw_nosingletons'){
    fname <- cache$get_filename(term, POS, dirname = cache$data_temp_dir(), prefix = paste0('inducedbysimclusterjbt__', jbtmodel$name, '__', vsmodel$name, '__', simfun.name,  '__n', topn.similar.terms, '__', thresh, '__', cluster.fun.name, '__'))
    result <- cache$load(filename = fname, computefun = function() {
      sims <- jbtmodel$sim(term=term, POS=POS)
      result <- induceby.simcluster.terms(terms=sims$term, vsmodel=vsmodel, simfun=simfun, simfun.name=simfun.name, simfun.issymmetric=simfun.issymmetric, thresh=thresh, minsize=minsize, cluster.fun=cluster.fun, cluster.fun.name=cluster.fun.name)
      return(result)
    })
    if(is.numeric(minsize) & minsize > 1){
      result$itemlists <- Filter(function(l) length(l) >= minsize, result$itemlists)
    }
    return(result)
  }

  #'
  #' induce senses by clustering the similarity matrix
  #'
  induceby.simcluster.vsm <- function(term, vsmodel, topn.similar.terms = 500, simfun = senseasim$cos, simfun.name = 'cos', simfun.issymmetric = T, thresh = 0.66, minsize = 5,cluster.fun = function(X) { clust$cw(X, allowsingletons = F) }, cluster.fun.name = 'cw_nosingletons'){
    fname <- cache$get_filename(term, '', dirname = cache$data_temp_dir(), prefix = paste0('inducedbysimclustervsm__', vsmodel$name, '__', simfun.name,  '__n', topn.similar.terms, '__', thresh, '__', cluster.fun.name, '__'))
    result <- cache$load(filename = fname, computefun = function() {
      sims <- vs.similarities(term, vsmodel, simfun = simfun, simfun.name = simfun.name)
      sims <- sims[1:topn.similar.terms,]
      result <- induceby.simcluster.terms(terms=rownames(sims), vsmodel=vsmodel, simfun=simfun, simfun.name=simfun.name, simfun.issymmetric=simfun.issymmetric, thresh=thresh, minsize=minsize, cluster.fun=cluster.fun, cluster.fun.name=cluster.fun.name)
      return(result)
    })
    if(is.numeric(minsize) & minsize > 1){
      result$itemlists <- Filter(function(l) length(l) >= minsize, result$itemlists)
    }
    return(result)
  }


  #'
  #' induce senses by clustering the similarity matrix of 'terms'
  #'
  induceby.simcluster.terms <- function(terms, vsmodel, simfun = senseasim$cos, simfun.name = 'cos', simfun.issymmetric = T, thresh = 0.66, minsize = 5, cluster.fun = function(X) { clust$cw(X, allowsingletons = F) }, cluster.fun.name = 'cw_nosingletons'){
    desc <- digest::digest(terms)
    fname <- cache$get_filename(desc, '', dirname = cache$data_temp_dir(), prefix = paste0('inducedbysimclusterterms__', vsmodel$name, '__', simfun.name, '__', thresh, '__', cluster.fun.name, '__'))
    result <- cache$load(filename = fname, computefun = function() {
      n <- if (is.array(terms) || is.data.frame(terms)) nrow(terms) else length(terms)
      SIM <- vs.similarity.matrix(terms, vsmodel, n = n, identifier = desc, simfun = simfun, simfun.name = simfun.name, simfun.issymmetric = simfun.issymmetric)
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
