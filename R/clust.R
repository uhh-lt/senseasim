#'
#'
#' access jbt models cached data
#'
#'

clust <- new.env(parent = .GlobalEnv)

with(clust, {

  # Some variables for testing ----
  fournodes <- { function() {
    message('Creating 4-nodes sample data. Access with \'clust$fournodes\'.')
    B <- matrix(c(1,1,1,1,1,1,0,1,1,0,1,0,1,1,0,1), nrow = 4)
    rownames(B) <- sapply(1:nrow(B), function(i) paste0('node',i))
    colnames(B) <- rownames(B)
    return(B)
  } }()

  twentynodes <- { function() {
    message('Creating 20-nodes sample data. Access with \'clust$twentynodes\'.')
    B <- matrix(0,20,20)
    A <- matrix(runif(100),10,10)
    B[01:10,01:10] <- A
    B[11:20,11:20] <- A
    rownames(B) <- sapply(1:20, function(i) paste0('node',i))
    colnames(B) <- rownames(B)
    return(B)
  } }()

  twentynodesundirected <- { function() {
    message('Creating 20-undirected-nodes sample data. Access with \'clust$twentynodesundirected\'.')
    A <- twentynodes
    B <- matrix(0, dim(A)[[1]], dim(A)[[2]])
    for(i in 1:dim(A)[[1]]){
      for(j in 1:dim(A)[[2]]){
        B[i,j] <- {
          if(i < j)
            A[i,j]
          else
            A[j,i]
        }
      }
    }
    rownames(B) <- rownames(A)
    colnames(B) <- colnames(A)
    return(B)
  } }()

  # Utility methods ----
  as.cluster.lists <- function(labels){
    cluster_lists <- lapply(unique(labels), function(lbl) which(labels == lbl))
    return(cluster_lists)
  }

  as.cluster.labels <- function(senselists){
    labels <- Reduce(
      f = function(result, i) {
        x <- replicate(length(senselists[[i]]), i)
        names(x) <- senselists[[i]]
        c(result, x)
      },
      x = 1:length(senselists),
      init = c()
    )
    return(labels)
  }

  merge_singleton_clusters <- function(labels){
    quantities <- table(labels)
    labels_with_size_1 <- names(which(quantities == 1))
    labels[which(labels %in% labels_with_size_1)] <- NA
    return(labels)
  }

  # Clustering algorithms ----

  #' Chinese Whispers
  #'
  #' cluster a graph using the chinese whispers graph clustering algorithm
  #'
  #' @param A adjacency matrix
  #'
  #' A <- matrix(runif(25),5,5)  # create random quadratic matrix
  #' A[sample(1:25, 5)] <- 0 # make A sparse
  #' A <- A * t(A) * (1 - diag(5)) # make matrix symmetrical and fix diagonal
  #'
  #' test: clust$chinese_whispers(twenty_nodes)
  #'
  #' @return a vector containing a cluster id for each input node
  chinese_whispers <- function(A, max_iter = 20, eps = 1e-5, remove_self_loops = T, allowsingletons = F){
    # init
    assertthat::are_equal(ncol(A), nrow(A))
    N <- nrow(A)
    # can nodes vote for their own class?
    if(remove_self_loops) {
      diag(A) <- 0
    }
    labels <- seq_len(N)
    labels_new <- labels
    names(labels_new) <- rownames(A)

    for(iter in seq_len(max_iter)){
      for(u in sample(seq_len(N), size = N, replace = F)) {
        # each neighbor votes for its class
        votes <- table(labels[which(A[u,] != 0)])
        if(length(votes) > 1){
          # pick a random class for max vote ties
          max_votes <- which(max(votes) == votes)
          newlabel_u <- as.integer(names(votes)[[sample(max_votes, 1)]])
          labels_new[[u]] <- newlabel_u
        }
      }
      if(sqrt(sum((labels-labels_new)^2)) <= eps) {
        if(!allowsingletons)
          labels_new <- merge_singleton_clusters(labels_new)
        message(sprintf('found %d clusters, took %d iterations.', length(unique(labels_new)), iter))
        return(labels_new)
      }
      labels <- labels_new
    }

    if(!allowsingletons)
      labels_new <- merge_singleton_clusters(labels_new)
    message(sprintf('found %d clusters, took %d iterations.', length(unique(labels_new)), max_iter))
    return(labels_new)
  }

  #'
  #'
  #' @param e expansion exponent (A %^% e)
  #' @param r inflation exponent (A ^ r)
  #' @param ithresh inflation pruning parameter, keep only values equal or greater to ithresh
  #' @param eps epsilon threshold to test for convergence
  #'
  #' test: clust$mcl(twentynodes, addLoops = !remove_self_loops)$Cluster
  #'
  mcl <- function(A, e = 2, r = 2, ithresh = 1e-5, max_iter = 100, eps = 1e-5, remove_self_loops = F, add_dummy = F, allowsingletons = F){
    # init
    assertthat::are_equal(ncol(A), nrow(A))
    N <- nrow(A)

    # add an unconnected dummy node
    if(add_dummy){
      A <- cbind(rbind(A, 0), 0)
      A[N+1, N+1] <- 1
    }

    # remove self loops
    if(remove_self_loops) {
      diag(A) <- 0
    }
    # normalize by column
    margins <- colSums(A)
    A <- t(apply(A, MARGIN = 1, function(row) row / margins))

    # helper function to interpret results
    interpret <- function(){
      # interpret cluster results
      labels <- replicate(N, -1)
      for(i in seq_len(N)){
        votes <- which(A[,i] == max(A[,i]))
        # resolve ties
        if(length(votes) > 1)
          labels[[i]] <- paste0(votes, collapse = '_')
        # sample(length(votes), 1)
        else if(length(votes) < 1)
          labels[[i]] <- -1
        else
          labels[[i]] <- votes
      }

      if (!allowsingletons)
        labels <- merge_singleton_clusters(labels)

      names(labels) <- rownames(A)
      return(labels)
    }

    mpow <- function(M, exponent){
      if(exponent < 2)
        return(M)
      rM <- M
      for(i in 2:exponent)
        rM <- rM %*% M
      return(rM)
    }

    for(iter in seq_len(max_iter)){
      # expand
      newA <- mpow(A, e)
      # inflate
      newA <- newA ^ r
      # prune by threshold but keep the diagonals
      newA[which(newA < ithresh)] <- 0
      margins <- colSums(newA)
      newA <- t(apply(newA, MARGIN = 1, function(row) row / margins))
      # test for convergence
      if(sqrt(sum((A-newA)^2, na.rm = T)) <= eps){
        A <- newA
        labels <- interpret()
        message(sprintf('found %d clusters, took %d iterations.', length(unique(labels)), iter))
        return(labels)
      }
      A <- newA
    }

    # interpret results
    labels <- interpret()
    message(sprintf('found %d clusters, took %d iterations.', length(unique(labels)), max_iter))
    return(labels)
  }

  #'
  #' K-Means clusters row vectors
  #'
  kmeans <- function(A, k = 6, allowsingletons = F){
    result <- stats::kmeans(A, k)
    labels <- result$cluster
    if(!allowsingletons)
      labels <- merge_singleton_clusters(labels)
    message(sprintf('found %d clusters.', length(unique(labels))))
    return(labels)
  }

  #'
  #' Hierichal clustering clusters by dissimilarities
  #'
  #'  dist(A)
  #'
  hclust <- function(A, k=6, h=NULL, allowsingletons = F, isNormalizedSimilarityMatrix = T, debugplot = F){
    # convert similarity matrix into disimilarities, otherwise assume A to be a dissimilarity matrix
    D <- as.dist({ if(isNormalizedSimilarityMatrix) (1-A) else A })
    clusters <- stats::hclust(D)
    if(debugplot)
      plot(clusters)
    labels <- {
      if(!is.null(h))
        cutree(clusters, h = h)
      else
        cutree(clusters, k = k)
    }
    if(!allowsingletons)
      labels <- merge_singleton_clusters(labels)
    message(sprintf('found %d clusters.', length(unique(labels))))
    return(labels)
  }

  #'
  #' (!!!EXPERIMENTAL!!!)
  #' Self Organizing Maps clusters by rows
  #'
  som <- function(A, xdim = 2, ydim = 3, allowsingletons = F){
    #require(kohonen) # for selforganizing maps
    result <- kohonen::som(A, grid=kohonen::somgrid(xdim = xdim, ydim = ydim))
    labels <- result$unit.classif
    names(labels) <- rownames(A)
    if(!allowsingletons)
      labels <- merge_singleton_clusters(labels)
    message(sprintf('found %d clusters.', length(unique(labels))))
    return(labels)
  }

})
