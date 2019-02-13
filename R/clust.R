#'
#'
#' define some clustering algorithms
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
    labels_ <- unique(labels)
    clusterlists <- lapply(labels_, function(lbl) names(labels)[which(labels == lbl)])
    names(clusterlists) <- labels_
    return(clusterlists)
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

  #'
  #' merged singleton clusters
  #'
  merge_singleton_clusters <- function(labels, newlabel = 'singletons.merged'){
    quantities <- table(labels)
    labels_with_size_1 <- names(which(quantities == 1))
    labels[which(labels %in% labels_with_size_1)] <- newlabel
    return(labels)
  }

  #'
  #' prune graph / adjaceny matrix
  #'
  #' prune graph by keeping top outgoing and top incomnig edges
  #'
  graph.prune.inout <- function(A, nkeep=3, make.symmetric = F) {
    # assert ncol(A) == nrow(A)
    # first make sure that selfloops are removed
    diag(A) <- 0
    # reset nkeep to the min(nkeep, len)
    nkeep <- min(nrow(A), nkeep)
    idx <- seq_len(nrow(A))
    maxidx <- sapply(idx, function(i){
      # throw in some randomness (just in case the matrix is too homogenous)
      sampledcolidx <- sample(seq_len(ncol(A)))
      sampledrowidx <- sample(seq_len(nrow(A)))
      c(
        sampledcolidx[order(A[i, sampledcolidx], decreasing=T)[1:nkeep]],
        sampledrowidx[order(A[sampledrowidx, i], decreasing=T)[1:nkeep]]
      )
    })
    rowidx <- c(rep(idx, each=nkeep), c(maxidx[1:nkeep,]))
    colidx <- c(c(maxidx[(nkeep+1):(nkeep*2),]), rep(idx, each=nkeep))
    B <- matrix(0, nrow=nrow(A), ncol=ncol(A))
    rownames(B) <- rownames(A); colnames(B) <- colnames(A)
    B[cbind(rowidx, colidx)] <- A[cbind(rowidx, colidx)]
    if(make.symmetric)
      B <- (B + t(B)) / 2 # make symmetric
    return(B)
  }

  #'
  #' prune graph / adjaceny matrix
  #'
  #' prune graph by keeping top outgoing edges
  #'
  graph.prune <- function(A, nkeep=3, make.symmetric = T) {
    # assert ncol(A) == nrow(A)
    # first make sure that selfloops are removed since they will have the strongest score
    diag(A) <- 0
    # reset nkeep to the min(nkeep, len)
    nkeep <- min(nrow(A), nkeep)
    idx <- seq_len(nrow(A))
    rowidx <- c(sapply(idx, function(i){
      # throw in some randomness (just in case the matrix is too homogenous)
      sampledcolidx <- sample(seq_len(ncol(A)))
      sampledcolidx[order(A[i, sampledcolidx], decreasing=T)[1:nkeep]]
    }))
    colidx <- rep(idx, each=nkeep)
    B <- matrix(0, nrow=nrow(A), ncol=ncol(A))
    rownames(B) <- rownames(A); colnames(B) <- colnames(A)
    B[cbind(rowidx, colidx)] <- A[cbind(rowidx, colidx)]
    if(make.symmetric)
      B <- (B + t(B)) / 2 # make symmetric
    return(B)
  }

  #'
  #' visualize graph / adjaceny matrix
  #'
  graph.viz <- function(A, labels=NULL, labels.as.list=T, label.in.name=T, tkplot=F) {
    # ensure that weights are between 0 and 1
    miA <- min(A)
    if(miA < 0) # shift if needed
      A <- A - miA
    maA <- max(A)
    if(maA > 1)  # scale if needed
      A <- A / maA
    # produce undirected weighted graph
    net <- igraph::graph_from_adjacency_matrix(A, mode = 'undirected', weighted = T)
    igraph::V(net)$color <- 'lightgray'
    if(!is.null(labels)){
      if(labels.as.list){
        labels <- as.cluster.labels(labels)
      }

      labelfactor <- factor(labels)
      labelfactorid <- as.numeric(labelfactor)

      # vertex properties
      igraph::V(net)[names(labels)]$color <- c('lightgray', 'yellow', 'magenta', 'red', 'green', 'white', 'cyan', 'lightblue')[(labelfactorid %% 8)+1]
      igraph::V(net)[names(labels)]$frame.color <- c('black', 'red', 'green', 'blue', 'yellow', 'white')[((labelfactorid %/% 8) %% 6)+1]
      #vertices$size <- 14

      # vertex labels and label properties
      igraph::V(net)[names(labels)]$label <- if(label.in.name) paste(names(labels), labelfactorid, sep='##') else names(vertices)
      igraph::V(net)[names(labels)]$label.color <- c('black', 'red', 'green', 'blue', 'white')[((labelfactorid %/% 48) %% 5)+1]
      # igraph::V(net)$label.font <- 1   # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
      # igraph::V(net)$label.cex <- 1  # Font size (multiplication factor, device-dependent)
      # igraph::V(net)$label.dist <- 0 # Distance between the label and the vertex
      # igraph::V(net)$label.degree <- 0  # The position of the label in relation to the vertex (use pi)
      # igraph::V(net)$label.family <- 'Times'  # Font family of the label (e.g. 'Times', 'Helvetica')
      # igraph::V(net)[names(labels)]$shape <- c('circle', 'square')[((labelfactorid %/% 240) %% 2)+1] # 'csquare', 'rectangle', 'crectangle', 'vrectangle', 'pie', 'raster', 'sphere', 'none'

      # edge properties
      igraph::E(net)$color <- 'gray'
      igraph::E(net)$lty <- 'solid'   # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
      igraph::E(net)$curved <- 0.3 # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
      igraph::E(net)$width <- igraph::E(net)$weight
    }

    if(tkplot){
      tkid <- igraph::tkplot(net)
    }else{
      p <- igraph::plot.igraph(net)
    }

    return(list(A=A, labels=labels))
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
  #' test: clust$cw(twenty_nodes)
  #'
  #' @return a vector containing a cluster id for each input node
  cw <- function(A, max_iter = 20, eps = 1e-5, remove_self_loops = T, allowsingletons = F){
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
