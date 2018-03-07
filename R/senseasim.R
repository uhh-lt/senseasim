
senseasim <- new.env(parent = .GlobalEnv)

with(senseasim, {

  #'
  #' score
  #'
  score <- function(term1, term2, POS1 = 'NN', POS2 = 'NN', vsm_modelname = sensevectors$.defaults$vsm_model, senseinventoryname = sensevectors$.defaults$senseinventoryname, topn_sense_terms =  sensevectors$.defaults$topn_sense_terms, shift_lambda = sensevectors$.defaults$shift_lambda, simfun = senseasim$cos) {

    R1 <- sensevectors$get_sense_vectors(term = term1, POS = POS1, vsm_modelname = vsm_modelname, senseinventoryname = senseinventoryname, topn_sense_terms = topn_sense_terms, shift_lambda = shift_lambda)
    R2 <- sensevectors$get_sense_vectors(term = term2, POS = POS2, vsm_modelname = vsm_modelname, senseinventoryname = senseinventoryname, topn_sense_terms = topn_sense_terms, shift_lambda = shift_lambda)
    SIM <- sim.matrix(R1$v_shift, R2$v_shift, simfun = simfun)
    maxscore <- max.sim(SIM)
    return(list(
      t1_info = R1,
      t2_info = R2,
      scores = SIM,
      maxscore = maxscore
    ))

  }

  #'
  #' create similarity martix between vectors of the two matrices
  #' compute similarities between each vector in matrix M1 and each other vector in matrix M2
  #' similarities: rows are M1_ vectors, cols are M2_ vectors
  #' M1 and M2 are expected to be column vectors! I.e. columns are examples, and rows are dimensions
  #' SIM[M1_i, M2_j]
  #'
  sim.matrix <- function(M1, M2, simfun = cos){
    # columns of both matrices must match!
    assertthat::see_if(assertthat::are_equal(nrow(M1), nrow(M2)), msg='Dimension mismatch: rows in M1 unequal to rows in M2!')

    # create the similarity matrix
    SIM <-
      apply(
        M2, 2,
        function(v1) {
          apply(
            M1, 2,
            function(v2){
              simfun(v1, v2)
            }
          )
        }
      )

    # if SIM is not a matrix because M1 matrix was merely a vector, correct it manually!
    if(!is.matrix(SIM)) {
      SIM <- matrix(SIM, nrow=ncol(M1), ncol=ncol(M2), byrow = F)
      rownames(SIM) <- colnames(M1)
      colnames(SIM) <- colnames(M2)
    }
    return(SIM)
  }

  #'
  #' get the arg max similarity value from the similarity matrix
  #'
  max.sim <- function(SIM) {
    argmax_sim <- which.max(SIM)
    if(length(argmax_sim) < 1) {
      # argmax cannot be determined, probably due to NA values
      return(list(
        max_sim           = NA,
        argmax_sim        = 0,
        argmax_sim_i      = matrix(nrow = 1, ncol = 2, data = c(0,0)),
        argmax_sim_names  = matrix(nrow = 1, ncol = 2, data = c('-','-'))
      ))
    }
    # get the (row,col) index
    argmax_sim_ind <- arrayInd(argmax_sim, dim(SIM))
    # best similarity
    max_sim <- SIM[[argmax_sim]]
    # return
    return(list(
      max_sim           = max_sim,
      argmax_sim        = argmax_sim,
      argmax_sim_i      = argmax_sim_ind,
      argmax_sim_names  = matrix(nrow = 1, ncol = 2, data = c( rownames(SIM)[argmax_sim_ind[1,1]], colnames(SIM)[argmax_sim_ind[1,2]]) )
    ))
  }

  #'
  #' define cosine similarity
  #'
  cos <- function(v1,v2){
    co <- sum(v1*v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
    return(co)
  }

  #'
  #' define cosine similarity with normalized length vectors (unit length)
  #'
  #' v1 and v2 can be matrices
  #'
  ncos <- function(v1,v2){
    #co <- sum(v1*v2)
    co <- v1 %*% v2 # dot product
    return(co)
  }

  #'
  #' define euclidean similarity
  #'
  euc <- function(v1,v2){
    dist <- sqrt(sum((v1-v2)^2))
    sim <- 1 / (1+dist)
    return(sim)
  }

  #' #'
  #' #' define sine difference
  #' #'
  #' sin <- function(v1,v2){
  #'   si <- sqrt(sum(pracma::cross(v1,v2)^2)) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
  #'   return(co)
  #' }
  #'
  #' #'
  #' #' cross product function (see https://stackoverflow.com/questions/36798301/r-compute-cross-product-of-vectors-physics#answer-36802067)
  #' #'
  #' xprod <- function(...) {
  #'   args <- list(...)
  #'   # Check for valid arguments
  #'   if (length(args) == 0) {
  #'     stop("No data supplied")
  #'   }
  #'   len <- unique(sapply(args, FUN=length))
  #'   if (length(len) > 1) {
  #'     stop("All vectors must be the same length")
  #'   }
  #'   if (len != length(args) + 1) {
  #'     stop("Must supply N-1 vectors of length N")
  #'   }
  #'   # Compute generalized cross product by taking the determinant of sub-matricies
  #'   m <- do.call(rbind, args)
  #'   sapply(seq_len(len), FUN=function(i) { det(m[,-i,drop=FALSE]) * (-1)^(i+1) })
  #' }

}) # end with(...)


