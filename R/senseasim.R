
senseasim <- new.env(parent = .GlobalEnv)

with(senseasim, {

  #'
  #' create similarity martix between vectors of the two matrices
  #' compute similarities between each vector in matrix M1 and each other vector in matrix M2
  #' similarities: rows are m1_ vectors, cols are m2_ vectors
  #' SIM[M1_i, M2_j]
  #'
  msim <- function(M1, M2, simfun = cos){

    # columns of both matrices must match!
    assertthat::see_if(assertthat::are_equal(dim(M1)[[2]], dim(M2)[[2]]), msg='Dimension mismatch: cols in M1 unequal to cols in M2!')

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
      SIM <- matrix(SIM, nrow=dim(M1)[2], ncol=dim(M2)[2], byrow = F)
      rownames(SIM) <- colnames(M1)
      colnames(SIM) <- colnames(M2)
    }
    return(SIM)
  }

  maxsim <- function(SIM) {
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
      argmax_sim_names  = matrix(nrow = 1, ncol = 2, data = c( colnames(M1)[argmax_sim_ind[1,1]], colnames(M2)[argmax_sim_ind[1,2]]) )
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


