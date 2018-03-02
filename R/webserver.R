
library(ggplot2)
library(dplyr)
#'
#' R Webserver wrapper
#'
#'
function(){ }

#* redirect to swagger API description
#* @get /
#* @html
function(){
  '
<!DOCTYPE HTML>
<html>
  <head>
    <meta charset="UTF-8">
    <meta http-equiv="refresh" content="1; url=https://goo.gl/forms/7l8OZ9bptiQvBLJ82">
    <script type="text/javascript">window.location.href = "__swagger__/"</script>
    <title>Page Redirection</title>
  <body>If you are not redirected automatically, follow this <a href="__swagger__/">link</a>.
  '
}


#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Get vector
#* @param term The term (default: vitamin)
#* @param model The vector space model to use
#* @serializer contentType list(type="text/plain")
#* @get /vector
function(res, term='vitamin', model=sensevectors$.defaults$vsm_model){
  vsm$load_default_matrices(list(model))
  vec <- vsm$get_vector(term = term, modelname = model, .as_column = F)
  res$body <- paste(paste0(rownames(vec), collapse = ''), paste0(vec, collapse = ' '))
}

#* Get sense vectors
#* @param term The term (default: vitamin)
#* @param POS The part of speech of the term (default: NN)
#* @param RET which information to return (default: index)
#* @get /sensevector
function(res, term='vitamin', POS='NN', RET='index'){
  # @ serializer contentType list(type="application/json")
  # json <- jsonlite::toJSON(list(
  #   index <- vec$index,
  #   vector <- t(vec$v)
  # ))
  # res$body <- json
  sensevectors$init()
  vec <- sensevectors$get_sense_vectors(term, POS)
  message('index')
  switch (RET,
    'status' = vec$status,
    'vector' = vec$v,
    'vectorshift' = vec$v_shift,
    vec$index
  )
}

#* Plot out data from the iris dataset (EXAMPLE FUNCTION)
#* @param spec If provided, filter the data to only this species (e.g. 'setosa')
#* @get /plot
#* @png
function(spec){
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!missing(spec)){
    title <- paste0("Only the '", spec, "' Species")
    myData <- subset(iris, Species == spec)
  }

  plot(myData$Sepal.Length, myData$Petal.Length,
       main=title, xlab="Sepal Length", ylab="Petal Length")
}

#* Plot senses of two terms
#* @param term1
#* @param term2
#* @param POS1
#* @param POS2
#* @get /plotsenses
#* @png
function(term1='iron', term2='vitamin', POS1 = 'NN', POS2 = 'NN', vsm_modelname = sensevectors$.defaults$vsm_model){

  ## 1: get data
  R1 <- sensevectors$get_sense_vectors(term = term1, POS = POS1)
  R2 <- sensevectors$get_sense_vectors(term = term2, POS = POS2)
  R1$index$t1 <- T; R1$index$t2 <- F; R2$index$t1 <- F; R2$index$t2 <- T;
  index <- rbind(R1$index, R2$index)
  unique_i <- which(!duplicated(index$idx))

  model <- vsm$.models_loaded[[vsm_modelname]]
  M <- model$M[index$idx[unique_i],]
  # make column vectors
  M <- matrix(M, nrow=ncol(model$M), dimnames = list(NULL, index$mterm[unique_i]), byrow = T)
  # add sense vectors (shifted + non_shifted), replace colnames by stg like 'iron#1s'
  colnames(R1$v) <- paste0(term1,'#',1:ncol(R1$v))
  colnames(R2$v) <- paste0(term2,'#',1:ncol(R1$v))
  colnames(R1$v_shift) <- paste0(term1,'#',1:ncol(R1$v_shift),'s')
  colnames(R2$v_shift) <- paste0(term2,'#',1:ncol(R2$v_shift),'s')
  M <- cbind(M, R1$v, R1$v_shift, R2$v, R2$v_shift)

  # add description of sense vectors to index
  temp_index <- index[0,] # copy index definition
  temp_index[1:(2*(R1$nsenses+R2$nsenses)),'mterm'] <- c(colnames(R1$v), colnames(R1$v_shift), colnames(R2$v), colnames(R2$v_shift))
  temp_index$t1 <- c(rep(T, R1$nsenses*2), rep(F, R2$nsenses*2))
  temp_index$t2 <- !temp_index$t1
  temp_index$sense <- c(1:R1$nsenses, 1:R1$nsenses, 1:R2$nsenses, 1:R2$nsenses)
  temp_index$is_shifted <- c(rep(F, R1$nsenses), rep(T, R1$nsenses), rep(F, R2$nsenses), rep(T, R2$nsenses))
  index$is_shifted <- NA
  index <- rbind(index, temp_index)

  # prepare some more metadata
  index$is_sense_vector <- is.na(index$idx)
  index$is_sense_vector <- is.na(index$idx)

  ## 2: run tsne or PCA
  set.seed(1)
  num_rows_sample <- 15000
  Mdf <- embdf_TSNE(M, ndim = 2, normalize_length = T)
  index[,c('x','y')] <- Mdf[index$mterm,]

  ## 3: plot data
  plot_bulls_eye(index)

}

plot_bulls_eye <- function(index) {

  # get vectors of t1 and t2
  # (x,y) * 0.8 # scale t2 vectors
  # (x,y) * 0.6 # scale sense vectors, and term vectors
  index$scale[index$t2 & index$sense > 0 & !index$is_sense_vector] <- 0.8
  index$scale[index$sense == 0 | index$is_sense_vector] <- 0.6
  index$scale[is.na(index$scale)] <- 1.0
  index[, c('x','y')] <- index[,c('x','y')] * index$scale

  # prepare colo labels for different senses
  index$usense <- index$sense + 1
  index$usense[index$t2] <- (index$usense[index$t2] + R1$nsenses + 1)

  circles <- get_circles(dia = c(1.2, 1.6, 2))

  p <- ggplot() +
    geom_path (data = circles, aes(x = x, y = y, group = lev), colour = 'gray') + # circles
    geom_label(data = index[index$t1 & index$sense > 0 & !index$is_sense_vector,], aes_string(x='x', y='y', label='mterm', fill = 'usense'), colour = 'black') + # t1 terms
    geom_label(data = index[index$t2 & index$sense > 0 & !index$is_sense_vector,], aes_string(x='x', y='y', label='mterm', fill = 'usense'), colour = 'white') + # t2 terms
    geom_label_repel(data = index[index$t1 & (index$sense == 0 | (index$is_sense_vector & !index$is_shifted)),], aes_string(x='x', y='y', label='mterm', fill = 'usense'), color = 'black') + # t1 sense vectors + original t1 vector
    geom_label_repel(data = index[index$t2 & (index$sense == 0 | (index$is_sense_vector & !index$is_shifted)),], aes_string(x='x', y='y', label='mterm', fill = 'usense'), color = 'white') + # t2 sense vectors + original t1 vector
    geom_segment(data = index[index$t1 & (index$sense == 0 | (index$is_sense_vector & !index$is_shifted)),], aes_string(x='0', y='0', xend='x', yend='y', color='usense'), arrow = arrow(length = unit(0.01, 'npc'))) + # arrows t1
    geom_segment(data = index[index$t2 & (index$sense == 0 | (index$is_sense_vector & !index$is_shifted)),], aes_string(x='0', y='0', xend='x', yend='y', color='usense'), arrow = arrow(length = unit(0.01, 'npc')), linetype='dashed') + # arrows t2
    geom_hline(yintercept=0, linetype='dashed', color = 'gray') + # add a horizontal line
    geom_vline(xintercept=0, linetype='dashed', color = 'gray') + # add a vertical line
    geom_text(data = index[index$t1 & index$sense == 0,], aes_string(x=0, y=-1, label = 'mterm'), color = 'darkgray', fontface='italic', nudge_y = -0.05, size=8, family='sans') + # add term 1 on the outer circle
    geom_text(data = index[index$t2 & index$sense == 0,], aes_string(x=0, y=-0.8, label = 'mterm'), color = 'darkgray', fontface='italic', nudge_y = -0.05, size=8, family='sans') + # add term 2 on the inner circle
    guides(colour = guide_legend(override.aes = list(size=8))) +
    xlab("") + ylab("") +
    theme_light(base_size=20) +
    # theme_classic(base_size=20) +
    theme(
      strip.background = element_blank(),
      strip.text.x     = element_blank(),
      axis.text.x      = element_blank(),
      axis.text.y      = element_blank(),
      axis.ticks       = element_blank(),
      axis.line        = element_blank(),
      panel.border     = element_blank()
      #legend.position  = 'none'
    )
  p

  #ggsave('~/git/senseasim/bullseye.pdf', p, width=20, height=20, units='cm')
  return(p)
}


embdf_TSNE <- function(M, ndim = 2, normalize_length = T) {
  tsne <- Rtsne::Rtsne(
    t(M),
    check_duplicates = FALSE,
    pca = TRUE,
    perplexity=5,
    theta=0.5,
    dims=ndim
  )
  emb <- as.data.frame(tsne$Y)
  if(normalize_length)
    emb <- as.data.frame(t(apply(tsne$Y, 1, function(vec) (vec / sqrt(sum(vec^2)))))) # take only first ndim dimensions and normalize vector length
  rownames(emb) <- colnames(M)
  return(emb)
}

embdf_PCA <- function(M, ndim = 2, normalize_length = T) {
  pca <- prcomp(
    t(M),
    center = TRUE,
    scale. = TRUE
  )
  emb <- as.data.frame(pca$x[,1:ndim])
  if(normalize_length)
    emb <- as.data.frame(t(apply(pca$x[,1:ndim], 1, function(vec) (vec / sqrt(sum(vec^2)))))) # take only first ndim dimensions and normalize vector length
  colnames(emb) <- gsub('PC', 'V',colnames(emb))
  rownames(emb) <- colnames(M)
  return(emb)
}

get_circles <- function(center=c(0,0), dia = c(1.2, 1.6, 2)){
  circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2, filled=FALSE){
    tt <- seq(start*pi, end*pi, length.out=npoints)
    df <- data.frame(
      x = center[1] + diameter / 2 * cos(tt),
      y = center[2] + diameter / 2 * sin(tt)
    )
    if(filled==TRUE) { #add a point at the center so the whole 'pie slice' is filled
      df <- rbind(df, center)
    }
    return(df)
  }
  circlegrid <- data.frame(dia = dia)
  circlegrid <- circlegrid %>%
    mutate(data = lapply(dia, function(x) {
      df     <- circleFun(center = center, diameter = x)
      df$lev <- x
      df
    }))
  circles <- bind_rows(circlegrid$data)
  circles$lev <- as.factor(circles$lev)
  return(circles)
}


#* Log some information about the incoming request
#* @filter logger
function(req){
  message(
    as.character(Sys.time()),
    ' - ',
    req$REQUEST_METHOD,
    ' ',
    req$PATH_INFO,
    ' - ',
    req$HTTP_USER_AGENT,
    ' @ ',
    req$REMOTE_ADDR
  )
  plumber::forward()
}



