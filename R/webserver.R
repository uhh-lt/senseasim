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

  indices <- list()

  R1 <- sensevectors$get_sense_vectors(term = term1, POS = POS1)
  R2 <- sensevectors$get_sense_vectors(term = term2, POS = POS2)
  R1$index$t1 <- T; R1$index$t2 <- F; R2$index$t1 <- F; R2$index$t2 <- T;
  index <- rbind(R1$index, R2$index)
  unique_i <- which(!duplicated(index$idx))

  model <- vsm$.models_loaded[[vsm_modelname]]
  M <- model$M[index$idx[unique_i],]
  M <- matrix(M, nrow=ncol(model$M), dimnames = list(NULL, index$mterm[unique_i]), byrow = T)

  ## 2: run tsne or PCA
  Mred <- embdf_TSNE(M, ndim = 2, normalize_length = T)

  ## 3: plot data
  plot_bulls_eye(Mred)

}

plot_bulls_eye <- function(embdf) {

  # clusters and terms (term1 outer term2 inner)
  embdf_n1 <- as.data.frame(t(apply(embdf[,c('V1','V2')], 1, function(vec) (vec / sqrt(sum(vec^2)))))) * 1.0 # take only first ndim dimensions and normalize vector length
  embdf_n2 <- embdf_n1[,c('V1','V2')] * 0.8

  embdf_n1 <- embdf_n1[names(r$labels1),]
  embdf_n1[, r$term1] <- as.character(r$labels1)
  embdf_n1$rname <- rownames(embdf_n1)
  embdf_n1t <- embdf_n1[r$term1, ]
  embdf_n1 <- embdf_n1[-which(rownames(embdf_n1) == r$term1), ]

  embdf_n2 <- embdf_n2[names(r$labels2),]
  embdf_n2[, r$term2] <- as.character(r$labels2)
  embdf_n2$rname <- rownames(embdf_n2)
  embdf_n2t <- embdf_n2[r$term2, ]
  embdf_n2 <- embdf_n2[-which(rownames(embdf_n2) == r$term2), ]

  # averaged vectors
  embdf_avg <- embdf
  embdf_avg[names(r$labels1),'class1'] <- r$labels1
  embdf_avg[names(r$labels2),'class2'] <- r$labels2

  embdf_avg_1 <- embdf_avg %>% filter(!is.na(class1)) %>% group_by(class1) %>% summarise(V1=mean(V1), V2=mean(V2))
  embdf_avg_2 <- embdf_avg %>% filter(!is.na(class2)) %>% group_by(class2) %>% summarise(V1=mean(V1), V2=mean(V2))

  embdf_navg_1 <- as.data.frame(t(apply(embdf_avg_1[,c('V1','V2')], 1, function(vec) (vec / sqrt(sum(vec^2)))))) * 0.6
  embdf_navg_1s <- as.data.frame(t(apply(embdf_avg_1[,c('V1','V2')], 1, function(vec) ( vec + unlist(embdf[r$term1, c('V1','V2')])) / 2 ))) # shifted average
  embdf_navg_1s <- as.data.frame(t(apply(embdf_navg_1s[,c('V1','V2')], 1, function(vec) (vec / sqrt(sum(vec^2)))))) * 0.6
  embdf_navg_1[,'rname'] <- as.factor(sub('0', '', paste0(r$term1, embdf_avg_1$class1)))
  embdf_navg_1[,'class'] <-  as.factor(embdf_avg_1$class1)
  embdf_navg_1s <- cbind(embdf_navg_1s, embdf_navg_1[,c('rname','class')])

  embdf_navg_2 <- as.data.frame(t(apply(embdf_avg_2[,c('V1','V2')], 1, function(vec) (vec / sqrt(sum(vec^2)))))) * 0.6
  embdf_navg_2s <- as.data.frame(t(apply(embdf_navg_2[,c('V1','V2')], 1, function(vec) ( vec + unlist(embdf[r$term2, c('V1','V2')])) / 2 )))
  embdf_navg_2s <- as.data.frame(t(apply(embdf_navg_2s[,c('V1','V2')], 1, function(vec) (vec / sqrt(sum(vec^2)))))) * 0.6
  embdf_navg_2[,'rname'] <- as.factor(sub('0', '', paste0(r$term2, embdf_avg_2$class2)))
  embdf_navg_2[,'class'] <- as.factor(embdf_avg_2$class2)
  embdf_navg_2s <- cbind(embdf_navg_2s, embdf_navg_2[,c('rname','class')])

  circles <- get_circles(dia = c(1.2, 1.6, 2))

  p <- ggplot() +
    geom_path (data = circles, aes(x = x, y = y, group = lev), colour = 'gray') +
    geom_label(data = embdf_n1, aes_string(x='V1', y='V2', label='rname',fill = r$term1), colour = "black") +
    geom_label(data = embdf_n2, aes_string(x='V1', y='V2', label='rname',fill = r$term2), colour = "white") +
    geom_label_repel(data = embdf_navg_1, aes_string(x='V1', y='V2', label='rname',fill = 'class'), color = 'black') +
    geom_label_repel(data = embdf_navg_2, aes_string(x='V1', y='V2', label='rname', fill = 'class'), color = 'white') +
    geom_segment(data = embdf_navg_1, aes_string(x='0', y='0', xend='V1', yend='V2', color='class'), arrow = arrow(length = unit(0.01, 'npc'))) +
    geom_segment(data = embdf_navg_2, aes_string(x='0', y='0', xend='V1', yend='V2', color='class'),linetype='dashed', arrow = arrow(length = unit(0.01, 'npc'))) +
    geom_hline(yintercept=0, linetype='dashed', color = 'gray') +
    geom_vline(xintercept=0, linetype='dashed', color = 'gray') +
    geom_text(data = embdf_n1t, aes_string(x=0, y=-1, label = 'rname'), color = 'darkgray', fontface='italic', nudge_y = -0.05, size=8, family='sans') +
    geom_text(data = embdf_n2t, aes_string(x=0, y=-0.8, label = 'rname'), color = 'darkgray', fontface='italic', nudge_y = -0.05, size=8, family='sans') +
    guides(colour = guide_legend(override.aes = list(size=6))) +
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
      panel.border     = element_blank(),
      legend.position  = 'none'
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



