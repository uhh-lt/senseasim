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
function(term1='iron', term2='vitamin', POS1 = 'NN', POS2 = 'NN'){

  ## 1: get data

  indices <- list()

  R1 <- sensevectors$get_sense_vectors(term = term1, POS = POS1)
  R2 <- sensevectors$get_sense_vectors(term = term2, POS = POS2)

  M <- cbind(V, S1, S2)

  ## 2: run tsne or PCA
  Mred <- embdf_TSNE(M, ndim = T, normalize_length = T)

  ## 3: plot data
  embdf$class <- r$labels
  embdf[names(r$labels1), r$term1] <- as.character(r$labels1)
  embdf[names(r$labels2), r$term2] <- as.character(r$labels2)
  p1 <- ggplot(embdf, aes(x=V1, y=V2, label=rownames(embdf))) +
    geom_label(aes_string(fill = r$term1), colour = "white") +
    guides(colour = guide_legend(override.aes = list(size=6))) +
    xlab("") + ylab("") +
    theme_light(base_size=20) +
    theme(
      strip.background = element_blank(),
      strip.text.x     = element_blank(),
      axis.ticks       = element_blank(),
      axis.line        = element_blank(),
      panel.border     = element_blank()
    )

  p2 <- ggplot(embdf, aes(x=V1, y=V2, label=rownames(embdf))) +
    geom_label(aes_string(fill = r$term2), colour = "white") +
    guides(colour = guide_legend(override.aes = list(size=6))) +
    xlab("") + ylab("") +
    theme_light(base_size=20) +
    theme(
      strip.background = element_blank(),
      strip.text.x     = element_blank(),
      axis.ticks       = element_blank(),
      axis.line        = element_blank(),
      panel.border     = element_blank()
    )

  p <- multiplot(p1,p2,cols = 2)


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
  rownames(emb) <- NULL
  if(normalize_length)
    emb <- as.data.frame(t(apply(tsne$Y, 1, function(vec) (vec / sqrt(sum(vec^2)))))) # take only first ndim dimensions and normalize vector length
  return(emb)
}

embdf_PCA <- function(M, ndim = 2, normalize_length = T) {
  pca <- prcomp(
    t(M),
    center = TRUE,
    scale. = TRUE
  )
  emb <- as.data.frame(pca$x[,1:ndim])
  rownames(emb) <- NULL
  if(normalize_length)
    emb <- as.data.frame(t(apply(pca$x[,1:ndim], 1, function(vec) (vec / sqrt(sum(vec^2)))))) # take only first ndim dimensions and normalize vector length
  colnames(emb) <- gsub('PC', 'V',colnames(emb))
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



