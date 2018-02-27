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

  indices <- list()

  v1 <- vsm$get_vector(term = term1, modelname = sensevectors$.defaults$vsm_model, .as_column = T); colnames(v1) <- c(term1)
  v2 <- vsm$get_vector(term = term2, modelname = sensevectors$.defaults$vsm_model, .as_column = T); colnames(v2) <- c(term2)

  V <- cbind(v1,v2)
  indices[[term1]] <- c(1)
  indices[[term2]] <- c(2)
  n <- 2

  jb_sense_lists1 <- jbt$get_JBT_senses(term1, POS1, isas = F, modelname = sensevectors$.defaults$jbt_sense_api)
  jb_sense_lists2 <- jbt$get_JBT_senses(term2, POS2, isas = F, modelname = sensevectors$.defaults$jbt_sense_api)

  for(i in 1:length(jb_sense_lists1)){
    list_of_jb_terms <- jb_sense_lists1[[i]]
    sense_terms <- list_of_jb_terms[1:min(length(list_of_jb_terms), sensevectors$.defaults$topn_sense_terms)]
    vectors <- sensevectors$get_vectors_from_jbtterms(sense_terms, sensevectors$.defaults$vsm_model, .as_column_vectors = T)
    V <- cbind(V, vectors)
    indices[[paste0('senseterms_sense__', i, '__', term1)]] <- seq(1:ncol(vectors)) + n
    n <- ncol(vectors) + n
  }

  for(i in 1:length(jb_sense_lists2)){
    list_of_jb_terms <- jb_sense_lists2[[i]]
    sense_terms <- list_of_jb_terms[1:min(length(list_of_jb_terms), sensevectors$.defaults$topn_sense_terms)]
    vectors <- sensevectors$get_vectors_from_jbtterms(sense_terms, sensevectors$.defaults$vsm_model, .as_column_vectors = T)
    V <- cbind(V, vectors)
    indices[[paste0('senseterms_sense__', i, '__', term2)]] <- seq(1:ncol(vectors)) + n
    n <- ncol(vectors) + n
  }

  S1 <- sensevectors$get_sense_vectors(term = term1, POS = POS1, vsm_modelname = sensevectors$.defaults$vsm_model)
  indices[[paste0('sensevectors__', term1)]] <- seq(1:ncol(S1)) + n
  n <- ncol(S1) + n

  S2 <- sensevectors$get_sense_vectors(term = term2, POS = POS2, vsm_modelname = sensevectors$.defaults$vsm_model)
  indices[[paste0('sensevectors__', term2)]] <- seq(1:ncol(S2)) + n
  n <- ncol(S2) + n

  M <- cbind(V, S1, S2)

  ## TODO:
  ## 1: run tsne or PCA
  ## 2: plot data

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



