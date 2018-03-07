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
#* @param vsm_modelname The vector space model to use
#* @param senseinventoryname The inventory to use
#* @param topn_sense_terms
#* @param shift_lambda
#* @get /sensevector
function(res, term='vitamin', POS='NN', RET='index', vsm_modelname = sensevectors$.defaults$vsm_model, senseinventoryname = sensevectors$.defaults$senseinventoryname, topn_sense_terms = sensevectors$.defaults$topn_sense_terms, shift_lambda = sensevectors$.defaults$shift_lambda){
  # @ serializer contentType list(type="application/json")
  # json <- jsonlite::toJSON(list(
  #   index <- vec$index,
  #   vector <- t(vec$v)
  # ))
  # res$body <- json

  if(is.character(shift_lambda)){
    shift_lambda <- as.double(shift_lambda)
  }
  if(is.character(topn_sense_terms)){
    topn_sense_terms <- as.integer(topn_sense_terms)
  }

  vsm$load_default_matrices(list(vsm_modelname))

  vec <- sensevectors$get_sense_vectors(term, POS, vsm_modelname = vsm_modelname, senseinventoryname = senseinventoryname, topn_sense_terms = topn_sense_terms, shift_lambda = shift_lambda)
  message('index')
  switch (RET,
    'status' = vec$status,
    'vector' = vec$v,
    'vectorshift' = vec$v_shift,
    vec$index
  )
}

# #* Plot out data from the iris dataset (EXAMPLE FUNCTION)
# #* @param spec If provided, filter the data to only this species (e.g. 'setosa')
# #* @get /plot
# #* @png
# function(spec){
#   myData <- iris
#   title <- "All Species"
#
#   # Filter if the species was specified
#   if (!missing(spec)){
#     title <- paste0("Only the '", spec, "' Species")
#     myData <- subset(iris, Species == spec)
#   }
#
#   plot(myData$Sepal.Length, myData$Petal.Length,
#        main=title, xlab="Sepal Length", ylab="Petal Length")
# }

#* Plot senses of two terms
#* @param term1
#* @param term2
#* @param POS1
#* @param POS2
#* @param vsm_modelname The vector space model to use
#* @param senseinventoryname The inventory to use
#* @param topn_sense_terms
#* @param shift_lambda
#* @param reduction tsne or pca, default: tsne
#* @get /plotsenses
#* @png
function(term1='iron', term2='vitamin', POS1 = 'NN', POS2 = 'NN', vsm_modelname = sensevectors$.defaults$vsm_model, senseinventoryname = sensevectors$.defaults$senseinventoryname, topn_sense_terms =  sensevectors$.defaults$topn_sense_terms, shift_lambda = sensevectors$.defaults$shift_lambda, reduction='tsne'){

  vsm$load_default_matrices(list(vsm_modelname))

  if(is.character(shift_lambda)){
    shift_lambda <- as.double(shift_lambda)
  }
  if(is.character(topn_sense_terms)){
    topn_sense_terms <- as.integer(topn_sense_terms)
  }

  #: get the plot
  p <- vis$plotsenses(term1, term2, POS1, POS2, vsm_modelname, senseinventoryname, topn_sense_terms, shift_lambda, reduction)

  #: return the plot
  print(p)
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



