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
  <script type="text/javascript">
  window.location.href = "__swagger__/"
  </script>
  <title>Page Redirection</title>
  </head>
  <body>If you are not redirected automatically, follow this <a href="__swagger__/">link</a>.
  '
}


#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot out data from the iris dataset
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

# #* Log some information about the incoming request
# #* @filter redirect
# function(req, resp){
#   if(req$PATH_INFO == '/'){
#     #req$PATH_INFO = '/__swagger__/'
#     cat("redirecting '\\' to '\\__swagger__\\'." )
#   }
#
#   plumber::do_forward(pr$endpoints[[1]][[1]], '/')
# }

#pr <- plumber::plumb("R/webserver.R")
#pr$run(port=6348, swagger = TRUE)
#pr$handle("GET", "/", pr$routes$`__swagger__`)
#pr$run()


