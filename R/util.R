#'
#' Provide some utility functions
#'

util <- new.env(parent = .GlobalEnv)

with(util, {

  message <- function(content) base::message(as.messagestring(content))

  as.messagestring <- function(content) sprintf(
    '[%s-%d-%s] %s',
    gsub('\\..*$', '', Sys.info()[['nodename']]),
    Sys.getpid(),
    format(Sys.time(), "%Y%m%d-%H%M%S"),
    content
  )

  sendmessage <- function(subject = ' ', content = ' ') {
    api <- Sys.getenv('MAILGUN_API')
    recepient <- Sys.getenv('MAILGUN_RECEPIENT')
    if(stringi::stri_isempty(api) || stringi::stri_isempty(recepient)){
      return('Missing environment variables! (please set $MAILGUN_API and $MAILGUN_RECEPIENT)')
    }
    r <- curl::curl_fetch_memory(
      api,
      curl::handle_setform(
        curl::new_handle(),
        from='The Computer <computer@computerwork.org>',
        to=recepient,
        subject = subject,
        text = content
      )
    )
    return(rawToChar(r$content))
  }

  py.source_string <- function(pystring, envir = parent.frame(), convert = TRUE){
    pyfile <- tempfile(fileext='.py')
    sink(file=pyfile)
    cat(pystring, sep='\n')
    sink()
    reticulate::source_python(pyfile, envir, convert)
    unlink(pyfile)
  }

})
