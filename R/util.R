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
    format(Sys.time(), "%m%d-%H%M%S"),
    content
  )


  sendmessage <- function(content = ' ') {
    api <- Sys.getenv('MAILGUN_API')
    recepient <- Sys.getenv('MAILGUN_RECEPIENT')
    if(stringi::stri_isempty(api) || stringi::stri_isempty(recepient)){
      return('Missing environment variables! (please set $MAILGUN_API and $MAILGUN_RECEPIENT)')
    }
    r <- curl_fetch_memory(
      api,
      curl::handle_setform(
        curl::new_handle(),
        from='The Computer <computer@computerwork.org>',
        to=recepient,
        subject = 'Computation Finished! <EOM>',
        text = content
      )
    )
    return(rawToChar(r$content))
  }

})
