

#'
#'
#' access jbt models cached data
#'
#'

jbt <- new.env(parent = .GlobalEnv)

with(jbt, {

  .sim_models = list(
    'stanfordnew' = 'http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/stanfordNew/jo/similar/%s%%23%s?numberOfEntries=1000&format=json'
  )

  .sense_models = list(
    'stanfordnew_fine' = 'http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/stanfordNew/jo/senses/%s%%23%s?format=json&sensetype=CW-finer',
    'stanfordnew_broad' = 'http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/stanfordNew/jo/senses/%s%%23%s?&format=json&sensetype=CW'
  )

  #'
  #' Helper function to convert POS tags to JBT POS tags
  #'
  .convertToJbtPOS <- function(POS) {
    POS_ <- toupper(POS)
    switch(
      POS_,
      A = 'JJ',
      J = 'JJ',
      N = 'NN',
      V = 'VB',
      {
        #warning(sprintf('unkown POS %s', POS))
        POS
      }
    )
  }

  .cleanJbtTerm <- function(...){
    if(length(...)[[1]] > 1) # gsub can handle list and vectors, but not lists of lists, ...
      sapply(..., .cleanJbtTerm)
    else
      # clean both, isas (':') or jbt terms ('#'), clear empty spaces
      gsub('^\\s+', '',  # whitespae at end
           gsub('\\s+$', '', # whitespae at front
                gsub('#.*', '', # jbt term: foo#NN
                     gsub(':.*','', ...) # isa term: foo:4711
                )))
  }

  #'
  #'
  #'
  get_JBT_similarities <- function(term, POS = 'N', model_template = .sim_models[[1]], modelname = names(.sim_models)[[1]]) {

    jbtPOS <- .convertToJbtPOS(POS)

    # get from temp dir if existent
    fname <- cache$get_filename(term, jbtPOS, dirname = cache$data_temp_dir(), prefix = paste0('jbtsimapi__', modelname, '__'))
    js_doc <- cache$load(filename = fname, loadfun = function() {
      url <- sprintf(model_template, term, jbtPOS)
      get_json_from_url(url)
    })

    if (!is.null(js_doc)) {
      if (length(js_doc$results) > 0) {
        sim <- as.vector(js_doc$results$score)
        names(sim) <- js_doc$results$key
        return(sim)
      }
    }
    return(list())
  }

  get_json_from_url = function(url){
    tryCatch(
      expr = {
        # get from api
        message(sprintf('[%s-%d-%s] querying  \'%s\'.', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), url))
        # try to fetch and read json document
        js_doc <- fromJSON(txt = url)
        return(js_doc)
      },
      error = function(cond) {
        message(sprintf('[%s-%d-%s] ERROR retrieving \'%s\': %s', gsub('\\..*$', '', Sys.info()[['nodename']]), Sys.getpid(), format(Sys.time(), "%m%d-%H%M%S"), url, cond))
        return(NULL)
      }
    )
  }

  #'
  #'
  #'
  get_JBT_senses <- function(term, POS = 'N', model_template = .sense_models[[1]], modelname = names(.sense_models)[[1]], isas = F) {
    jbtPOS <- .convertToJbtPOS(POS)
    fname <- cache$get_filename(term, jbtPOS, dirname = cache$data_temp_dir(), prefix = paste0('jbtsenseapi__', modelname, '__'))
    json_doc <- cache$load(fname, function() {
      url <- sprintf(model_template, term, jbtPOS)
      get_json_from_url(url)
    })

    if (!is.null(json_doc)) {
      if (length(json_doc$result) > 0){
        if (isas){
          return(json_doc$result$isas)
        }
        # else
        return(json_doc$result$senses)
      }
    }
    # else
    return(list())
  }

}) # end with(...)
