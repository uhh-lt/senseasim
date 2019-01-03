#'
#'
#' access jbt models cached data
#'
#'

jbt <- new.env(parent = .GlobalEnv)

with(jbt, {

  .sim_urlpattern='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/similar/${term}%23${pos}?numberOfEntries=1000&format=json'
  .sense_urlpattern='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/senses/${term}%23${pos}&format=json&sensetype=CW'
  .sense_fine_urlpattern='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/senses/${term}%23${pos}?format=json&sensetype=CW-finer'

  .get_jbt_url = function(pattern, model, term, pos)
    return(stringr::str_interp(pattern))

  .jbtmodels_for_lang <- function(lang) {
    matching_models <- grep('^bn_', names(jbt$.jbt_models), value=T)
    return(matching_models)
  }

  .get_best_jbtmodel_for_lang <- function(lang) {
    matching_models <- .jbtmodels_for_lang(lang)
    if(length(matching_models) > 0){
      return(matching_models[[1]])
    }
    return(NULL)
  }

  .jbt_models <- list(
    en_jbt_stanfordNew = 'stanfordNew',
    en_jbt_stanfordContext = 'stanfordContext',
    en_jbt_wikipediaStanford = 'wikipediaStanford',
    en_jbt_trigram = 'trigram',
    en_jbt_medlineTrigram = 'medlineTrigram',
    en_jbt_medlineParsed = 'medlineParsed',
    en_jbt_medlineTrigramMwe = 'medlineTrigramMwe',
    en_jbt_superfamilyBigram = 'superfamilyBigram',
    en_jbt_superfamilyTrigram = 'superfamilyTrigram',
    en_jbt_pfamBigram = 'pfamBigram',
    en_jbt_pfamTrigram = 'pfamTrigram',
    en_jbt_reviewsTrigram = 'reviewsTrigram',
    en_jbt_twitter2012Bigram = 'twitter2012Bigram',
    en_jbt_google = 'google',
    en_jbt_stanford = 'stanford',
    en_jbt_google1520 = 'google1520',
    en_jbt_wikipediaTrigram = 'wikipediaTrigram',
    de_jbt_germanParsedLemma = 'germanParsedLemma',
    de_jbt_germanTrigram = 'germanTrigram',
    de_jbt_germanTrigramMwe = 'germanTrigramMwe',
    de_jbt_germanEducrawl = 'germanEducrawl',
    de_jbt_twitterDETrigram = 'twitterDETrigram',
    es_jbt_spanishTrigram = 'spanishTrigram',
    fr_jbt_frenchTrigram = 'frenchTrigram',
    ar_jbt_arabicTrigram = 'arabicTrigram',
    bn_jbt_bengaliBigram = 'bengaliBigram',
    he_jbt_hebrewTrigram = 'hebrewTrigram',
    hi_jbt_hindiBigram = 'hindiBigram',
    hi_jbt_hindiTrigram = 'hindiTrigram',
    nl_jbt_dutchTrigram = 'dutchTrigram',
    ru_jbt_russianTrigram = 'russianTrigram',
    sv_jbt_swedishTrigramMwe = 'swedishTrigramMwe',
    tr_jbt_turkishTrigram = 'turkishTrigram'
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
  get_JBT_similarities <- function(term, POS = 'N', jbt_modelname = names(.jbt_models)[[1]]) {
    model = .jbt_models[[jbt_modelname]]
    jbtPOS <- .convertToJbtPOS(POS)

    # get from temp dir if existent
    fname <- cache$get_filename(term, jbtPOS, dirname = cache$data_temp_dir(), prefix = paste0('jbtsimapi__', jbt_modelname, '__'))
    js_doc <- cache$load(filename = fname, computefun = function() {
      url <- .get_jbt_url(.sim_urlpattern, model, term, jbtPOS)
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
        js_doc <- jsonlite::fromJSON(txt = url)
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
  get_JBT_senses <- function(term, POS = 'N', jbt_modelname = names(.jbt_models)[[1]], finer=T, isas = F) {
    model = .jbt_models[[jbt_modelname]]
    jbtPOS <- .convertToJbtPOS(POS)
    fname <- cache$get_filename(term, jbtPOS, dirname = cache$data_temp_dir(), prefix = paste0('jbtsenseapi', if(finer) 'finer' else '' ,'__', jbt_modelname, '__'))
    json_doc <- cache$load(fname, function() {
      if(finer){
        url <- .get_jbt_url(.sense_fine_urlpattern, model, term, jbtPOS)
      }else{
        url <- .get_jbt_url(.sense_urlpattern, model, term, jbtPOS)
      }
      get_json_from_url(url)
    })

    if (!is.null(json_doc)) {
      if (length(json_doc$result) > 0){
        if (isas){
          return(Filter(function(l) length(l) > 0, json_doc$result$isas))
        }
        # else
        return(Filter(function(l) length(l) > 0, json_doc$result$senses))
      }
    }
    # else
    return(list())
  }

}) # end with(...)
