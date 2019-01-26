#'
#'
#' access jbt models cached data
#'
#'

jbt <- new.env(parent = .GlobalEnv)

with(jbt, {

  models <- list()

  .INITIALIZED <- F
  .init <- function(reinitialize = F) {
    if(!.INITIALIZED || reinitialize){
      models_available <- .jbt_models_available()
      models <<- lapply(names(models_available), .getmodel)
      names(models) <<- names(models_available)
      .INITIALIZED <- T
    }
  }

  .sim_urlpattern='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/similar/${term}%23${pos}?numberOfEntries=1000&format=json'
  .sim_urlpattern_nopos='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/similar/${term}?numberOfEntries=1000&format=json'
  .sense_urlpattern='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/senses/${term}%23${pos}&format=json&sensetype=CW'
  .sense_urlpattern_nopos='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/senses/${term}&format=json&sensetype=CW'
  .sense_fine_urlpattern='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/senses/${term}%23${pos}?format=json&sensetype=CW-finer'
  .sense_fine_urlpattern_nopos='http://ltmaggie.informatik.uni-hamburg.de/jobimviz/ws/api/${model}/jo/senses/${term}?format=json&sensetype=CW-finer'

  .jbt_models_available <- function() list(
    en_jbt_trigram = list(lang = 'en', apiname = 'trigram', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_stanfordNew = list(lang = 'en', apiname = 'stanfordNew', sensemodel = T, finersensemodel = T, POS = T),
    en_jbt_stanfordContext = list(lang = 'en', apiname = 'stanfordContext', sensemodel = F, finersensemodel = F, POS = T),
    en_jbt_wikipediaStanford = list(lang = 'en', apiname = 'wikipediaStanford', sensemodel = F, finersensemodel = F, POS = T),
    en_jbt_medlineTrigram = list(lang = 'en', apiname = 'medlineTrigram', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_medlineParsed = list(lang = 'en', apiname = 'medlineParsed', sensemodel = F, finersensemodel = F, POS = T),
    en_jbt_medlineTrigramMwe = list(lang = 'en', apiname = 'medlineTrigramMwe', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_superfamilyBigram = list(lang = 'en', apiname = 'superfamilyBigram', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_superfamilyTrigram = list(lang = 'en', apiname = 'superfamilyTrigram', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_pfamBigram = list(lang = 'en', apiname = 'pfamBigram', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_pfamTrigram = list(lang = 'en', apiname = 'pfamTrigram', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_reviewsTrigram = list(lang = 'en', apiname = 'reviewsTrigram', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_twitter2012Bigram = list(lang = 'en', apiname = 'twitter2012Bigram', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_google = list(lang = 'en', apiname = 'google', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_stanford = list(lang = 'en', apiname = 'stanford', sensemodel = F, finersensemodel = F, POS = T),
    en_jbt_google1520 = list(lang = 'en', apiname = 'google1520', sensemodel = F, finersensemodel = F, POS = F),
    en_jbt_wikipediaTrigram = list(lang = 'en', apiname = 'wikipediaTrigram', sensemodel = F, finersensemodel = F, POS = F),
    de_jbt_germanTrigram = list(lang = 'de', apiname = 'germanTrigram', sensemodel = F, finersensemodel = F, POS = F),
    de_jbt_germanParsedLemma = list(lang = 'de', apiname = 'germanParsedLemma', sensemodel = F, finersensemodel = F, POS = T),
    de_jbt_germanTrigramMwe = list(lang = 'de', apiname = 'germanTrigramMwe', sensemodel = F, finersensemodel = F, POS = F),
    de_jbt_germanEducrawl = list(lang = 'de', apiname = 'germanEducrawl', sensemodel = F, finersensemodel = F, POS = T),
    de_jbt_twitterDETrigram = list(lang = 'de', apiname = 'twitterDETrigram', sensemodel = F, finersensemodel = F, POS = F),
    es_jbt_spanishTrigram = list(lang = 'es', apiname = 'spanishTrigram', sensemodel = F, finersensemodel = F, POS = F),
    fr_jbt_frenchTrigram = list(lang = 'fr', apiname = 'frenchTrigram', sensemodel = F, finersensemodel = F, POS = F),
    ar_jbt_arabicTrigram = list(lang = 'ar', apiname = 'arabicTrigram', sensemodel = F, finersensemodel = F, POS = F),
    bn_jbt_bengaliBigram = list(lang = 'bn', apiname = 'bengaliBigram', sensemodel = F, finersensemodel = F, POS = F),
    he_jbt_hebrewTrigram = list(lang = 'he', apiname = 'hebrewTrigram', sensemodel = F, finersensemodel = F, POS = F),
    hi_jbt_hindiTrigram = list(lang = 'hi', apiname = 'hindiTrigram', sensemodel = F, finersensemodel = F, POS = F),
    hi_jbt_hindiBigram = list(lang = 'hi', apiname = 'hindiBigram', sensemodel = F, finersensemodel = F, POS = F),
    nl_jbt_dutchTrigram = list(lang = 'nl', apiname = 'dutchTrigram', sensemodel = F, finersensemodel = F, POS = F),
    ru_jbt_russianTrigram = list(lang = 'ru', apiname = 'russianTrigram', sensemodel = F, finersensemodel = F, POS = F),
    sv_jbt_swedishTrigramMwe = list(lang = 'sv', apiname = 'swedishTrigramMwe', sensemodel = F, finersensemodel = F, POS = F),
    tr_jbt_turkishTrigram = list(lang = 'tr', apiname = 'turkishTrigram', sensemodel = F, finersensemodel = F, POS = F)
  )

  .get_jbt_url = function(pattern, model, term, pos)
    return(stringr::str_interp(pattern))

  .modelnames_for_lang <- function(lang) {
    matching_models <- grep(paste0('^',lang,'_'), names(.jbt_models_available()), value=T)
    return(matching_models)
  }

  .get_best_modelname_for_lang <- function(lang) {
    matching_models <- .modelnames_for_lang(lang)
    if(length(matching_models) > 0){
      return(matching_models[[1]])
    }
    return(NULL)
  }

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

  .get_json_from_url = function(url){
    tryCatch(
      expr = {
        # get from api
       util$message(sprintf('querying  \'%s\'.', url))
        # try to fetch and read json document
        js_doc <- jsonlite::fromJSON(txt = url)
        return(js_doc)
      },
      error = function(cond) {
        util$message(sprintf('ERROR retrieving \'%s\': %s', url, cond))
        return(NULL)
      }
    )
  }

  #'
  #'
  #'
  .get_JBT_similarities <- function(term, POS = NA, jbt_modelname) {
    if(is.na(POS)) POS <- 'N'
    model <- models[[jbt_modelname]]()
    if(model$POS)
      jbtPOS <- .convertToJbtPOS(POS)
    else
      jbtPOS <- ''

    # get from temp dir if existent
    fname <- cache$get_filename(term, jbtPOS, dirname = cache$data_temp_dir(), prefix = paste0('jbtsimapi__', jbt_modelname, '__'))
    js_doc <- cache$load(filename = fname, computefun = function() {
      pattern <- if(model$POS) .sim_urlpattern else .sim_urlpattern_nopos
      url <- .get_jbt_url(pattern, model$apiname, term, jbtPOS)
      .get_json_from_url(url)
    })

    if (!is.null(js_doc)) {
      if (length(js_doc$results) > 0) {
        sim <- data.frame(list(jbtterm=js_doc$results$key, score=js_doc$results$score), row.names = NULL)
        sim['term'] <- .cleanJbtTerm(sim$jbtterm)
        return(sim)
      }
    }
    return(list())
  }

  #'
  #'
  #'
  .get_JBT_senses <- function(term, POS = NA, finer=T, isas = F, jbt_modelname) {
    if(is.na(POS)) POS <- 'N'
    model <- models[[jbt_modelname]]()
    if(model$POS)
      jbtPOS <- .convertToJbtPOS(POS)
    else
      jbtPOS <- ''

    fname <- cache$get_filename(term, jbtPOS, dirname = cache$data_temp_dir(), prefix = paste0('jbtsenseapi', if(finer) 'finer' else '' ,'__', jbt_modelname, '__'))
    json_doc <- cache$load(fname, function() {
      if(finer){
        pattern <- if(model$POS) .sense_fine_urlpattern else .sense_fine_urlpattern_nopos
        url <- .get_jbt_url(pattern, model$apiname, term, jbtPOS)
      }else{
        pattern <- if(model$POS) .sense_urlpattern else .sense_urlpattern_nopos
        url <- .get_jbt_url(pattern, model$apiname, term, jbtPOS)
      }
      .get_json_from_url(url)
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

  .getmodel <- function(jbtmodelname){
    jbtmodel <- .jbt_models_available()[[jbtmodelname]]
    jbtmodel$name <- jbtmodelname
    jbtmodel$sim <- function(term, POS = NA) .get_JBT_similarities(term, POS, jbtmodelname)
    jbtmodel$senses <- function(term, POS = NA, finer=T, isas = F) .get_JBT_senses(term, POS, finer, isas, jbtmodelname)
    return(function() jbtmodel)
  }

}) # end with(...)

