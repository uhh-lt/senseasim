#'
#'
#' Utility functions for plotting data
#'
#'

vis <- new.env(parent = .GlobalEnv)

with(vis, {

  plotsenses <- function(term1='iron', term2='vitamin', POS1 = 'NN', POS2 = 'NN', vsm_modelname = sensevectors$.defaults$vsm_model, jbt_sense_api = sensevectors$.defaults$jbt_sense_api, topn_sense_terms =  sensevectors$.defaults$topn_sense_terms, shift_lambda = sensevectors$.defaults$shift_lambda){

    ## 1: get data
    R1 <- sensevectors$get_sense_vectors(term = term1, POS = POS1, vsm_modelname = vsm_modelname, jbt_sense_api = jbt_sense_api, topn_sense_terms = topn_sense_terms, shift_lambda = shift_lambda)
    R2 <- sensevectors$get_sense_vectors(term = term2, POS = POS2, vsm_modelname = vsm_modelname, jbt_sense_api = jbt_sense_api, topn_sense_terms = topn_sense_terms, shift_lambda = shift_lambda)
    R1$index$t1 <- T; R1$index$t2 <- F; R2$index$t1 <- F; R2$index$t2 <- T;
    index <- rbind(R1$index, R2$index)
    unique_i <- which(!duplicated(index$idx))

    model <- vsm$.models_loaded[[vsm_modelname]]
    M <- model$M[index$idx[unique_i],]
    # make column vectors
    M <- matrix(M, nrow=ncol(model$M), dimnames = list(NULL, index$mterm[unique_i]), byrow = T)
    # add sense vectors (shifted + non_shifted), replace colnames by stg like 'iron#1s'
    colnames(R1$v) <- paste0(term1,'#',1:ncol(R1$v))
    colnames(R2$v) <- paste0(term2,'#',1:ncol(R1$v))
    colnames(R1$v_shift) <- paste0(term1,'#',1:ncol(R1$v_shift),'s')
    colnames(R2$v_shift) <- paste0(term2,'#',1:ncol(R2$v_shift),'s')
    M <- cbind(M, R1$v, R1$v_shift, R2$v, R2$v_shift)

    # add description of sense vectors to index
    temp_index <- index[0,] # copy index definition
    temp_index[1:(2*(R1$nsenses+R2$nsenses)),'mterm'] <- c(colnames(R1$v), colnames(R1$v_shift), colnames(R2$v), colnames(R2$v_shift))
    temp_index$t1 <- c(rep(T, R1$nsenses*2), rep(F, R2$nsenses*2))
    temp_index$t2 <- !temp_index$t1
    temp_index$sense <- c(1:R1$nsenses, 1:R1$nsenses, 1:R2$nsenses, 1:R2$nsenses)
    temp_index$is_shifted <- c(rep(F, R1$nsenses), rep(T, R1$nsenses), rep(F, R2$nsenses), rep(T, R2$nsenses))
    index$is_shifted <- NA
    index <- rbind(index, temp_index)

    # prepare some more metadata
    index$is_sense_vector <- is.na(index$idx)
    index$is_sense_vector <- is.na(index$idx)

    ## 2: run tsne or PCA
    set.seed(1)
    num_rows_sample <- 15000
    Mdf <- embdf_TSNE(M, ndim = 2, normalize_length = T)
    index[,c('x','y')] <- Mdf[index$mterm,]

    ## 3: plot data
    p <- plot_bulls_eye(index)
    print(p)
  }

  plot_bulls_eye <- function(index) {

    # get vectors of t1 and t2
    # (x,y) * 0.8 # scale t2 vectors
    # (x,y) * 0.6 # scale sense vectors, and term vectors
    index$scale[index$t2 & index$sense > 0 & !index$is_sense_vector] <- 0.8
    index$scale[index$sense == 0 | index$is_sense_vector] <- 0.6
    index$scale[is.na(index$scale)] <- 1.0
    index[, c('x','y')] <- index[,c('x','y')] * index$scale

    # prepare color labels for different senses
    index$usense <- index$sense + 1
    index$usense[index$t2] <- (index$usense[index$t2] + max(index$sense[index$t1]) + 1)
    index$usense <- sapply(index$usense, toString)
    index$fontcolor <- 'black'
    index$fontcolor[index$t2] <- 'white'
    ncolors <- max(index$sense[index$t1]) + max(index$sense[index$t2]) + 2

    circles <- get_circles(dia = c(1.2, 1.6, 2))

    p <- ggplot2::ggplot() +
      ggplot2::geom_path (data = circles, ggplot2::aes(x = x, y = y, group = lev), colour = 'gray') + # circles
      ggplot2::geom_label(data = index[index$sense > 0 & !index$is_sense_vector,], ggplot2::aes_string(x='x', y='y', label='mterm', fill = 'usense', color = 'fontcolor')) + # terms t1 & t2
      ggrepel::geom_label_repel(data = index[(index$sense == 0 | (index$is_sense_vector & index$is_shifted)),], ggplot2::aes_string(x='x', y='y', label='mterm', fill = 'usense', color = 'fontcolor')) + # t1 sense vectors + original t1 vector
      ggplot2::scale_color_identity() +
      ggplot2::geom_segment(data = index[index$t1 & (index$sense == 0 | (index$is_sense_vector & index$is_shifted)),], ggplot2::aes_string(x='0', y='0', xend='x', yend='y'), color = 'darkgray', arrow = ggplot2::arrow(length = ggplot2::unit(0.01, 'npc'))) + # arrows t1
      ggplot2::geom_segment(data = index[index$t2 & (index$sense == 0 | (index$is_sense_vector & index$is_shifted)),], ggplot2::aes_string(x='0', y='0', xend='x', yend='y'), color = 'darkgray', arrow = ggplot2::arrow(length = ggplot2::unit(0.01, 'npc')), linetype='dashed') + # arrows t2
      ggplot2::geom_hline(yintercept=0, linetype='dashed', color = 'gray') + # add a horizontal line
      ggplot2::geom_vline(xintercept=0, linetype='dashed', color = 'gray') + # add a vertical line
      ggplot2::geom_text(data = index[index$t1 & index$sense == 0,], ggplot2::aes_string(x=0, y=-1, label = 'mterm'), color = 'darkgray', fontface='italic', nudge_y = -0.05, size=8, family='sans') + # add term 1 on the outer circle
      ggplot2::geom_text(data = index[index$t2 & index$sense == 0,], ggplot2::aes_string(x=0, y=-0.8, label = 'mterm'), color = 'darkgray', fontface='italic', nudge_y = -0.05, size=8, family='sans') + # add term 2 on the inner circle
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=ncolors))) +
      ggplot2::xlab('') + ggplot2::ylab('') +
      # theme_light(base_size=20) +
      ggplot2::theme_classic(base_size=20) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x     = ggplot2::element_blank(),
        axis.text.x      = ggplot2::element_blank(),
        axis.text.y      = ggplot2::element_blank(),
        axis.ticks       = ggplot2::element_blank(),
        axis.line        = ggplot2::element_blank(),
        panel.border     = ggplot2::element_blank(),
        legend.position  = 'none'
      )

    #ggsave('bullseye.pdf', p, width=20, height=20, units='cm')
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

  get_circles <- function(center=c(0,0), dia = c(1.2, 1.6, 2)){

    suppressPackageStartupMessages(require(dplyr))

    circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2, filled=FALSE){
      tt <- seq(start*pi, end*pi, length.out=npoints)
      df <- data.frame(
        x = center[1] + diameter / 2 * cos(tt),
        y = center[2] + diameter / 2 * sin(tt)
      )
      if(filled==TRUE) { #add a point at the center so the whole 'pie slice' is filled
        df <- rbind(df, center)
      }
      return(df)
    }

    circlegrid <- data.frame(dia = dia)
    circlegrid <- circlegrid %>%
      mutate(data = lapply(dia, function(x) {
        df     <- circleFun(center = center, diameter = x)
        df$lev <- x
        df
      }))
    circles <- bind_rows(circlegrid$data)
    circles$lev <- as.factor(circles$lev)
    return(circles)
  }

})
