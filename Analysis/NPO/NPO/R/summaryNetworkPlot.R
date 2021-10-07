summaryNetworkPlot <- function( subject, channel, seizureUsed, signalType, clusterid, parms, grph, context ) {
#  library( GGally )
#  library( ggnetwork )
#  library( dplyr )
#  library( ggpubr )
#  library( Rfast )
#  library( reshape2 )
#  library( ggplot2 )
#  library( igraph )
  
  ylim_ <- c( context$range_min, context$range_max )

  waves <- data.frame( mn=parms$avg_waveform )
  #  p_wave <- ggplot( data=waves, aes( x=seq(1,length(mn)), y=mn ) ) + geom_line() + geom_errorbar(aes(ymin=mn-sd,ymax=mn+sd)) + ylim(c(-250,250))
  p_wave <- ggplot( data=waves, aes( x=seq(1,length(mn)), y=mn ) ) + geom_line() + ylim( ylim_[1], ylim_[2] )
  
  N <- c('duration','rate','diameter','edge_density','degree','hub_score','mean_distance','transitivity')
  V <- c( log10(parms$duration), log10(parms$rate), log10(parms$diameter), 5*parms$edge_density, log10(parms$degree), log10(parms$hub_score), log10(parms$mean_distance), parms$transitivity )
  
  df <- data.frame( names=N, values=V )  
  df_raw <- melt( df )
  p_A <- ggplot( data=df_raw, aes(x=names,y=value) ) + geom_bar(stat="identity") + ylim(c(-1,5)) + coord_flip()
  
  p_top <- ggarrange( p_wave, p_A, ncol=2, widths=c(1,1) )
  
  #  p2 <- barplot( V, horiz=TRUE, xlim=c(-1,1), names.arg=N )
  ecc <- 1 + igraph::eccentricity( grph )
  bet <- 200 * igraph::betweenness( grph, directed = FALSE, weights = NULL, normalized = TRUE )
    edge_alpha <- min( 1, 500 / length(E(grph)) )
  if ( length(V(grph))<=3 | length(E(grph))<=3 ) {
    p3 <- p_top
  } else {
    p3 <- ggplot( ggnetwork(grph, layout="circle", arrow.gap=0.05),aes(x,y,xend=xend,yend=yend)) +
      geom_edges(color="grey50", arrow=arrow(length=unit(10,"pt"), type="closed"),alpha=edge_alpha) +
      geom_nodes(size=bet, color=ecc) +
      geom_nodetext(aes(label=NA)) + 
      theme_blank() +
      coord_fixed( 0.5 )
  }  
  #  print( p )
  p <- ggarrange( p_top, p3, nrow=2, widths=c(1,1) )
  
  return( p )
}

