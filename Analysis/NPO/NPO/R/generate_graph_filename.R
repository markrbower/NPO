generate_graph_filename <- function( subject, channel, seizureUsed ) {
  return( paste0( subject, '_', channel, '_', seizureUsed, '_graph.xml' ) )
}
