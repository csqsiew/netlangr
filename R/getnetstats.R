#' Get micro-level network statistics for words in the language network.
#'
#' @param network An igraph object of the language network.
#' @return A dataframe containing network statistics for each node in the network.
#' @examples
#' data <- getnetstats(network)
#' head(data)

getnetstats <- function(network) {

  # degree (local, undirected)
  degree_output <- as.data.frame(degree(network))
  degree_output$node <- row.names(degree_output)
  colnames(degree_output)[1] <- 'degree'

  # clustering coefficent (local, undirected)
  clustering_output <- as.data.frame(transitivity(network, type = 'local', isolates = 'zero'))
  clustering_output$node <- names(V(network))
  colnames(clustering_output)[1] <- 'clustering'

  # network component
  g.gc <- giantc(network) # list of words in the gc
  g.h <- hermits(network) # list of words that are hermits

  comp_output <- as.data.frame(V(network)$name) # data wrangling to categorize words into network comps
  comp_output$location <- 'L'
  comp_output[which(V(network)$name %in% g.gc), ]$location <- 'G'
  comp_output[which(V(network)$name %in% g.h), ]$location <- 'H'
  colnames(comp_output)[1] <- 'node'

  # closeness centrality
  # closeness in igraph calculates a value for all nodes in a disconnected graph
  # it only really makes sense to calculate closeness for nodes in the gc.
  x <- sapply(decompose.graph(network), vcount) # split graph into their components
  y <- which(x == max(x)) # find the largest connected component
  lcc <- decompose.graph(network)[[y]] # select the lcc
  closeness_output <- as.data.frame(closeness(lcc, normalized = T)) # get closeness for each node, normalized or not?
  closeness_output$node <- row.names(closeness_output)
  colnames(closeness_output)[1] <- 'closeness_gc'

  # compile data and output
  df <- suppressMessages(suppressWarnings(left_join(comp_output, degree_output) %>%
                                            left_join(clustering_output) %>%
                                            left_join(closeness_output))) # hermits and islands will have NA for closeness

  return(df)
}
