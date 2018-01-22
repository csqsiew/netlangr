#' Get micro-level network statistics for words in the language network.
#'
#' @param network An igraph object of the language network.
#' @return A dataframe containing network statistics for each node in the network.
#' @examples
#' data <- getnetstats(network)
#' head(data)

getmultistats <- function(network) {

  # degree
  # unique degree (nodes that are found in both layers)
  # subset the graph by weight == 2 (this is the pg.net)
  pg.net <- igraph::subgraph.edges(network, igraph::E(network)[igraph::E(network)$weight > 1], del = F)
  degree_output_1 <- as.data.frame(igraph::degree(pg.net))
  degree_output_1$node <- row.names(degree_output_1)
  colnames(degree_output_1)[1] <- 'degree.pg'

  # neighborhood degree (found in either layer, exclude duplicates)
  degree_output_2 <- as.data.frame(igraph::degree(network)) # ignores weights, naturally exclude duplicates
  degree_output_2$node <- row.names(degree_output_2)
  colnames(degree_output_2)[1] <- 'degree.all'

  # clustering
  # unique C (overlap in both layers), i.e., pg-C
  clustering_output_1 <- as.data.frame(igraph::transitivity(pg.net, type = 'local', isolates = 'zero'))
  clustering_output_1$node <- names(igraph::V(pg.net))
  colnames(clustering_output_1)[1] <- 'clustering.pg'

  # neighborhood C (both layers considered, unweighted)
  clustering_output_2 <- as.data.frame(igraph::transitivity(network, type = 'local', isolates = 'zero'))
  clustering_output_2$node <- names(igraph::V(network))
  colnames(clustering_output_2)[1] <- 'clustering.unweighted'

  # neighborhood C (both layers considered, weighted)
  clustering_output_3 <- as.data.frame(igraph::transitivity(network, type = 'weighted', isolates = 'zero'))
  clustering_output_3$node <- names(igraph::V(network))
  colnames(clustering_output_3)[1] <- 'clustering.weighted'

  # network component
  g.gc <- giantc(network) # list of words in the gc
  g.h <- hermits(network) # list of words that are hermits

  comp_output <- as.data.frame(igraph::V(network)$name) # data wrangling to categorize words into comps
  comp_output$location <- 'L'
  comp_output[which(igraph::V(network)$name %in% g.gc), ]$location <- 'G' # will exist
  if (length(which(igraph::V(network)$name %in% g.h)) > 0) {
    comp_output[which(igraph::V(network)$name %in% g.h), ]$location <- 'H' # might not exist
  }
  colnames(comp_output)[1] <- 'node'

  # closeness centrality
  # closeness in igraph calculates a value for all nodes in a disconnected graph
  # it only really makes sense to calculate closeness for nodes in the gc.
  x <- sapply(igraph::decompose.graph(network), vcount) # split graph into their components
  y <- which(x == max(x)) # find the largest connected component
  lcc <- igraph::decompose.graph(network)[[y]] # select the lcc
  closeness_output_1 <- as.data.frame(igraph::closeness(lcc, normalized = T)) # get closeness for each node, normalized or not?
  # normalized = T because networks can be of different sizes, want values to be comparable across different network sizes?
  closeness_output_1$node <- row.names(closeness_output_1)
  colnames(closeness_output_1)[1] <- 'closeness.gc.weighted'

  igraph::E(lcc)$weight <- 1 # to force weights to be the same for all, so centrality is calculated on an unweighted lcc
  # might not be a huge difference...
  closeness_output_2 <- as.data.frame(igraph::closeness(lcc, normalized = T)) # get closeness for each node, normalized or not?
  closeness_output_2$node <- row.names(closeness_output_2)
  colnames(closeness_output_2)[1] <- 'closeness.gc.unweighted'

  # compile data and output
  df <- suppressMessages(suppressWarnings(dplyr::left_join(comp_output, degree_output_1) %>%
                                            dplyr::left_join(degree_output_2) %>%
                                            dplyr::left_join(clustering_output_1) %>%
                                            dplyr::left_join(clustering_output_2) %>%
                                            dplyr::left_join(clustering_output_3) %>%
                                            dplyr::left_join(closeness_output_1) %>%
                                            dplyr::left_join(closeness_output_2))) # hermits and islands will have NA for closeness


  return(df)
}
