#' Get micro-level network measures for words in the multilayer language network.
#'
#' @param network An igraph object of the language network (phonographic multiplex).
#' @return A dataframe containing network statistics for each node in the network.
#' @examples
#' data <- getmultistats(network)
#' head(data)

getmultistats <- function(network) {

  # degree
  # unique degree (nodes that are found in both layers)
  # subset the graph by weight == 2 (this is the pg.net)
  pg.net <- igraph::subgraph.edges(network, igraph::E(network)[igraph::E(network)$weight > 1], del = F)
  #degree_output_1 <- as.data.frame(igraph::degree(pg.net))
  #degree_output_1$node <- row.names(degree_output_1)
  #colnames(degree_output_1)[1] <- 'degree.pg'

  degree_output_1 <- data.frame(degree.pg = igraph::degree(pg.net),
                              id = c(1:vcount(pg.net)))

  # neighborhood degree (found in either layer, exclude duplicates)
  #degree_output_2 <- as.data.frame(igraph::degree(network)) # ignores weights, naturally exclude duplicates
  #degree_output_2$node <- row.names(degree_output_2)
  #colnames(degree_output_2)[1] <- 'degree.all'

  degree_output_2 <- data.frame(degree.all = igraph::degree(network),
                                id = c(1:vcount(network)))

  # clustering
  # unique C (overlap in both layers), i.e., pg-C
  #clustering_output_1 <- as.data.frame(igraph::transitivity(pg.net, type = 'local', isolates = 'zero'))
  #clustering_output_1$node <- names(igraph::V(pg.net))
  #colnames(clustering_output_1)[1] <- 'clustering.pg'

  clustering_output_1 <- data.frame(id = c(1:vcount(pg.net)),
                          clustering.pg = igraph::transitivity(pg.net, type = 'local', isolates = 'zero'))


  # neighborhood C (both layers considered, unweighted)
  #clustering_output_2 <- as.data.frame(igraph::transitivity(network, type = 'local', isolates = 'zero'))
  #clustering_output_2$node <- names(igraph::V(network))
  #colnames(clustering_output_2)[1] <- 'clustering.unweighted'

  clustering_output_2 <- data.frame(id = c(1:vcount(network)),
                  clustering.unweighted = igraph::transitivity(network, type = 'local', isolates = 'zero'))


  # neighborhood C (both layers considered, weighted)
  #clustering_output_3 <- as.data.frame(igraph::transitivity(network, type = 'weighted', isolates = 'zero'))
  #clustering_output_3$node <- names(igraph::V(network))
  #colnames(clustering_output_3)[1] <- 'clustering.weighted'

  clustering_output_3 <- data.frame(id = c(1:vcount(network)),
                clustering.weighted = igraph::transitivity(network, type = 'weighted', isolates = 'zero'))


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
  comp_output$id <- c(1:nrow(comp_output))

  # closeness centrality
  # closeness in igraph calculates a value for all nodes in a disconnected graph
  # it only really makes sense to calculate closeness for nodes in the gc.
  x <- sapply(igraph::decompose.graph(network), igraph::vcount) # split graph into their components
  y <- which(x == max(x)) # find the largest connected component
  lcc <- igraph::decompose.graph(network)[[y]] # select the lcc

  cc_out <- igraph::closeness(lcc, normalized = T)

  closeness_output_1 <- data.frame(
    closeness.gc.weighted = cc_out, id = V(lcc)$id) # get closeness for each node, normalized or not?

  #closeness_output_1 <- as.data.frame(igraph::closeness(lcc, normalized = T)) # get closeness for each node, normalized or not?
  # normalized = T because networks can be of different sizes, want values to be comparable across different network sizes?
  #closeness_output_1$node <- row.names(closeness_output_1)
  #colnames(closeness_output_1)[1] <- 'closeness.gc.weighted'

  igraph::E(lcc)$weight <- 1 # to force weights to be the same for all, so centrality is calculated on an unweighted lcc
  # might not be a huge difference...
  cc_out <- igraph::closeness(lcc, normalized = T)
  closeness_output_2 <- data.frame(
    closeness.gc.unweighted = cc_out, id = V(lcc)$id)

  #closeness_output_2 <- as.data.frame(igraph::closeness(lcc, normalized = T)) # get closeness for each node, normalized or not?
  #closeness_output_2$node <- row.names(closeness_output_2)
  #colnames(closeness_output_2)[1] <- 'closeness.gc.unweighted'

  # compile data and output
  df <- suppressMessages(suppressWarnings(dplyr::left_join(comp_output, degree_output_1, by = 'id') %>%
                                            dplyr::left_join(degree_output_2, by = 'id') %>%
                                            dplyr::left_join(clustering_output_1, by = 'id') %>%
                                            dplyr::left_join(clustering_output_2, by = 'id') %>%
                                            dplyr::left_join(clustering_output_3, by = 'id') %>%
                                            dplyr::left_join(closeness_output_1, by = 'id') %>%
                                            dplyr::left_join(closeness_output_2, by = 'id'))) # hermits and islands will have NA for closeness


  return(df)
}
