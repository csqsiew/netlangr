#' Get micro-level network measures for words in the single layer language network.
#'
#' @param network An igraph object of the language network.
#' @return A dataframe containing network statistics for each node in the network.
#' @examples
#' data <- getnetstats(network)
#' head(data)

getnetstats <- function(network) {

  # degree (local, undirected)
  degree_output <- data.frame(degree = igraph::degree(network),
                              id = c(1:igraph::gorder(network)))

  # clustering coefficent (local, undirected)
  clustering_output <- data.frame(id = c(1:igraph::gorder(network)),
                                  clustering = igraph::transitivity(network, type = 'local', isolates = 'zero'))

  # network component
  g.gc <- giantc(network) # list of words in the gc
  g.h <- hermits(network) # list of words that are hermits

  comp_output <- as.data.frame(igraph::V(network)$name) # data wrangling to categorize words into network comps
  comp_output$location <- 'L'
  comp_output[which(igraph::V(network)$name %in% g.gc), ]$location <- 'G'
  if (length(which(igraph::V(network)$name %in% g.h)) > 0) {
    comp_output[which(igraph::V(network)$name %in% g.h), ]$location <- 'H' # might not exist
  }
  colnames(comp_output)[1] <- 'node'
  comp_output$id <- c(1:nrow(comp_output))

  # V(network)$id <- c(1:gorder(network))

  # closeness centrality
  # closeness in igraph calculates a value for all nodes in a disconnected graph
  # it only really makes sense to calculate closeness for nodes in the gc.
  x <- sapply(igraph::decompose.graph(network), igraph::gorder) # split graph into their components
  y <- which(x == max(x)) # find the largest connected component
  lcc <- igraph::decompose.graph(network)[[y]] # select the lcc
  cc_out <- igraph::closeness(lcc, normalized = T)
  closeness_output <- data.frame(
    closeness_gc = cc_out, id = V(lcc)$id) # get closeness for each node, normalized or not?


  # compile data and output
  df <- suppressMessages(suppressWarnings(dplyr::left_join(comp_output, degree_output, by = 'id') %>%
                                            dplyr::left_join(clustering_output, by = 'id') %>%
                                            dplyr::left_join(closeness_output, by = 'id'))) # hermits and islands will have NA for closeness

  return(df)
}
