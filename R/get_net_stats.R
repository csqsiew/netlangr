#' Produces the degree, clustering coefficient, closeness centrality, component labels for each node in the igraph network object.
#'
#' @param g An igraph network object.
#' @return A dataframe object of the network statistics of the nodes.
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- makelangnet(somewords)
#' get_net_stats(somewordsnet)

get_closeness <- function(g) {
  # compute closeness using tore's closeness function
  # compute for ALL nodes in the network
  x <- as.data.frame(tnet::closeness_w(igraph::as_adj(g, sparse = F), gconly = F))
  x$label <- colnames(igraph::as_adj(g, sparse = F))[1:nrow(x)]
  # add back hermits
  hermits <- colnames(igraph::as_adj(g, sparse = F))[(nrow(x)+1):gorder(g)]
  y <- data.frame(node = 'h', closeness = 0, n.closeness = 0, label = hermits)
  return(rbind(x,y))
}

get_components <- function(g) {
  # recode nodes depending on whether they are in the GC, LI, or H
  clu <- igraph::components(g)     # in-built function in igraph that labels all nodes with a component id
  y <- clu$membership                    # note that y is a named vector
  z <- sort(table(y), decreasing = T)

  z_gc <- z[1]
  gc <- which(y == names(z_gc))             # extract nodes in the largest connected component
  gc.names <- names(gc)

  z_h <- which(z == 1) # extract nodes which are hermits
  h <- which(y %in% z_h)
  h.names <- names(y)[h]

  out <- data.frame(label = V(g)$name, component = 'L', stringsAsFactors = F)
  out$component[which(out$label %in% gc.names)] <- 'G'
  out$component[which(out$label %in% h.names)] <- 'H'

  return(out)
}

get_net_stats <- function(g) {
  x1 <- get_closeness(g)
  x2 <- get_components(g)
  x3 <- data.frame(label = V(g)$name, degree = igraph::degree(g),
                   clustering = igraph::transitivity(g, type = 'local', isolates = 'zero'))
  x4 <- suppressWarnings(dplyr::left_join(x3, x2, by = 'label') %>% dplyr::left_join(x1, by = 'label') %>%
                           dplyr::select(-node) %>%
                           tidyr::separate(label, into = c('name', 'node'), sep = '_', remove = F))
  x4$node <- as.numeric(x4$node)
  return(x4)
}
