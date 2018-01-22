#' Converts a list of words to an igraph network object. Note that makelangnet is simply a wrapper for tolangnet and nodeindex
#'
#' @param wordlist A list of words. Must be a character vector.
#' @return An igraph object of the language network created from \code{wordlist}.
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- tolangnet(somewords)
#' plot(somewordsnet) #plots the graph

makemultinet <- function(words) {
  # check that the input has two columns with Ortho and Phono names
  if(is.data.frame(words) == F) {
    stop('Please input a dataframe.')
  }

  if(is.element('Ortho', colnames(words)) == F) {
    stop('Ortho column not in dataframe.')
  }

  if(is.element('Phono', colnames(words)) == F) {
    stop('Phono column not in dataframe.')
  }

  words$label <- paste0(words$Ortho, ';', words$Phono)

  p.l <- nodeindex(tolangnet(words$Phono), words$label)
  igraph::E(p.l)$weight <- 1
  o.l <- nodeindex(tolangnet(words$Ortho), words$label)
  igraph::E(o.l)$weight <- 1

  m.net <- p.l %u% o.l
  igraph::E(m.net)$weight <- igraph::E(m.net)$weight_1 + igraph::E(m.net)$weight_2
  m.net <- igraph::remove.edge.attribute(m.net, "weight_1")
  m.net <- igraph::remove.edge.attribute(m.net, "weight_2")
  # edges = 1 are now NAs, convert them back to 1
  igraph::E(m.net)$weight[which(is.na(igraph::E(m.net)$weight))] <- 1

  return(m.net)
}

