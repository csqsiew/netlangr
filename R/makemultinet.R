#' Converts a list of words to an igraph network object. MULTILAYER
#' Note that makemultinet is simply a wrapper for the tolangnet and nodeindex functions,
#' but with additional code to make a weighted multilayered network from the union of the orthographic
#' and phonological layers of the language network.
#'
#' @param words A dataframe with Phono and Ortho columns of character class.
#' @return An igraph object of the language network created from \code{words}.
#'

makemultinet <- function(words, parallel = F) {
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


  if (parallel == F) {
    p.l <- nodeindex(tolangnet(words$Phono), words$label)
    igraph::E(p.l)$weight <- 1
    igraph::E(p.l)$type <- 'p'
    o.l <- nodeindex(tolangnet(words$Ortho), words$label)
    igraph::E(o.l)$weight <- 1
    igraph::E(o.l)$type <- 'o'
  } else { # parallel == T
    p.l <- nodeindex(tolangnet_p(words$Phono), words$label)
    igraph::E(p.l)$weight <- 1
    igraph::E(p.l)$type <- 'p'
    o.l <- nodeindex(tolangnet_p(words$Ortho), words$label)
    igraph::E(o.l)$weight <- 1
    igraph::E(o.l)$type <- 'o'
  }

  # m.net <- p.l %u% o.l
  m.net <- igraph::union(p.l, o.l, byname = T)
  igraph::E(m.net)$weight <- igraph::E(m.net)$weight_1 + igraph::E(m.net)$weight_2
  m.net <- igraph::remove.edge.attribute(m.net, "weight_1")
  m.net <- igraph::remove.edge.attribute(m.net, "weight_2")
  # edges = 1 are now NAs, convert them back to 1
  igraph::E(m.net)$weight[which(is.na(igraph::E(m.net)$weight))] <- 1

  igraph::E(m.net)$type <- gsub('NA', '', paste0(igraph::E(m.net)$type_1, igraph::E(m.net)$type_2))
  m.net <- igraph::remove.edge.attribute(m.net, "type_1")
  m.net <- igraph::remove.edge.attribute(m.net, "type_2")

  igraph::V(m.net)$id <- c(1:igraph::gorder(m.net)) # to make each node unique

  return(m.net)
}

