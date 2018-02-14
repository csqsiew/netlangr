#' Converts a list of words to an igraph network object. SINGLE LAYER
#' Note that makelangnet is simply a wrapper for the tolangnet and nodeindex functions.
#'
#' @param wordlist A list of words. *Must be a character vector.*
#' @return An igraph object of the language network created from \code{wordlist}.
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- tolangnet(somewords)
#' plot(somewordsnet) #plots the graph

makelangnet <- function(wordlist, parallel = F) {

  if (parallel == F) {
    network <- nodeindex(tolangnet(wordlist), wordlist)
  } else { # parallel == T
    network <- nodeindex(tolangnet_p(wordlist), wordlist)
  }

  return(network)

  }
