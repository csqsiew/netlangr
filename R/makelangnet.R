#' Converts a list of words to an igraph network object. SINGLE LAYER
#' Note that makelangnet is simply a wrapper for the tolangnet and nodeindex functions.
#'
#' @param wordlist A list of words. *Must be a character vector.*
#' @return An igraph object of the language network created from \code{wordlist}.
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- tolangnet(somewords)
#' plot(somewordsnet) #plots the graph

makelangnet <- function(wordlist) {
  # network <- tolangnet(wordlist)
  network <- nodeindex(tolangnet(wordlist), wordlist)
  return(network)
}
