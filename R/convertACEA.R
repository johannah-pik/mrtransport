#' Convert ACEA vehicles on roads data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("ACEA")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @importFrom magclass getItems getSets getItems<- getSets<-

convertACEA <- function (x, subtype) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x <- toolCountryFill(x, fill = NA)
  return(x)
}