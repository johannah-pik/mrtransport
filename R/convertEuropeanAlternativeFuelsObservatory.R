#' Convert European Alternative Fuels Observatory vehicles on roads data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("EUstatisticalPocketbook")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @importFrom magclass getItems getSets getItems<- getSets<-

convertEuropeanAlternativeFuelsObservatory <- function (x, subtype) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x <- toolCountryFill(x, fill = NA)
  return(x)
}