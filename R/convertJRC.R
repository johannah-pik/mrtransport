#' Convert Eurostat road transportation data to iso country.
#'
#' @param x a magpie data object
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("JRC")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @importFrom magclass getItems getSets getItems<- getSets<-

convertJRC <- function(x) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1), mapping = c("EL" = "GRC"))
  getSets(x)["d1.1"] <- "region"
  #Convert million pkm to billion pkm
  x <- x * 10^-3
  getItems(x, dim = "unit") <- "billion (p|t)km/yr"
  x <- suppressMessages(toolCountryFill(x, fill = NA))
  return(x)
}
