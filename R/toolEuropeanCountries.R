#' Countries of the European regions EUR and NEU
#'
#' Used to apply the European (TRACCS) car classes also to the European countries that are not covered by TRACCS
#' and are filled with GCAM and UCD data.
#'
#' @author Johanna Hoppe
#' @return character vector of ISO country codes
#' @import data.table

toolEuropeanCountries <- function() {
  regionCode12 <- NULL

  regionMap <- fread(system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport",
                                 mustWork = TRUE))
  return(regionMap[regionCode12 %in% c("EUR", "NEU")]$countryCode)
}
