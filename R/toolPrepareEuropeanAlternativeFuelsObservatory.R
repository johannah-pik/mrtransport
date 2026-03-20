#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories.
#'
#' @author Johanna Hoppe
#' @param x the input data read via readSource, a magpie objec
#' @return a quitte object
#'
#' @import data.table
#' @importFrom rmndt magpie2dt

toolPrepareEuropeanAlternativeFuelsObservatory <- function(x) {

  dataTable <- magpie2dt(x)
  mapfile <- system.file("extdata", "mappingEUAFOtoEDGET.csv", package = "mrtransport", mustWork = TRUE)
  mapping <- fread(mapfile, skip = 0)
  mappedData <- merge(dataTable, mapping, by = "technologyEUAFO")
  mappedData <- mappedData[!value == ""]
  mappedData <- mappedData[!technology == ""]
  mappedData <- mappedData[, .(value = sum(value)), by = c("region", "period", "variable", "technology", "unit")]

  return(mappedData)
}
