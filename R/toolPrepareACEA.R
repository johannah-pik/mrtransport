#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. 
#'
#' @author Johanna Hoppe
#' @param x the input data read via readSource, a magpie object
#' @param subtype one of the different EDGE-T inputdata subtypes
#' @return a quitte object
#'
#' @import data.table
#' @importFrom rmndt magpie2dt

toolPrepareACEA <- function(x) {

  dataTable <- magpie2dt(x)
  mapfile <- system.file("extdata", "mappingACEAToEDGET.csv", package = "mrtransport", mustWork = TRUE)
  mapping <- fread(mapfile, skip = 0)
  mappedData <- merge(dataTable, mapping, by = "technologyACEA")
  mappedData <- mappedData[!technology == ""]
  mappedData <- mappedData[, .(value = sum(value)), by = c("region", "period", "variable", "unit", "technology")]
  
  return(mappedData)
}