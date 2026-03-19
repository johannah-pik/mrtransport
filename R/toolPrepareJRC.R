#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full structure of the decision tree.
#'
#' @author Johanna Hoppe
#' @param x the input data read via readSource, a magpie object
#' @return a quitte object

#' @import data.table
#' @importFrom rmndt magpie2dt
#' @export

toolPrepareJRC <- function(x) {
  region <- JRCtechnology <- JRCtransportMode <- period <-
    technology <- subsectorL3 <- variable <- unit <- period <- NULL


  if (subtype == "energyServiceDemand") {
    mapping <- fread(system.file("extdata", "mappingJRCtoEDGET.csv", package = "mrtransport", mustWork = TRUE))
    dt <- magpie2dt(x)
    dt <- merge.data.table(dt, mapping, all.x = TRUE, allow.cartesian = TRUE, by = "JRCtransportMode")
    dt <- dt[, .(value = sum(value)), c("region", "period", "subsectorL3", "variable", "unit")]
    setcolorder(dt, c("region", "period", "subsectorL3", "technology", "variable", "unit"))
  } else if (subtype == "energyServiceDemandTechnologyLevel") {
    mappingModes <- fread(system.file("extdata", "mappingJRCtoEDGET.csv", package = "mrtransport", mustWork = TRUE))
    mappingTechnologies <- fread(system.file("extdata", "mappingJRCtoEDGET.csv", package = "mrtransport", mustWork = TRUE))
    dt <- magpie2dt(x)
    dt <- merge.data.table(dt, mappingModes, all.x = TRUE, allow.cartesian = TRUE, by = "JRCtransportMode")
    dt <- merge.data.table(dt, mappingTechnologies, all.x = TRUE, allow.cartesian = TRUE, by = "JRCtechnology")
  }






  if (anyNA(dt) == TRUE) {
    stop("Eurostat data contains NAs")
  }
  return(dt)
}
