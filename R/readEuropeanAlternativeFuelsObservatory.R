#' Read European Alternative Fuels Observatory data
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("EEA")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom readxl read_excel

readEuropeanAlternativeFuelsObservatory <- function(subtype = c("historicalVehStockTechShares")) {
  switch(
    subtype,
    "historicalVehStockTechShares" = {
      
      carsTechShares <- as.data.table(
        read_excel("alternativeFuelCarShares.xlsx", 
                   sheet = "AF cars fleet share", 
                   range = "A5:F37")
      )[, period := 2024][, unit := "%"][, variable := "vehicle stock share"]
      
      measureVars <- names(carsTechShares)[!names(carsTechShares) %in% c("Country", "variable", "unit", "period")]
      carsTechShares <- melt(carsTechShares, measure.vars = measureVars, variable.name = "technologyEUAFO")
      setnames(carsTechShares, "Country", "region")
      carsTechShares[, value := as.numeric(as.character(value))]
      setcolorder(carsTechShares, c("region", "period", "variable", "technologyEUAFO", "unit", "value"))
      
      MP <- as.magpie(
        carsTechShares,
        spatial   = "region",     # region column
        temporal  = "period",     # years
        datacol   = "value"       # numeric values
      )},
    "historicalVehSalesTechShares" = {
      
      carsTechShares <- as.data.table(
        read_excel("alternativeFuelCarShares.xlsx", 
                   sheet = "AF cars sales share", 
                   range = "A5:F37")
      )[, period := 2025][, unit := "%"][, variable := "vehicle sales share"]
      
      measureVars <- names(carsTechShares)[!names(carsTechShares) %in% c("Country", "variable", "unit", "period")]
      carsTechShares <- melt(carsTechShares, measure.vars = measureVars, variable.name = "technologyEUAFO")
      setnames(carsTechShares, "Country", "region")
      carsTechShares[, value := as.numeric(as.character(value))]
      setcolorder(carsTechShares, c("region", "period", "variable", "technologyEUAFO", "unit", "value"))
      
      MP <- as.magpie(
        carsTechShares,
        spatial   = "region",     # region column
        temporal  = "period",     # years
        datacol   = "value"       # numeric values
      )
    }
  )
  return(MP)
}