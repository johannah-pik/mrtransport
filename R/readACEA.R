#' Read ACEA vehicles on roads data
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("ACEA")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom readxl read_excel

readACEA <- function(subtype = c("historicalEnergyServiceDemand")) {
  switch(
    subtype,
    "historicalVehStock" = {
      
      cars <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "Cars", 
                   range = "A4:F37")
      )
      trucks <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "Trucks", 
                   range = "A4:F37")
      )
      buses <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "Buses", 
                   range = "A4:F35")
      )
      
      stock <- rbind(cars[, variable := "Stock|Transport|Pass|Road|LDV|Four Wheelers"], 
                     trucks[, variable := "Stock|Transport|Freight|Road"], 
                     buses[, variable := "Stock|Transport|Pass|Road|Bus"])

      measureVars <- names(stock)[!names(stock) %in% c("Country", "unit", "variable")]
      stock <- melt(stock, measure.vars = measureVars, variable.name = "period")
      setnames(stock, "Country", "region")
      stock <- stock[, value := value * 10^-6][, unit := "million veh"]
      setcolorder(stock, c("region", "period", "variable", "unit", "value"))
      stock <- stock[!region %in% c("EU-27", "EFTA")]
      MP <- as.magpie(
        stock,
        spatial   = "region",     # region column
        temporal  = "period",     # years
        datacol   = "value"       # numeric values
      )},
    "historicalVehStockTechShares" = {
      
      cars2023 <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "CarsByPowerSource2023", 
                   range = "A6:J36")
      )[, period := 2023]
      cars2024 <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "CarsByPowerSource2024", 
                   range = "A6:J36")
      )[, period := 2024]
      trucks2023 <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "TrucksByPowerSource2023", 
                   range = "A6:J36")
      )[, period := 2023]
      trucks2024 <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "TrucksByPowerSource2024", 
                   range = "A6:J36")
      )[, period := 2024]
      buses2023 <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "BussesByPowerSource2023", 
                   range = "A6:J36")
      )[, period := 2023]
      buses2024 <- as.data.table(
        read_excel("vehiclesOnEuropeanRoads.xlsx", 
                   sheet = "BussesByPowerSource2024", 
                   range = "A6:J36")
      )[, period := 2024]
      
      techShares <- rbind(cars2023[, variable := "Stock|Transport|Pass|Road|LDV|Four Wheelers"], 
                          cars2024[, variable := "Stock|Transport|Pass|Road|LDV|Four Wheelers"], 
                          trucks2023[, variable := "Stock|Transport|Freight|Road"], 
                          trucks2024[, variable := "Stock|Transport|Freight|Road"], 
                          buses2023[, variable := "Stock|Transport|Pass|Road|Bus"], 
                          buses2024[, variable := "Stock|Transport|Pass|Road|Bus"])
    
      measureVars <- names(techShares)[!names(techShares) %in% c("Country", "period", "variable")]
      techShares <- melt(techShares, measure.vars = measureVars, variable.name = "technologyACEA")
      setnames(techShares, "Country", "region")
      techShares[, value := as.numeric(as.character(value))][, unit := "%"]
      setcolorder(techShares, c("region", "period", "variable", "technologyACEA", "unit", "value"))
      techShares <- techShares[!region %in% c("EU-27", "EFTA")]
      
      MP <- as.magpie(
        techShares,
        spatial   = "region",     # region column
        temporal  = "period",     # years
        datacol   = "value"       # numeric values
      )
    }
  )

  return(MP)
}