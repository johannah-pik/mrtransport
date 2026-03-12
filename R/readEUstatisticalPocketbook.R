#' Read EU statistical pocketbook transport data
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("EUstatisticalPocketbook")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom readxl read_excel

readEUstatisticalPocketbook <- function(subtype = c("historicalEnergyServiceDemand", 
                                                    "historicalVehStock", 
                                                    "historicalVehSales")) {

  switch(
    subtype,
    "historicalEnergyServiceDemand" = {
    # energy service demand in pkm/yr for passenger transport and tkm/yr in freight transport
      
    # Read in sheets from pocket book data
    # Detailed notes upon the data are provided in each sheet in the xls files in the source folder
     sheets <- c("cars", "bus_coach", "tram and metro", "rail_pkm")

     passengerData <- rbindlist(lapply(sheets, function(x){
       
       modeData <- as.data.table(
         read_excel("pb2025-section23.xlsx", 
                    sheet = x, 
                    range = "B4:AL46")
       )
       
       # Just rename the first column from ...1 to "region"
       setnames(modeData, "...1", "region")
       modeData <- modeData[!is.na(region)][, unit := "billion pkm/yr"]
       measureVars <- names(modeData)[!names(modeData) %in% c("region", "unit")]
       modeData <- melt(modeData, measure.vars = measureVars, variable.name = "period")
       modeData[, transportMode := paste0("Pass ", x)]
     })) 

     hsr <- as.data.table(read_excel("pb2025-section23.xlsx", 
                            sheet = "hs_rail_r", 
                            range = "C4:Q38"))
     
     # Just rename the first column from ...1 to "region"
     setnames(hsr, "...1", "period")
     hsr <- hsr[!is.na(period)][, unit := "billion pkm/yr"]
     measureVars <- names(hsr)[!names(hsr) %in% c("period", "unit")]
     hsr <- melt(hsr, measure.vars = measureVars, variable.name = "region")
     hsr[, transportMode := paste0("Pass ", "hs_rail_r")]
     passengerData <- rbind(passengerData, hsr)
       
     sheets <- c("road_ter", "rail_tkm", "iww")
     ranges <- c("B5:U38", "B5:AL46", "B4:AL45")
     freightData <- data.table()
     for (i in 1:3) {
    
       modeData <- as.data.table(
         read_excel("pb2025-section22.xlsx", 
                    sheet = sheets[i], 
                    range = ranges[i])
       )
       
       # Just rename the first column from ...1 to "region"
       setnames(modeData, "...1", "region")
       modeData <- modeData[!is.na(region)][, unit := "billion tkm/yr"]
       measureVars <- names(modeData)[!names(modeData) %in% c("region", "unit")]
       modeData <- melt(modeData, measure.vars = measureVars, variable.name = "period")
       modeData[, transportMode := paste0("Freight ", sheets[i])]
       freightData <- rbind(freightData, modeData)
     } 
    
     data <- rbind(passengerData, freightData)
     data[, period := droplevels(period)]
     data <- data[!(is.na(value))][, variable := "Energy service demand"]
     data <- data[!value == "-"]

     data[, value := as.numeric(value)]
     setcolorder(data, c("region", "period", "variable", "transportMode", "unit", "value"))
     data <- data[!region == "EU-27"]
     
     MP <- as.magpie(
       data,
       spatial   = "region",     # region column
       temporal  = "period",     # years
       datacol   = "value"       # numeric values
     )},
    "historicalVehStock" = {

      # total vehicle stock data in million vehicles
      sheets <- c("stock_cars", "stock_busses", "stock_goods")
      ranges <- c("B5:AL48", "B5:AL48", "B5:AL49")
      fleetList <- list()
      for (i in 1:3) {
              
        vehicleData <- as.data.table(
          read_excel("pb2025-section26.xlsx", 
                     sheet = sheets[i], 
                     range = ranges[i])
        )
        # Just rename the first column from ...1 to "region"
        setnames(vehicleData, "...1", "region")
        vehicleData <- vehicleData[!is.na(region)][, unit := "thousand vehicles"]
        measureVars <- names(vehicleData)[!names(vehicleData) %in% c("region", "unit")]
        # The 2017 value for GE is erroneous (there is just an ":") in the excel data source
        if (i == 2) vehicleData[region == "GE", "2017" := NA]
        vehicleData <- melt(vehicleData, measure.vars = measureVars, variable.name = "period")
        vehicleData <- vehicleData[!is.na(value), value := as.double(value) * 10^-3][, unit := "million vehicles"]
        
        vehicleData[, transportMode := sheets[i]]
        vehicleData[, `:=`(
          region = as.character(region),
          unit = as.character(unit), 
          period = as.character(period),
          value = as.numeric(value),
          transportMode = as.character(transportMode)
        )]
        
        setcolorder(vehicleData, c("region", "unit", "period", "value", "transportMode"))
        fleetList[[i]] <- vehicleData
      }
      fleetData <- rbindlist(fleetList, use.names = TRUE)
      fleetData <- fleetData[!is.na(value)][, variable := "Vehicle stock"]
      setcolorder(fleetData, c("region", "period", "variable", "transportMode", "unit", "value"))
      fleetData <- fleetData[!region == "EU-27"]
      MP <- as.magpie(
        fleetData,
        spatial   = "region",     # region column
        temporal    = "period",     # years
        datacol     = "value"       # numeric values
      )},
    "historicalVehSales" = {

      # total vehicle sales data in million vehicles
      sheets <- c("car_reg_new", "com_reg", "bus_reg")
      newCarRegistrations <- as.data.table(
          read_excel("pb2025-section26.xlsx", 
                     sheet = "car_reg_new", 
                     range = "B5:AC47")
        )
        
      # Just rename the first column from ...1 to "region"
      setnames(newCarRegistrations, "...1", "region")
      newCarRegistrations <- newCarRegistrations[!is.na(region)][, unit := "thousand vehicles"]
      measureVars <- names(newCarRegistrations)[!names(newCarRegistrations) %in% c("region", "unit")]
      newCarRegistrations <- melt(newCarRegistrations, measure.vars = measureVars, variable.name = "period")
      newCarRegistrations <- newCarRegistrations[, value := value * 10^-3][, unit := "million vehicles"]
      newCarRegistrations[, transportMode := "car_reg_new"]
      
      newBusRegistrations <- as.data.table(
        read_excel("pb2025-section26.xlsx", 
                   sheet = "bus_reg", 
                   range = "B6:W48")
      )
      
      # rename the first column from ...1 to "region"
      setnames(newBusRegistrations, "...1", "region")
      newBusRegistrations <- newBusRegistrations[!is.na(region)][, unit := "thousand vehicles"]
      measureVars <- names(newBusRegistrations)[!names(newBusRegistrations) %in% c("region", "unit")]
      newBusRegistrations <- melt(newBusRegistrations, measure.vars = measureVars, variable.name = "period")
      newBusRegistrations <- newBusRegistrations[, value := value * 10^-3][, unit := "million vehicles"]
      newBusRegistrations[, transportMode := "bus_reg"]
      
      newTruckRegistrations <- as.data.table(
        read_excel("pb2025-section26.xlsx", 
                   sheet = "com_reg", 
                   range = "CJ6:CW48")
      )

      # rename the first column from ...1 to "region"
      setnames(newTruckRegistrations, "...14", "region")
      newTruckRegistrations[, c("...13") := NULL]
      newTruckRegistrations <- newTruckRegistrations[!is.na(region)][, unit := "thousand vehicles"]
      measureVars <- names(newTruckRegistrations)[!names(newTruckRegistrations) %in% c("region", "unit")]
      newTruckRegistrations <- melt(newTruckRegistrations, measure.vars = measureVars, variable.name = "period")
      newTruckRegistrations <- newTruckRegistrations[!is.na(value), value := as.numeric(value) * 10^-3][, unit := "million vehicles"]
      newTruckRegistrations[, transportMode := "com_reg"]
      
      newRegistrations <- rbind(newCarRegistrations, newTruckRegistrations, newBusRegistrations) 
      newRegistrations <- newRegistrations[!is.na(value)][, variable := "Vehicle sales"]
      setcolorder(newRegistrations, c("region", "period", "variable", "transportMode", "unit", "value"))
      newRegistrations <- newRegistrations[!region == "EU-27"][, value := as.numeric(value)]
      MP <- as.magpie(
        newRegistrations,
        spatial   = "region",     # region column
        temporal    = "period",     # years
        datacol     = "value"       # numeric values
      )}
  )
  return(MP)
}
