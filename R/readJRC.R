#' Read EU Joint Research Center data.
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("EurostatEnergyCountryDataSheets")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom magclass as.magpie
#' @importFrom readxl read_excel

readJRC <- function(subtype = c("energyServiceDemand", "energyServiceDemandTechnologyLevel")) {
  Eurostatsector <- variable <- region <- NULL

  countries <- c(
    "BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY",
    "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE"
  )

  switch(subtype,
    "energyServiceDemand" = {
      dt <- do.call("rbind", lapply(
        countries,
        function(x) {
          output <- suppressMessages(data.table(read_excel(
            path = file.path(x, paste0("JRC-IDEES-2023_Transport", "_", x, ".xlsx")),
            sheet = "Transport", "A1:Y31"
          )))
          output <- output[-c(1,2)]
          output <- output[c(1:13), unit := "million pkm"]
          output <- output[c(14:28), unit := "million tkm"]
          setnames(output, 1, "JRCtransportMode")
          # Delete totals
          totals <- c("Passenger transport (mio pkm)", "Road transport", "Rail, metro and tram", "Aviation", "Freight transport (mio tkm)",
                      "Domestic navigation", "International maritime bunkers (mio tkm)")
          output <- output[!JRCtransportMode %in% totals]
          # Refine ambiguous entries
          output[JRCtransportMode == "Domestic", JRCtransportMode := "Domestic aviation"]
          output[grepl("International.*", JRCtransportMode), JRCtransportMode := paste0(JRCtransportMode, " aviation")]
          output[JRCtransportMode == "Intra-EEA" | JRCtransportMode == "Extra-EEA", JRCtransportMode := paste0(JRCtransportMode, " international maritime")]
          output <- melt(output, id.vars = c("JRCtransportMode", "unit"), variable.name = "period")
          output[, region := x]
          return(output)
        }
      ))

      # Bring to quitte column order
      dt[, variable := "Energy service demand"]
      dt <- dt[, c("region", "JRCtransportMode", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
      return(x)
    },
    "energyServiceDemandTechnologyLevel" = {
      dt <- do.call("rbind", lapply(
        countries,
        function(x) {
          output <- suppressMessages(data.table(read_excel(
            path = file.path(x, paste0("JRC-IDEES-2023_Transport", "_", x, ".xlsx")),
            sheet = "TrRoad_act", "A1:Y18"
          )))
          output <- output[-c(1:4)]
          output <- output[, unit := "million pkm"]
          setnames(output, 1, "JRCtechnology")
          output <- output[c(2:7), JRCtransportMode := "Passenger cars"]
          output <- output[c(9:13), JRCtransportMode := "Motor coaches, buses and trolley buses"]
          # Delete totals
          totals <- c("Passenger cars", "Road transport", "Motor coaches, buses and trolley buses")
          output <- output[!JRCtechnology %in% totals]
          output <- melt(output, id.vars = c("JRCtransportMode", "JRCtechnology", "unit"), variable.name = "period")
          output[, region := x]
          return(output)
        }
      ))
      # Bring to quitte column order
      dt[, variable := "Energy service demand"]
      dt <- dt[, c("region", "JRCtransportMode", "JRCtechnology", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
      return(x)
    }
  )
}
