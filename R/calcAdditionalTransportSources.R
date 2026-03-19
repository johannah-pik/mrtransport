#' Provide additional data sources for Transport validation
#' @author Johanna Hoppe
#' @param subtype one of the source parameters
#' @param FiveYearAverage switch to enable smoothing by calculating centered 5 years avergage
#' @import data.table
#' @importFrom rmndt approx_dt

calcAdditionalTransportSources <- function(subtype, FiveYearAverage = TRUE) { # nolint: cyclocomp_linter

  switch(
    subtype,
    "energyServiceDemand" = {
      unit <- "billion (p|t)km/yr"
      description <- "Ernergy service demand for different transport modes provided by EU pocketbook data"
      weight <- NULL

      historicalEnergyServiceDemand <- toolPrepareEUstatisticalPocketbook(readSource("EUstatisticalPocketbook", "historicalEnergyServiceDemand"))
      historicalEnergyServiceDemand[, scenario := "historical"][, model := "EUpocketbook"]
      historicalEnergyServiceDemand[subsectorL3 == "trn_pass_road_LDV_4W", variable := "ES|Transport|Pass|Road|LDV|Four Wheelers"]
      historicalEnergyServiceDemand[subsectorL2 == "Bus", variable := "ES|Transport|Pass|Road|Bus"]
      historicalEnergyServiceDemand[subsectorL1 == "HSR", variable := "ES|Transport|Pass|Rail|HSR"]
      historicalEnergyServiceDemand[subsectorL1 == "Passenger Rail", variable := "ES|Transport|Pass|Rail|non-HSR"]
      historicalEnergyServiceDemand[subsectorL1 == "trn_freight_road", variable := "ES|Transport|Freight|Road"]
      historicalEnergyServiceDemand[subsectorL1 ==  "Freight Rail", variable := "ES|Transport|Freight|Rail"]
      historicalEnergyServiceDemand[subsectorL1 ==  "Domestic Ship", variable := "ES|Transport|Freight|Domestic Shipping"]
      historicalEnergyServiceDemand[, c("subsectorL1", "subsectorL2", "subsectorL3") := NULL]
      quitteobj <- historicalEnergyServiceDemand
    },
    "vehicleStock" = {
      unit <- "million veh"
      description <- "Total vehicle stock for cars, trucks and busses. Sources: EU poket book, ACEA vehicles on roads"
      weight <- NULL

      ## vehicle stock data (total/not differentiated by technology)
      stockEUpocketBook <- toolPrepareEUstatisticalPocketbook(readSource("EUstatisticalPocketbook", "historicalVehStock"))
      stockEUpocketBook[, scenario := "historical"][, model := "EUpocketbook"]
      stockEUpocketBook[subsectorL3 == "trn_pass_road_LDV_4W", variable := "Stock|Transport|Pass|Road|LDV|Four Wheelers"]
      stockEUpocketBook[subsectorL2 == "Bus", variable := "Stock|Transport|Pass|Road|Bus"]
      stockEUpocketBook[subsectorL2 == "trn_freight_road", variable := "Stock|Transport|Freight|Road"]
      stockEUpocketBook[, c("subsectorL1", "subsectorL2", "subsectorL3") := NULL]

      ## vehicle stock data (total/not differentiated by technology)
      stockACEA <- magpie2dt(readSource("ACEA", "historicalVehStock"))
      stockACEA[, scenario := "historical"][, model := "ACEA"]

      stock <- rbind(stockEUpocketBook, stockACEA)
      quitteobj <- stock

    },
    "vehicleSales" = {
      unit <- "million veh"
      description <- "Total vehicle sales for cars, trucks and busses. Sources: EU poket book"
      weight <- NULL

      ## vehicle registrations
      sales <- toolPrepareEUstatisticalPocketbook(readSource("EUstatisticalPocketbook", "historicalVehSales"))
      sales[, scenario := "historical"][, model := "EUpocketbook"]
      sales[subsectorL3 == "trn_pass_road_LDV_4W", variable := "Sales|Transport|Pass|Road|LDV|Four Wheelers"]
      sales[subsectorL2 == "Bus", variable := "Sales|Transport|Pass|Road|Bus"]
      sales[subsectorL2 == "trn_freight_road", variable := "Sales|Transport|Freight|Road"]
      sales[, c("subsectorL1", "subsectorL2", "subsectorL3") := NULL]
      quitteobj <- sales
    },
    "vehicleStockTechShares" = {
      unit <- "-"
      description <- "Technology shares in car stock: ACEA, European Alternative Fuels Observatory"
      weight <- as.magpie(magpie2dt(readSource("ACEA", "historicalVehStock"))[, c("unit", "variable") := NULL])

      ## vehicle stock data technology shares
      stockSharesACEA <-  toolPrepareACEA((readSource("ACEA", "historicalVehStockTechShares")))
      stockSharesACEA[, value := value * 10^-2]
      stockSharesACEA[, variable := paste0("Share|", variable, "|", technology)][, technology := NULL]
      stockSharesACEA[, scenario := "historical"][, model := "ACEA"]

      stockSharesEAFO <- toolPrepareEuropeanAlternativeFuelsObservatory(readSource("EuropeanAlternativeFuelsObservatory", "historicalVehStockTechShares"))
      stockSharesEAFO[, value := value * 10^-2]
      stockSharesEAFO[, variable := paste0("Share|Stock|Transport|Pass|Road|LDV|Four Wheelers|", technology)][, technology := NULL]
      stockSharesEAFO[, scenario := "historical"][, model := "European Alternative Fuels Observatory"]

      stockShares <- rbind(stockSharesEAFO, stockSharesACEA)
      quitteobj <- stockShares
    },
    "vehicleSalesTechShares" = {
      unit <- "-"
      description <- "Technology shares in car sales: European Alternative Fuels Observatory"
      weight <- magpie2dt(readSource("ACEA", "historicalVehStock"))[, c("unit", "variable") := NULL]
      weight <- as.magpie(weight[period == 2024][, period := NULL])


      ## vehicle sales technology shares
      salesShares <- toolPrepareEuropeanAlternativeFuelsObservatory(readSource("EuropeanAlternativeFuelsObservatory", "historicalVehSalesTechShares"))
      salesShares[, value := value * 10^-2]
      salesShares[, variable := paste0("Share|Sales|Transport|Pass|Road|LDV|Four Wheelers|", technology)][, technology := NULL]
      salesShares[, scenario := "historical"][, model := "European Alternative Fuels Observatory"]
      quitteobj <- salesShares
    }
  )

  if (FiveYearAverage) {
    # Smoothing data by calculating centered 5 year average
    quitteobjAverage <- approx_dt(quitteobj, c(1965:1:2030), "period", "value",
                                c("scenario", "model", "region", "variable", "unit"), extrapolate = TRUE)
    setorder(quitteobjAverage, scenario, model, region, variable, unit, period)

    quitteobjAverage[, avg5ycentered := frollmean(value, n = 5, align = "center"),
                   by = .(scenario, model, region, variable, unit)][, value := NULL]
    # keep only the original timesteps
    quitteobj <- merge(quitteobj, quitteobjAverage, by = intersect(names(quitteobj), names(quitteobjAverage)), all.x = TRUE)
    quitteobj[, value := NULL]
    setnames(quitteobj, "avg5ycentered", "value")
  }


  x <- as.magpie(as.data.frame(quitteobj))

  return(list(
    x           = x,
    weight      = weight,
    unit        = unit,
    description = description,
    aggregationFunction = "toolAggregateVehicleTypes"
  ))
}
