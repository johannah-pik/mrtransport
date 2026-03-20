#' Provide additional data sources for Transport validation
#' @author Johanna Hoppe
#' @param subtype one of the source parameters: "energyServiceDemand", "vehicleStock", "vehicleSales", "vehicleStockTechShares", "vehicleSalesTechShares"
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

      energyServiceDemandEUpocketbook <- toolPrepareEUstatisticalPocketbook(readSource("EUstatisticalPocketbook", "historicalEnergyServiceDemand"))
      energyServiceDemandEUpocketbook[, scenario := "historical"][, model := "EUpocketbook"]
      energyServiceDemandEUpocketbook[subsectorL3 == "trn_pass_road_LDV_4W", variable := "ES|Transport|Pass|Road|LDV|Four Wheelers"]
      energyServiceDemandEUpocketbook[subsectorL2 == "Bus", variable := "ES|Transport|Pass|Road|Bus"]
      energyServiceDemandEUpocketbook[subsectorL1 == "HSR", variable := "ES|Transport|Pass|Rail|HSR"]
      energyServiceDemandEUpocketbook[subsectorL1 == "Passenger Rail", variable := "ES|Transport|Pass|Rail|non-HSR"]
      energyServiceDemandEUpocketbook[subsectorL1 == "trn_freight_road", variable := "ES|Transport|Freight|Road"]
      energyServiceDemandEUpocketbook[subsectorL1 ==  "Freight Rail", variable := "ES|Transport|Freight|Rail"]
      energyServiceDemandEUpocketbook[subsectorL1 ==  "Domestic Ship", variable := "ES|Transport|Freight|Domestic Shipping"]
      energyServiceDemandEUpocketbook[, c("subsectorL1", "subsectorL2", "subsectorL3") := NULL]

      energyServiceDemandJRC <- toolPrepareJRC(readSource("JRC", "energyServiceDemand"), "energyServiceDemand")[, scenario := "historical"][, model := "JRC"]
      energyServiceDemandJRC[subsectorL3 == "trn_pass_road_LDV_4W", variable := "ES|Transport|Pass|Road|LDV|Four Wheelers"]
      energyServiceDemandJRC[subsectorL3 == "trn_pass_road_LDV_2W", variable := "ES|Transport|Pass|Road|LDV|Two Wheelers"]
      energyServiceDemandJRC[subsectorL3 == "Bus_tmp_subsector_L3", variable := "ES|Transport|Pass|Road|Bus"]
      energyServiceDemandJRC[subsectorL3 == "HSR_tmp_subsector_L3", variable := "ES|Transport|Pass|Rail|HSR"]
      energyServiceDemandJRC[subsectorL3 ==  "Domestic Aviation_tmp_subsector_L3", variable := "ES|Transport|Pass|Domestic Aviation"]
      energyServiceDemandJRC[subsectorL3 ==  "International Aviation_tmp_subsector_L3", variable := "ES|Transport|Bunkers|Pass|Aviation"]
      energyServiceDemandJRC[subsectorL3 == "Passenger Rail_tmp_subsector_L3", variable := "ES|Transport|Pass|Rail|non-HSR"]
      energyServiceDemandJRC[subsectorL3 == "trn_freight_road_tmp_subsector_L3", variable := "ES|Transport|Freight|Road"]
      energyServiceDemandJRC[subsectorL3 ==  "Freight Rail_tmp_subsector_L3", variable := "ES|Transport|Freight|Rail"]
      energyServiceDemandJRC[subsectorL3 ==  "Domestic Ship_tmp_subsector_L3", variable := "ES|Transport|Freight|Domestic Shipping"]
      energyServiceDemandJRC[subsectorL3 ==  "International Ship_tmp_subsector_L3", variable := "ES|Transport|Bunkers|Freight|Shipping"]

      #aggregate data to small-medium distance top nodes
      energyServiceDemandPassSMD <- energyServiceDemandJRC[subsectorL3 %in% c("trn_pass_road_LDV_4W", "trn_pass_road_LDV_2W", "Bus_tmp_subsector_L3",
                                                                            "HSR_tmp_subsector_L3", "Domestic Aviation_tmp_subsector_L3", "Passenger Rail_tmp_subsector_L3"),
                                                           .(value = sum(value)), by = c("region", "period", "unit", "value")][, variable := "ES|Transport edge|Pass"]
      energyServiceDemandFreightSMD <- energyServiceDemandJRC[subsectorL3 %in% c("trn_freight_road_tmp_subsector_L3", "Freight Rail_tmp_subsector_L3", "Bus_tmp_subsector_L3",
                                                                            "HSR_tmp_subsector_L3", "Domestic Aviation_tmp_subsector_L3", "Domestic Ship_tmp_subsector_L3"),
                                                           .(value = sum(value)), by = c("region", "period", "unit", "value")][, variable := "ES|Transport edge|Freight"]

      energyServiceDemandJRCTech <- toolPrepareJRC(readSource("JRC", "energyServiceDemandTechnologyLevel"), "energyServiceDemandTechnologyLevel")[, scenario := "historical"][, model := "JRC"]
      #check whether data sums up
      summationCheck <- energyServiceDemandJRCTech[, .(value = sum(value)), by = c("region", "period", "subsectorL3", "unit")]
      setnames(summationCheck, "value", "check")
      summationCheck <- merge(summationCheck, energyServiceDemandJRC, by = c("region", "period", "subsectorL3", "unit"))
      summationCheck[, test := abs(value - check)]
      if (sum(summationCheck$test) > 0.001) stop("Detailed JRC technology data does not match reporting for aggregated transport modes")
      energyServiceDemandJRCTech[subsectorL3 == "trn_pass_road_LDV_4W", variable := paste0("ES|Transport|Pass|Road|LDV|Four Wheelers|", technology)]
      energyServiceDemandJRCTech[subsectorL3 == "Bus_tmp_subsector_L3", variable := paste0("ES|Transport|Pass|Road|Bus|", technology)]

      energyServiceDemand <- rbind(energyServiceDemandJRC[, c("subsectorL3") := NULL], energyServiceDemandJRCTech[, c("subsectorL3", "technology") := NULL])
      setcolorder(energyServiceDemand, c("region", "period", "variable", "unit", "value"))
      quitteobj <- energyServiceDemand
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

  browser()
  quitteobj
  x <- as.magpie(as.data.frame(quitteobj))
  x <- suppressMessages(toolCountryFill(x, fill = NA))

  return(list(
    x           = x,
    weight      = weight,
    unit        = unit,
    description = description,
    aggregationFunction = "toolAggregateVehicleTypes"
  ))
}
