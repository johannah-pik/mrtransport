#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param mapIso2region map iso countries to regions
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' @param histSourceData the full source data containing the annual mileage and load factor
#' univocalNames
#' @return a quitte object

toolAdjustEsDemand <- function(dt, mapIso2region, completeData, filter, histSourceData) {
  variable <- period  <- unit <- value <-  demldv <- regionCode21 <-
    regionCode12 <- region <- univocalName <- NULL

  dt <- merge.data.table(dt, completeData[period <= 2010],
                         by = c("region", "period", "univocalName", "technology"), all = TRUE)
  # completeData does not contain unit so it needs to be added
  dt[univocalName %in% c(filter$trn_pass, "International Aviation"), unit := "billion pkm/yr"]
  dt[univocalName %in% c(filter$trn_freight, "International Ship"), unit := "billion tkm/yr"]
  dt[is.na(value), variable := "ES"]
  dt <- merge.data.table(dt, mapIso2region, by = "region", all.x = TRUE, allow.cartesian = TRUE)

  # 1: Some Truck types, Rail, alternative technologies and active modes are lacking energy service demand data
  # The missing modes get a zero demand for now. After the regional aggregation, the zero demand remains only
  # for alternative technologies
  dt[is.na(value), value := 0]



  #2: Add some base demand for Cycle & Walk (2%)
  dt[, demldv := sum(value), by = c("period", "region")]
  dt[univocalName == "Cycle" & value == 0, value := demldv * 0.01]
  dt[univocalName == "Walk" & value == 0, value := demldv * 0.002]
  dt[univocalName == "Cycle" & value == 0 & regionCode21 %in% c("USA", "AUS", "CAN"), value := demldv * 0.006]
  dt[univocalName == "Cycle" & value == 0 & regionCode21 %in% c("IND", "CHN"), value := demldv * 0.02]
  dt[, demldv := NULL]

  #3: Correct demand for CHN
  ## the category "truck > 14t" in China does most likely contain also heavy trucks
  ## otherwise there are none
  dt[region %in% c("CHN", "HKG", "MAC"), value := ifelse(univocalName == "Truck (26t)",
                                                         value[univocalName == "Truck (18t)"] / 4,
                                                         value),
     by = c("period", "region", "technology")]
  dt[region %in% c("CHN", "HKG", "MAC"), value := ifelse(univocalName == "Truck (40t)",
                                                         value[univocalName == "Truck (18t)"] / 4,
                                                         value),
     by = c("period", "region", "technology")]
  dt[region %in% c("CHN", "HKG", "MAC") & univocalName == "Truck (18t)", value := value / 2,
     by = c("period", "region", "technology")]


  ######## new China truck size adjustment from Robert

  ## Adjustments on truck size classes in CHN region according to newer data.
  ## This data is based on downscaled values from CEIC data, https://www.ceicdata.com/en/china/no-of-motor-vehicle/cn-no-of-motor-vehicle-truck-heavy
  ## It is further split to EDGE-T size classes : 40t	5%, 26t	9%, 18t	11%, 7.5t	14%, 0-3.5 t 61%
  ## The original data and the further processing can be found in the transport folder in the owncloud:
  ## "Data/RegionalData/compiling_CHA_data_heavy_duty_vehicles.xlsx", cells V17:V21

  # First step: define target vehicle shares by size:
  VehSharesTargetSize <- data.table(univocalName = c("Truck (0-3_5t)","Truck (18t)","Truck (26t)","Truck (40t)","Truck (7_5t)"),
                                    region = "CHN",
                                    variable = "Share_in_Vehicles",
                                    unit = "Percent",
                                    value = c(61,11,9,5,14))


  # ## For debugging: first create safe-copy to later check if only the wanted rows are changed. Outcomment for debugging.
  dt_safecopy <- copy(dt)

  # extract existing ES totals for regions and trucks of interest
  histESdemandCHA <- dt[region %in% c("CHN") & univocalName %like% "Truck"]

  histESdemandCHAold <- histESdemandCHA[    # drop variable and unit
    , .(value = sum(value)),
    by = .(region, univocalName, technology, period )
  ]

  histESdemandCHAoldSize <- histESdemandCHAold[
    , .(oldES = sum(value)),
    by = .(region, univocalName, period)
  ]

  histESdemandCHAoldTotal <- histESdemandCHAold[
    , .(total_ES = sum(value)),
    by = .(region, period)
  ]

  # calculate new ES splits

  ## First calculate total tkm per vehicle from annual mileage and load factors.
  ## Take the values for "Liquids", as that is the most common technology in 2010

  annualTkmPerVehicle <- histSourceData$annualMileage[region %in% c("CHN") & univocalName %like% "Truck" & period == 2010 & technology == "Liquids",
                                          .(annualMileage = value), by = .(region, univocalName)]

  annualTkmPerVehicle <- merge(annualTkmPerVehicle, histSourceData$loadFactor[region %in% c("CHN") & univocalName %like% "Truck" & period == 2010 & technology == "Liquids",
                                                                    .(loadFactor = loadFactor), by = .(region, univocalName)],
                               by = c("region", "univocalName"))

  annualTkmPerVehicle[, ESperVeh := annualMileage * loadFactor ]


  ## calculate resulting ES values by size
  ESSharesTargetSize <- merge(VehSharesTargetSize, annualTkmPerVehicle, by = c("region", "univocalName") )

  ESSharesTargetSize[ , ESsharesUnnormalized := value / 100 * ESperVeh ]

  ESSharesTargetSize[ , ESsharesNormalized := ESsharesUnnormalized / sum(ESsharesUnnormalized), by = region ]

  ESSharesTargetSizeBack <- ESSharesTargetSize[ , .(univocalName = univocalName ,
                                                    region = region,
                                                    variable = "TargetShareES",
                                                    unit = "Percent",
                                                    value = ESsharesNormalized * 100)
  ]

  # calculate target ES per vehicle size with old ES totals
  ## First create new DT with period x univocalname size, as SizeSharesTarget has no period, and histESdemandCHAoldTotal no size

  histESdemandCHAtargetPerSize <- CJ(
    period = histESdemandCHAoldTotal$period,
    univocalName = ESSharesTargetSizeBack$univocalName,
    unique = TRUE
  )[
    ESSharesTargetSizeBack[, .(univocalName, ESshare = value / 100)],
    on = "univocalName"
  ][
    histESdemandCHAoldTotal,
    on = "period"
  ]


  histESdemandCHAtargetPerSize[ , targetES := ESshare * total_ES ]

  ## calculate scaling factors for each size
  ESscaling <- merge(histESdemandCHAoldSize, histESdemandCHAtargetPerSize, by = c("period", "univocalName", "region") )

  ESscaling[ , scaling := targetES / oldES ]

  ## rescale using old ES totals:
  histESdemandCHAnewES <- merge(histESdemandCHAold, ESscaling, by = c("period", "univocalName", "region") )

  histESdemandCHAnewES[, value := value * scaling ]

  ## drop unused columns, add variable and unit
  histESdemandCHAnewES[, c("oldES", "ESshare", "total_ES", "targetES", "scaling") := NULL][, ':='(variable = "ES", unit = "billion tkm/yr")]

  ## check that the total new ES and the total old ES are unchanged:

  setkey(histESdemandCHAnewES,region)
  setkey(histESdemandCHAold,region)

  stopifnot(
    all.equal(
      histESdemandCHAnewES[, sum(value), by = period],
      histESdemandCHAold[, sum(value), by = period]
    )
  )

  ## update the original dt
  ## the "value := i.value" formulation overwrites only the rows in dt that are contained in histESdemandCHAnewES, and keeps everyhting else unchanged:

  joinCols <- c("region", "univocalName", "technology", "variable","unit", "period")

  dt[
    histESdemandCHAnewES,
    on = joinCols,
    value := i.value
  ]

  # ## For debugging: check which values were changed:
  # # Outcomment for debugging.
  #
  # key_cols <- c("period", "region", "univocalName", "technology", "variable")
  #
  # dt_diff <- dt[
  #   dt_safecopy,
  #   on = key_cols,
  #   nomatch = 0,
  #   .(
  #     period,
  #     region,
  #     univocalName,
  #     technology,
  #     variable,
  #     value_new = value,
  #     value_old = i.value
  #   )
  # ]
  # dt_changed <- dt_diff[value_new != value_old]

  ###### also adjust car ES demands upwards to better reflect car stock numbers in 2010 (~62 mio, eg IEA GEVO and others - see file in the owncloud
  ## "Data/RegionalData/compiling_CHA_data_LDV.xlsx",
  ## Simply increasing the ES values means that more cars are on the road, but also that 2010 FE demand BEFORE the IEA calibration is higher - thus preventing
  ## the strong upscaling of energy intensities during the IEA FE calibration that was previously the case.

  carTypes <- c("Compact Car", "Large Car", "Large Car and SUV", "Midsize Car", "Mini Car", "Subcompact Car","Van")

  dt[univocalName %in% carTypes & region == "CHN" , value := 3 * value]
  # dt[univocalName %in% carTypes & region == "CHN" & period == 2009, value := 1.9 * value]
  # dt[univocalName %in% carTypes & region == "CHN" & period == 2008, value := 1.8 * value]
  # dt[univocalName %in% carTypes & region == "CHN" & period == 2007, value := 1.7 * value]
  # dt[univocalName %in% carTypes & region == "CHN" & period == 2006, value := 1.6 * value]
  # dt[univocalName %in% carTypes & region == "CHN" & period <= 2005, value := 1.5 * value]

  ############ end of new CHA stuff from Robert

  dt[region %in% c("USA", "PRI", "UMI", "VIR"), value := ifelse(univocalName == "Truck (26t)",
                                                                value[univocalName == "Truck (18t)"] / 3,
                                                                value),
     by = c("period", "region", "technology")]
  dt[region %in% c("USA", "PRI", "UMI", "VIR"), value := ifelse(univocalName == "Truck (40t)",
                                                                value[univocalName == "Truck (18t)"] / 3,
                                                                value),
     by = c("period", "region", "technology")]
  dt[region %in% c("USA", "PRI", "UMI", "VIR") & univocalName == "Truck (18t)", value := value / 3,
     by = c("period", "region", "technology")]

  #from https://www.iea.org/reports/tracking-rail-2020-2
  dt[period <= 2010 & regionCode21 == "CHN" & univocalName == "HSR", value := 70000]
  # from https://theicct.org/sites/default/files/China_Freight_Assessment_English_20181022.pdf
  # total road freight demand seems to be around 5 billion tkm * 0.8, a factor 3 roughly
  dt[period <= 2010 & regionCode21 == "CHN" & univocalName %in% filter$trn_freight_road, value := value * 3]

  #4: Demand level corrections, adjusting to ETP demands
  dt[regionCode21 == "CHA" & univocalName == "Bus", value := value / 2.5]
  dt[regionCode21 == "IND" & univocalName == "Bus", value := value / 2]
  dt[regionCode21 == "OAS" & univocalName == "Bus", value := value / 5]
  dt[regionCode12 == "NEU" & univocalName == "Bus", value := value / 2]
  dt[regionCode21 == "MEA" & univocalName == "Bus", value := value / 2]

  #5: Adjust GER Truck size shares according to KBA data (calculated from stocks via AM and LF)
  dt[region == "DEU" & univocalName == "Truck (0-3_5t)", value := value * 2]
  dt[region == "DEU" & univocalName == "Truck (7_5t)", value := value * 0.25]
  dt[region == "DEU" & univocalName == "Truck (18t)", value := value * 0.65]
  dt[region == "DEU" & univocalName == "Truck (40t)", value := value * 1.4]

  #6: Total 2010 Freight demands, from ViZ 2010
  # (the shares are roughly OK)
  dt[region == "DEU" & univocalName %in% filter$trn_freight, value := value * 620 / 587]
  dt[, c("countryName", "regionCode21", "regionCode12", "check") := NULL]

  return(dt)
}
