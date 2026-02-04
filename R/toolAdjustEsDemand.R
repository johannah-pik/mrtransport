#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param mapIso2region map iso countries to regions
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' @param data source data containing the annual mileage and load factor
#' univocalNames
#' @return a quitte object

toolAdjustEsDemand <- function(dt, mapIso2region, completeData, filter, data) {
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

  ######## new China stuff from Robert


  # ## For debugging: first create safe-copy to later check if only the wanted rows are changed. Outcomment for debugging.
  # dt_safecopy <- copy(dt)

  # extract existing ES totals for regions and trucks of interest
  histESdemandCHA <- dt[region %in% c("CHN") & univocalName %like% "Truck"]

  histESdemandCHA_old <- histESdemandCHA[    # drop variable and unit
    , .(value = sum(value)),
    by = .(region, univocalName, technology, period )
  ]

  histESdemandCHA_old_size <- histESdemandCHA_old[
    , .(old_ES = sum(value)),
    by = .(region, univocalName, period)
  ]

  histESdemandCHA_old_total <- histESdemandCHA_old[
    , .(total_ES = sum(value)),
    by = .(region, period)
  ]

  # calculate new ES splits

  ## define target vehicle shares by size:
  ## this data is based on downscaled values from CEIC data, and further split to EDGE-T splits : 40t	5%, 26t	9%, 18t	11%, 7.5t	23%, 0-3.5 t 52%
  ## The data can be found in the transport folder in the owncloud: "Data/RegionalData/compiling_CHA_data_heavy_duty_vehicles.xlsx", cells V17:V21

  VehSharesTargetSize <- data.table(univocalName = c("Truck (0-3_5t)","Truck (18t)","Truck (26t)","Truck (40t)","Truck (7_5t)"),
                                    region = "CHN",
                                    variable = "Share_in_Vehicles",
                                    unit = "Percent",
                                    value = c(52,11,9,5,23))

  ## Then calculate total tkm per vehicle from annual mileage and load factors.
  ## Take the values for "Liquids", as that is the most common technology in 2010

  annualTkmPerVehicleCol <- data$annualMileage[region %in% c("CHN") & univocalName %like% "Truck" & period == 2010 & technology == "Liquids",
                                          .(annualMileage = value), by = .(region, univocalName)]

  annualTkmPerVehicleCol <- annualTkmPerVehicleCol[data$loadFactor[region %in% c("CHN") & univocalName %like% "Truck" & period == 2010 & technology == "Liquids",
                                                              .(loadFactor = loadFactor), by = .(region, univocalName)],
                                                   on = .( univocalName, region)][
                                                     , load_x_mileage := annualMileage * loadFactor
                                                   ]


  ## calculate resulting ES values by size
  ESSharesTargetSize <- VehSharesTargetSize[
    annualTkmPerVehicleCol,
    on = .(univocalName, region)
  ][
    , ESsharesUnnormalized := value / 100 * load_x_mileage
  ]

  ESSharesTargetSize[ , ESsharesNormalized := ESsharesUnnormalized / sum(ESsharesUnnormalized),
                      by = region
  ]

  ESSharesTargetSizeBack <- ESSharesTargetSize[ , .(univocalName = univocalName ,
                                                    region = region,
                                                    variable = "TargetShareES",
                                                    unit = "Percent",
                                                    value = ESsharesNormalized * 100)
  ]


  # calculate target ES per vehicle size with old ES totals
  ## First create new DT with period x univocalname size, as SizeSharesTarget has no period, and histESdemandCHA_old_total no size

  histESdemandCHA_target_size <- CJ(
    period = histESdemandCHA_old_total$period,
    univocalName = ESSharesTargetSizeBack$univocalName,
    unique = TRUE
  )[
    ESSharesTargetSizeBack[, .(univocalName, ES_share = value / 100)],
    on = "univocalName"
  ][
    histESdemandCHA_old_total,
    on = "period"
  ][
    , target_ES := ES_share * total_ES
  ]

  ## calculate scaling factors for each size
  ES_scaling <- histESdemandCHA_old_size[
    histESdemandCHA_target_size,
    on = .(period, univocalName, region)
  ][
    , scaling := target_ES / old_ES
  ]

  ## rescale using old ES totals:
  histESdemandCHA_newES <- histESdemandCHA_old[
    ES_scaling,
    on = .(period, univocalName, region)
  ][
    , value := value * scaling
  ]

  ## drop unused columns, add variable and unit
  histESdemandCHA_newES[, c("old_ES", "ES_share", "total_ES", "target_ES", "scaling") := NULL][, ':='(variable = "ES", unit = "billion tkm/yr")]

  ## check that the total new ES and the total old ES are unchanged:
  stopifnot(
    all.equal(
      histESdemandCHA_newES[, sum(value), by = period],
      histESdemandCHA[, sum(value), by = period]
    )
  )

  ## overwrite the data in dt:

  join_cols <- c("region", "univocalName", "technology", "variable","unit", "period")

  dt[
    histESdemandCHA_newES,
    on = join_cols,
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

  ## is it necessary / advised to clean up, or is this anyway all deleted because the function only returns dt ??
  histESdemandCHA        <- NULL
  histESdemandCHA_newES  <- NULL
  histESdemandCHA_target_size <- NULL
  ESSharesTargetSize    <- NULL

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
