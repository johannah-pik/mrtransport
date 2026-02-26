#' Read European Environment Agency transport data
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
#' @importFrom stringr str_extract

readEEA <- function(subtype = c("historicalPassEnergyServiceDemand", 
                                   "historicalFreightEnergyServiceDemand", 
                                   "futurePassEnergyServiceDemand",
                                   "futureFreightEnergyServiceDemand")) {

  switch(
    subtype,
    "historicalPassEnergyServiceDemand" = {
    allCountryData <- fread("eu-27_passenger_transport_activity_(gpkm)_for_different_transport_modes.csv",
                            skip = 10)
    # filter data
    colHeaders <- names(allCountryData)
    # different temporal resolutions are provided for data subsets
    # provided number of rows corresponds to different temporal resolutions
    yearCols <- colHeaders[grepl("^Years.*", colHeaders)]
    yearsData <- allCountryData[, ..yearCols]
    yearsData <- yearsData[!`Years - 1` %in% c("Years - 1", "")]
    yearsData[, `Years - 1` := as.integer(`Years - 1`)]
    yearsData[, `Years - 2` := as.integer(`Years - 2`)]
    yearsData[, `Years - 3` := as.integer(`Years - 3`)]
    yearsData[, `Years - 4` := as.integer(`Years - 4`)]
    temporalResolution <- c(nrow(yearsData[!is.na(`Years - 1`)]),
                            nrow(yearsData[!is.na(`Years - 2`)]),
                            nrow(yearsData[!is.na(`Years - 3`)]),
                            nrow(yearsData[!is.na(`Years - 4`)]))
    # collect country specific data
    colHeaders <- colHeaders[!colHeaders %in% yearCols]
    # Pattern matches both "EU-27 " and "XX "
    countryPattern <- "^(EU-27|[A-Z]{2})"
    countryCodes <- unique(str_extract(colHeaders, countryPattern))
    countrySpecificData <- lapply(countryCodes, function(reg){
      regCols <- colHeaders[grepl(paste0("^", reg, ".*"), colHeaders)]
      subData <- allCountryData[, ..regCols]
      allSubDataPerCountry <- list()
      # treat every column separately to match with provided years
      for (col in names(subData)) {
        browser()
        # extract col data
        colData <- subData[, ..col]
        colData <- colData[!get(col) %in% c(col, "")]
        # match with years column
        for (yearLength in temporalResolution) {
          if (nrow(colData) == yearLength) {
             colIndex <- which(temporalResolution == yearLength)
             yearCol <- yearsData[, .SD, .SDcols = colIndex]
             colName <- names(yearCol)
             yearCol <- yearCol[!is.na(get(colName))]
             setnames(yearCol, eval(colName), "period")
             fullColData <- cbind(colData, yearCol)
             break
          }
        }
        # add region col
        fullColData[, region := reg]
        # add variable and unit cols
        variableCol <- names(fullColData)[!names(fullColData) %in% c("period", "region")]
        unit <- str_remove_all(str_extract(variableCol, "\\(([^)]+)\\)"), "[()]")
        cleanName <- str_remove(str_remove(variableCol, "^[:alpha:]{2} "), "\\s*\\([^)]*\\)$")
        fullColData[, variable := cleanName][, unit := unit]
        setnames(fullColData, eval(variableCol), "value")
        # delete old variable col
        fullColData <- fullColData[, c("region", "variable", "period", "value", "unit")]
        allSubDataPerCountry[[length(allSubDataPerCountry) + 1]] <- fullColData
      }
      return(rbindlist(allSubDataPerCountry))
    })
    
    
    },
    "historicalFreightEnergyServiceDemand" = {
      
    },
    "futurePassEnergyServiceDemand" = {
      
    },
    "futureFreightEnergyServiceDemand" = {
      
    },
    
  )
}
