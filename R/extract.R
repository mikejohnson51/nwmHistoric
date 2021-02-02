#' @title NWM Reanalysis Extraction
#' @description Download hourly flow values for an NHD COMID from the National 
#' Water Model version 1.2 or 2.0. Returned data is available between 
#' "1993-01-01 00" and "2017-12-31 23" but can be 
#' subset using a startDate and endDate.
#' @param comid a NHD common identifier
#' @param siteNumber a USGS NWIS site number (eight digits)
#' @param startDate a start date (YYYY-MM-DD) or (YYYY-MM-DD HH)
#' @param endDate an end date (YYYY-MM-DD) or (YYYY-MM-DD HH)
#' @param tz the desired timezone of the data. Can be found with `OlsonNames()`
#' @param version the NWM version to extract (current = 1.2 or 2.0 (default))
#' @return data.frame
#' @importFrom RNetCDF close.nc
#' @importFrom dplyr bind_rows
#' @importFrom fastmatch fmatch
#' @importFrom lubridate with_tz tz as_datetime
#' @export

readNWMdata = function(comid = NULL,
                       siteID = NULL,
                       startDate = "1993-01-01",
                       endDate   = "2018-12-31",
                       tz = "UTC",
                       version = 2) {
  
  startDate = tryCatch({
    lubridate::ymd_hms(paste0(startDate, ":00:00"), tz = tz)
  }, warning = function(w){
    lubridate::ymd_hms(paste(startDate, "00:00:00"), tz = tz)
  })
  
  dt_utc <- lubridate::ymd_hms("2010-08-03 00:50:50")
  
  endDate = tryCatch({
      lubridate::as_datetime(paste0(endDate, ":00:00"), tz = tz)
    }, warning = function(w){
      lubridate::as_datetime(paste(endDate, "23:00:00"), tz = tz)
    })

  base = error.checks(startDate, endDate, tz, version)

  if (!is.null(siteID)) {
    comid = dataRetrieval::findNLDI(nwis = siteID)$comid
  }
  
  rc  <- retro_call(comid, meta.obj = base)

  if (!is.null(rc)) {

    res <-  bind_rows(lapply(1:rc$rows,
                   FUN  = extract_thredds,  
                   urls = rc$call.meta,  
                   dap  = rc$open.dap.file))


    close.nc(rc$open.dap.file)
    
    res$dateTime = with_tz(res$dateTime, tz)
  
    if (!is.null(siteID)) { res$siteID = siteID }
    
    res
    
  } else {
    message("The requested feature ID is not in the NWM v", version, " archive.")
    NULL
  }
}

