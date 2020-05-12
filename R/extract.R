#' @title NWM Reanalysis Extraction
#' @description Download hourly flow values for an NHD COMID from the National Water Model version 2.0. Returned data is availiable between "1993-01-01" and "2017-12-31" but can be subset using a startDate and endDate.
#' @param comid a NHD common identifier
#' @param siteNumber a USGS NWIS site number (eight digits)
#' @param startDate a start date (YYYY-MM-DD)
#' @param endDate an end date (YYYY-MM-DD)
#' @param tz the desired timezone of the data. Can be found with `OlsonNames()`
#' @param version the NWM version to extract (current = 1.2 or 2.0 (default))
#' @return data.frame
#' @importFrom RNetCDF close.nc
#' @importFrom dplyr bind_rows
#' @importFrom fastmatch fmatch
#' @importFrom lubridate with_tz
#' @export

readNWMdata = function(comid = NULL,
                       siteNumber = NULL,
                       startDate = "1993-01-01",
                       endDate   = "2018-12-31",
                       tz = "UTC",
                       version = 2.0) {
  
  base = error.checks(startDate, endDate, tz, version)

  if (!is.null(siteNumber)) {
    comid = nwmHistoric::gage_lu$comid[fmatch(siteNumber, nwmHistoric::gage_lu$nwis)]
  }
  
  rc  <- retro_call(comid, meta.obj = base)
  
  if (!is.null(rc)) {

    res <-  bind_rows(lapply(1:rc$rows,
                   FUN  = extract_thredds,  
                   urls = rc$call.meta,  
                   dap  = rc$open.dap.file))
    
    close.nc(rc$open.dap.file)
    
    res$time = with_tz(res$time, tz)
 
    if (!is.null(siteNumber)) { res$siteNumber = siteNumber }
    
    res
    
  } else {
    message("The requested feature ID is not in the NWM v", version, " archive.")
    NULL
  }
}
