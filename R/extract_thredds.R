#' @title NWM Reanalysis Extraction
#' @description Download hourly flow values for an NHD COMID from the National Water Model version 2.0. Returned data is avaliable between "1993-01-01" and "2017-12-31" but can be subset using a startDate and endDate.
#' @param comid a NHD common identifier
#' @param startDate a start date (YYYY-MM-DD)
#' @param endDate an end date (YYYY-MM-DD)
#' @return data.frame
#' @importFrom RNetCDF open.nc var.get.nc
#' @export

extract_retro_url = function(comid,
                       startDate = NULL,
                       endDate = NULL) {
  baseURL <-
    'https://cida-test.er.usgs.gov/thredds/dodsC/demo/temp/nwm/nwm_retro_full/nwm_retro_'
  
  ind <- retro_call(comid, startDate, endDate)
  
  url <- paste0(baseURL, ind$fileID, '.nc')
  
  cID <- open.nc(url) %>%
    var.get.nc("feature_id")
  
  id <- which(cID == comid)
  
  var <- paste0(url,
                "?streamflow[", id, ":1:", id, "]",
                "[", ind$startIndex , ":1:", ind$endIndex, "]") %>%
    open.nc() %>%
    var.get.nc("streamflow") * .1
  
  time <- hour_seq(ind$startDate, ind$endDate)
  
  df <- data.frame(
    model = "NWM20",
    comid = comid,
    time_utc  = time,
    flow  = var
  ) %>% mutate(
    year  = format(time, "%Y"),
    month = format(time, "%m"),
    day   = format(time, "%d"),
    hour  = format(time, "%H")
  )
  
  return(df)
}

