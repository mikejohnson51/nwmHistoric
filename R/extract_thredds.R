#' @title NWM Reanalysis Extraction
#' @description Download hourly flow values for an NHD COMID from the National Water Model version 2.0. Returned data is availiable between "1993-01-01" and "2017-12-31" but can be subset using a startDate and endDate.
#' @param comid a NHD common identifier
#' @siteID
#' @param startDate a start date (YYYY-MM-DD)
#' @param endDate an end date (YYYY-MM-DD)
#' @return data.frame
#' @importFrom RNetCDF open.nc var.get.nc close.nc
#' @importFrom dplyr mutate
#' @importFrom foreach %dopar% foreach
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @export

readNWMdata = function(comid = NULL, 
                       siteNumber = NULL,
                       startDate = NULL,
                       endDate = NULL) {
  
  if(!is.null(siteNumber)){comid = gage_lu$comid[match(siteNumber,gage_lu$gages)]}

  urls <- retro_call(comid, startDate, endDate) 
  
  if(!is.null(urls)){

  time <- hour_seq(urls$startDate[1], urls$endDate[1])
  
  `%dopar%` <- foreach::`%dopar%`
  no_cores  <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)
  
  res = foreach::foreach(i = 1:nrow(urls) , .combine = rbind) %dopar% {
        try({
          nc = open.nc(urls$url[i])
          var = nc %>% var.get.nc("streamflow") 
          close.nc(nc)
          
          data.frame(
            model = "NWM20",
            comid = urls$COMID[i],
            time_utc  = as.POSIXlt(time),
            flow  = var) 
        })
  }
  
  if(!is.null(siteNumber)){
    res$siteNumber = siteNumber
  }
  
  res %>% as_tibble()
  
  } else {
    NULL
  }
}

add_time = function(rawData) {
  rawData %>% mutate(
    year  = format(time_utc, "%Y"),
    month = format(time_utc, "%m"),
    day   = format(time_utc, "%d"),
    hour  = format(time_utc, "%H")
  )
}

