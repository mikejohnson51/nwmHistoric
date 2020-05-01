#' @title NWM Reanalysis Extraction
#' @description Download hourly flow values for an NHD COMID from the National Water Model version 2.0. Returned data is availiable between "1993-01-01" and "2017-12-31" but can be subset using a startDate and endDate.
#' @param comid a NHD common identifier
#' @param siteNumber a USGS NWIS site number (eight digits)
#' @param startDate a start date (YYYY-MM-DD)
#' @param endDate an end date (YYYY-MM-DD)
#' @return data.frame
#' @importFrom RNetCDF open.nc var.get.nc close.nc
#' @importFrom dplyr mutate bind_rows
#' @importFrom foreach %dopar% foreach
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @export

readNWMdata = function(comid = NULL, 
                       siteNumber = NULL,
                       startDate = "1993-01-01",
                       endDate = "2018-01-01",
                       version ="2") {
  
  hydroshare = 'http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/'
  
  baseURL = paste0(hydroshare, ifelse(version == "2", 'nwm_v2_retro_full.ncml', 'nwm_retro_full.ncml'))

  if(!is.null(siteNumber)){comid = gage_lu$comid[match(siteNumber,gage_lu$gages)]}

  urls <- retro_call(comid, startDate, endDate) 
  
  if(!is.null(urls)){

    time <- hour_seq(urls$startDate[1], urls$endDate[1])
    nc = open.nc(baseURL)  
    all = list()
  
    for(i in 1:nrow(urls)){
       var = var.get.nc(nc, "streamflow",
                               start = c(urls$startIndex[i], urls$index[i]),
                               count = c(urls$count[i],1),
                               unpack = TRUE)

              all[[i]] = data.frame(
                model = "NWM20",
                comid = urls$COMID[i],
                time_utc  = as.POSIXlt(time),
                flow  = var)
    
  }
  
  close.nc(nc)
  
  res = dplyr::bind_rows(all)
  
  if(!is.null(siteNumber)){ res$siteNumber = siteNumber}
  
  res
  
  } else {
    NULL
  }
}

