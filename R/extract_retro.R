#' @title Extract Historic timeseries for COMID
#' @param comid an NHD COMID 
#' @param archive where the archive is located
#' @return data.frame
#' @importFrom dplyr mutate
#' @importFrom RNetCDF open.nc var.get.nc close.nc
#' @export

extract_retro = function(comid, archive = NULL){
  
  df2 = nwmHistoric::nwm_retro_index
  
  fileID = sprintf("%03d", which(
      comid >= df2$minCOMID & 
      comid <= df2$maxCOMID
  ))
  
  file = paste0(archive, "nwm_retro_", fileID,".nc") 
  nc = RNetCDF::open.nc(file)
  
  feat = RNetCDF::var.get.nc(nc, var = "feature_id")
  comidID = which(feat == comid)
  
  var = RNetCDF::var.get.nc(nc, 
                            var = "streamflow", 
                            start= c(1, comidID), 
                            count= c(NA,1)
  ) * 0.01
  
  RNetCDF::close.nc(nc)
  
  time = seq(
    from = as.POSIXct("1993-01-01 0:00", tz="UTC"),
    to   = as.POSIXct("2017-12-31 23:00", tz="UTC"),
    by   = "hour"
  ) 
  
  df = data.frame(
    model = "NWM20",
    comid = comid,
    time_utc  = time,
    flow  = var
  ) %>% mutate(
    year  = format(time, "%Y"),
    month  = format(time, "%m"),
    day   = format(time, "%d"),
    hour  = format(time, "%H")
  )
  
  return(df)
  
}

