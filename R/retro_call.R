retro_call = function(comid = NULL, startDate = NULL, endDate = NULL){

  fileID = sprintf("%03d", which(
    comid >= nwm_retro_index$minCOMID & 
    comid <= nwm_retro_index$maxCOMID
  ))
  
  if(is.null(startDate)){startDate = "1993-01-01"}
  if(is.null(endDate))  {endDate = "2017-12-31" }
  
  all = hour_seq('1993-01-01', '2017-12-31')

  s = which(as.POSIXct(paste(startDate, "0:00"), tz="UTC") == all)
  e = which(as.POSIXct(paste(endDate, "23:00"), tz="UTC")  == all)
  
  return(list(fileID     = fileID,
              startDate  = startDate,
              endDate    = endDate,
              startIndex = s - 1 , 
              endIndex   = e - 1)
         )
}

