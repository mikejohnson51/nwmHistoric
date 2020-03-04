retro_call = function(comid = NULL, startDate = NULL, endDate = NULL){

  fileID_find = function(x){ 
    
   id =  sprintf("%03d", which(
    x >= nwm_retro_index$minCOMID & 
    x <= nwm_retro_index$maxCOMID
  ))
   
   index = which(cID %in% x)
   
   data.frame(
     comid = x,
     fileID = ifelse(length(id) > 0, id, NA),
     index = ifelse(length(id) > 0, index, NA)
    )
  }

  fileIDs = lapply(comid, fileID_find) %>% 
    dplyr::bind_rows()
  
  bad.ids = fileIDs$comid[!complete.cases(fileIDs)]
  
  if(length(bad.ids) > 0){
    warning(paste(bad.ids, collapse = ", "), " is/are not modeled COMID(s). Removed from extract.")
  }
  
  fileIDs = fileIDs[complete.cases(fileIDs),]
  
  if(nrow(fileIDs) > 0){
  
  if(is.null(startDate)){startDate = "1993-01-01"}
  if(is.null(endDate))  {endDate = "2017-12-31" }
  
  all = hour_seq('1993-01-01', '2017-12-31')

  s = which(as.POSIXct(paste(startDate, "0:00"), tz="UTC") == all)
  e = which(as.POSIXct(paste(endDate, "23:00"), tz="UTC")  == all)
  
  return(data.frame(
              COMID = fileIDs$comid,
              index = fileIDs$index,
              fileID     = fileIDs$fileID,
              startDate  = startDate,
              endDate    = endDate,
              startIndex = s - 1 , 
              endIndex   = e - 1)
         ) %>% 
    mutate(url = paste0(baseURL, fileID, '.nc',
                        "?streamflow[", index, ":1:", index, "]",
                        "[", startIndex , ":1:", endIndex, "]") )
  } else {
    return(NULL)
  }
}
