baseURL <- 'https://cida-test.er.usgs.gov/thredds/dodsC/demo/temp/nwm/nwm_v2_retro_full/nwm_retro_v2_'

#' @title Retro Call Meta Data
#' @description build out THREDDs call and meta information
#' @param comid an NHD COMID 
#' @param startDate a start date (YYYY-MM-DD)
#' @param endDate an end date (YYYY-MM-DD)
#' @return a data.frame
#' @keywords internal

retro_call = function(comid = NULL, startDate = NULL, endDate = NULL){

  
  fileIDs = lapply(comid, fileID_find) %>% 
    dplyr::bind_rows() %>% 
    add_comid_index()
  
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
              index = fileIDs$id,
              fileID     = fileIDs$fileID,
              startDate  = startDate,
              endDate    = endDate,
              startIndex = s - 1 , 
              endIndex   = e - 1
         ) %>% 
    mutate(url = paste0(baseURL, fileID, '.nc',
                        "?streamflow[", index, ":1:", index, "]",
                        "[", startIndex , ":1:", endIndex, "]") )
  )
  } else {
    return(NULL)
  }
}

#' re-export magrittr pipe operator
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export

NULL

#' @title Build COMID metadata
#' @description  Find the THREDDS file id and COMID index 
#' @param x an NHD COMID
#' @return data.frame
#' @keywords internal

fileID_find = function(x){ 
  
  id =  sprintf("%03d", which(
      x >= nwm_retro_index$minCOMID & 
      x <= nwm_retro_index$maxCOMID
  ))
  
  data.frame(
    comid = x,
    fileID = ifelse(length(id) > 0, id, NA),
    stringsAsFactors = FALSE
  )
  
}

#' @title Time Sequence Generator
#' @description Construct time series from start and end data
#' @param startDate 
#' @param endDate 
#' @return vector of dates
#' @keywords internal

hour_seq  = function(startDate, endDate){
  seq(
    from = as.POSIXct(paste(startDate, "0:00"), tz="UTC"),
    to   = as.POSIXct(paste(endDate, "23:00"), tz="UTC"),
    by   = "hour"
  )
}


#' @title Add COMID index based on THREDDs file
#' @param fileIDs a output from \code{fileID_find}
#' @return data.frame
#' @keywords internal
#' @export

add_comid_index = function(fileIDs) {
  u = unique(fileIDs$fileID)
  
  `%dopar%` <- foreach::`%dopar%`
  no_cores  <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)
  
  res = foreach::foreach(i = 1:length(u), .combine = rbind) %dopar% {
    tmp = fileIDs[fileIDs$fileID == u[i], ]
    
    url  = paste0(baseURL, u[i], '.nc?feature_id')
    id =  RNetCDF::open.nc(url) %>%
      RNetCDF::var.get.nc('feature_id')
    
    tmp$id = match(tmp$comid, id)
    
    tmp
  }
  
  res
}
