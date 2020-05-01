#' @title Retro Call Meta Data
#' @description build out THREDDs call and meta information
#' @param comid an NHD COMID 
#' @param startDate a start date (YYYY-MM-DD)
#' @param endDate an end date (YYYY-MM-DD)
#' @return a data.frame
#' @keywords internal

retro_call = function(comid = NULL, startDate = "1993-01-01", endDate = "2018-12-31"){
  
  id = fastmatch::fmatch(comid, feature_id)
  
  if(is.null(endDate)){endDate = startDate}

  if(length(id) > 0){

    all = hour_seq('1993-01-01', '2018-12-31')
    s = which(as.POSIXct(paste(startDate, "0:00"), tz="UTC") == all)
    e = which(as.POSIXct(paste(endDate, "23:00"), tz="UTC")  == all)
  
  return(
    data.frame(
              COMID = comid,
              index = id,
              startDate  = startDate,
              endDate    = endDate,
              startIndex = s , 
              endIndex   = e,
              count = (e-s)  + 1
         ) 
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

#' @title Time Sequence Generator
#' @description Construct time series from start and end data
#' @param startDate 
#' @param endDate 
#' @return vector of dates
#' @keywords internal

hour_seq  = function(startDate, endDate){
  seq(
    from = as.POSIXlt(paste(startDate, "0:00"), tz="UTC"),
    to   = as.POSIXlt(paste(endDate, "23:00"), tz="UTC"),
    by   = "hour"
  ) %>% 
  as.POSIXlt()
}



