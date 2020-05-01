#' @title Aggregate Hourly Data to Daily Values
#' @param rawData data extracted with \code{readNWMdata} 
#' @param FUN function used to aggregate values 
#' @return an aggregated data.frame
#' @export

aggregate_daily = function(rawData, FUN = "mean"){
  
  rawData$time_utc = as.POSIXlt(rawData$time_utc)
  
  data.frame(
    flow = rawData$flow,
    comid = rawData$comid,
    yr   = rawData$time_utc$year + 1900,
    mon  = rawData$time_utc$mon + 1,
    day  = rawData$time_utc$mday) %>% 
  group_by(comid, yr, mon, day) %>% 
    summarize_at(vars(flow), .funs = FUN)  %>% 
    mutate(time_utc = as.Date(paste(yr,sprintf('%02d',mon),sprintf('%02d',day), sep = "-"))) %>% 
    ungroup() %>% 
    select(-yr, -mon, -day)
  
}

#' @title Aggregate Hourly Data to Monthly Values
#' @param rawData data extracted with \code{readNWMdata} 
#' @param FUN function used to aggregate values 
#' @return an aggregated data.frame
#' @export

aggregate_monthly = function(rawData, FUN = "mean"){
  
  rawData$time_utc = as.POSIXlt(rawData$time_utc)
  
  data.frame(
    flow = rawData$flow,
    comid = rawData$comid,
    yr   = rawData$time_utc$year + 1900,
    mon  = rawData$time_utc$mon + 1) %>% 
    group_by(comid, yr, mon) %>% 
    summarize_at(vars(flow), .funs = FUN)  %>% 
    mutate(time_utc = as.Date(paste(yr,sprintf('%02d',mon), "01", sep = "-"), "%Y-%m-%d")) %>% 
    ungroup() %>% 
    select(-yr, -mon)
}

#' @title Aggregate Hourly Data to Seasonal Values
#' @param rawData data extracted with \code{readNWMdata} 
#' @param FUN function used to aggregate values 
#' @return an aggregated data.frame
#' @export
#' 
aggregate_season = function(rawData, FUN = "mean", na.rm = TRUE){
  
  rawData$time_utc = as.POSIXlt(rawData$time_utc)
  
  if(na.rm){
    rawData = rawData[!is.na(rawData$flow),]
  }
  
  data.frame(
    flow = rawData$flow,
    comid = rawData$comid,
    month   = rawData$time_utc$mon + 1)  %>% 
  mutate(season=recode(month, 
                    `1`="Winter",
                    `2`="Winter",
                    `3`="Spring",
                    `4`="Spring",
                    `5`="Spring",
                    `6`="Summer",
                    `7`="Summer",
                    `8`="Summer",
                    `9`="Fall",
                    `10`="Fall",
                    `11`="Fall",
                    `12`="Winter")) %>% 
    group_by(comid, season) %>% 
    summarize_at(vars(flow), FUN)  %>% 
    ungroup() 
  
}

#' @title Aggregate Hourly Data to Yearly Values
#' @param rawData data extracted with \code{readNWMdata} 
#' @param FUN function used to aggregate values 
#' @return an aggregated data.frame
#' @export
#' 
aggregate_yearly = function(rawData, FUN = "mean", na.rm = TRUE){
  
  rawData$time_utc = as.POSIXlt(rawData$time_utc)
  
  if(na.rm){
    rawData = rawData[!is.na(rawData$flow),]
  }
  
  data.frame(
    flow = rawData$flow,
    comid = rawData$comid,
    yr   = rawData$time_utc$year + 1900) %>% 
    group_by(comid, yr) %>% 
    summarize_at(vars(flow), FUN)  %>% 
    mutate(time_utc = as.Date(paste(yr,"01", "01", sep = "-"), "%Y-%m-%d")) %>% 
    ungroup() %>% 
    select(-yr)
  
}


# aggregate_waterYear = function(rawData){
#   
#   rawData$time_utc = as.POSIXlt(rawData$time_utc)
#   
#   if(na.rm){
#     rawData = rawData[!is.na(rawData$flow),]
#   }
#   
#   data.frame(
#     flow = rawData$flow,
#     comid = rawData$comid,
#     yr   = rawData$time_utc$year + 1900) %>% 
#     mutate(yr = add_wa)
#     group_by(comid, yr) %>% 
#     summarize_at(vars(flow), FUN)  %>% 
#     mutate(time_utc = as.Date(paste(yr,"01", "01", sep = "-"), "%Y-%m-%d")) %>% 
#     ungroup() %>% 
#     select(-yr)
#   
# }

# add_waterYear = function(rawData){
#   lapply(pt, class)
# }
# 
# calc_water = function(dateVec){
#   dateTimeVec <- as.POSIXlt(dateVec)
#   calYear <- dateTimeVec$year + 1900
#   calMon <- dateTimeVec$mon + 1
#   whichPastOct <- calMon >= 10
#   whichPastOct[is.na(whichPastOct)] <- FALSE
#   waterYear <- calYear
#   waterYear[whichPastOct] <- calYear[whichPastOct] + 1
# 
#   waterYear
# }
# 
# 
# pt$wy = calc_water(pt$time_utc)
# 
#   dateColWY <- allowedWYColNames[dateCol]
#   rawData[[dateColWY]] <- ?calcWaterYear(rawData[[dateCol]])
#   dateCol_i <- which(names(rawData) == dateCol)
#   dateColWY_i <- which(names(rawData) == dateColWY)
#   everything_else <- which(!(names(rawData) %in% c(dateCol, 
#                                                    dateColWY)))
#   everything_else <- everything_else[!everything_else %in% 
#                                        c(1:dateCol_i, dateColWY_i)]
#   rawData <- rawData[, c(1:dateCol_i, dateColWY_i, everything_else)]
# }
# return(rawData)
# }