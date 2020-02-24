#' re-export magrittr pipe operator
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export

NULL

hour_seq  = function(startDate, endDate){
  seq(
    from = as.POSIXct(paste(startDate, "0:00"), tz="UTC"),
    to   = as.POSIXct(paste(endDate, "23:00"), tz="UTC"),
    by   = "hour"
  )
}