#' Index Files
#' Dataset containing index information for NWM access
#' @docType data

'nwm_retro_index'

#' Index COMIDs
#' Dataset containing index information for NWM access
#' @docType data
#' 
#  baseURL <-
# 'https://cida-test.er.usgs.gov/thredds/dodsC/demo/temp/nwm/nwm_v2_retro_full/nwm_retro_v2_'
# 
# 
# ind <- retro_call(comid, startDate, endDate)
# 
# url <- paste0(baseURL, ind$fileID, '.nc')
# 
# cID <- open.nc(url) %>%
#   var.get.nc("feature_id")
# 
# save(cID, file = './data/ids.rda')

'cID'

baseURL <-
  'https://cida-test.er.usgs.gov/thredds/dodsC/demo/temp/nwm/nwm_v2_retro_full/nwm_retro_v2_'