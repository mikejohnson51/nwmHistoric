#' @title Make Map from NHD COMID
#' @param comid an NHD COMID
#' @return
#' @export
#' @examples
#' @importFrom sf st_cast
#' @importFrom HydroData findNLDI
#' @importFrom dplyr group_by slice filter
#' @import leaflet

make_map  = function(comid){
  
  data <- HydroData::findNLDI(comid = comid, nav = c("UM", "DM"),
                              find = "basin", distance_km = .005)
  
  ms <- data$UM %>% 
    sf::st_cast("POINT") %>% 
    group_by(nhdplus_comid) %>% 
    slice(n())
  
  siteID <- ms %>% 
    filter(nhdplus_comid  == comid)
  
  ms <- ms %>%
    filter(nhdplus_comid  != comid)
  
  ds <- data$DM %>% 
    sf::st_cast("POINT") %>% 
    group_by(nhdplus_comid) %>% 
    slice(n()) %>% 
    filter(nhdplus_comid != comid)
  
  make_label = function(x){ paste0("COMID: ", x) }
  
  bb <- sf::st_bbox(data$site) %>% as.numeric()
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB, 
                     group = "Base") %>%
    addProviderTiles(providers$Esri.WorldImagery, 
                     group = "Imagery") %>%
    addProviderTiles(providers$Esri.WorldShadedRelief, 
                     group = "Hillshade") %>%
    fitBounds( bb[1] - .01, bb[2] - .01, bb[3] + .01, bb[4] + .01) %>% 
    addPolygons( data = data$basin, 
                 col = 'gray') %>% 
    addPolylines(data = data$UM,   
                 col = 'skyblue') %>%
    addPolylines(data = data$DM,   
                 col = 'skyblue') %>%
    addPolylines(data = data$site,
                 col = 'blue') %>% 
    addCircleMarkers(data = ms, 
                     radius = 1,
                     popup = make_label(ms$nhdplus_comid),
                     col = 'red',
                     opacity = 1) %>% 
    addCircleMarkers(data = ds, 
                     radius = .5,
                     popup = make_label(ds$nhdplus_comid),
                     col = 'orange',
                     opacity = 1) %>% 
    addCircleMarkers(data = siteID, 
                     radius = 4,
                     popup = make_label(siteID$nhdplus_comid),
                     col = 'green',
                     opacity = 1) %>% 
    addLayersControl(baseGroups = c("Base", "Imagery", "Hillshade"))
  
}