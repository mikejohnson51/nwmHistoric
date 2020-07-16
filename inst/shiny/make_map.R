#' @title Discover NHD Features
#' @param comid a sf point or polyfon object
#' @importFrom sf st_geometry_type st_transform st_coordinates st_bbox read_sf st_zm
#' @importFrom httr RETRY timeout
#' @importFrom dplyr select
#' @return a sf object for the COMID flow path and catchment divide
#' @export

find_nhd = function(comid) {
  
  call  = data.frame(
    feature = c("flowpath", "catch"),
    query   = c("nhdflowline_network", "catchmentsp"),
    id      = c("comid", "featureid")
  )
  
  urls <- paste0(
    "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows",
    "?service=WFS",
    "&version=1.0.0",
    "&request=GetFeature",
    "&typeName=nhdplus:", call$query,
    "&cql_filter=",call$id,"=",comid,"&outputformat=json",
    "&srsName=EPSG:4269")
  
  
  out_list = list()
  
  for(i in seq_along(urls)){
    content <- httr::RETRY("GET", urls[i], times = 3, timeout(5))
    
    if (content$status_code == 200) {
      out_list[[call$feature[i]]] = tryCatch(
        # Character to sf, drop Z of XYZ
        st_zm(read_sf(rawToChar(content$content))) ,
        error   = function(e) { e },
        warning = function(w) { w }
      )
    } else {
      message(content$status_code)
      out_list[[i]] = NULL
    }
  }
  
  out_list
  
}



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
  
  # data <- HydroData::findNLDI(comid = comid, nav = c("UM", "DM"),
  #                             find = "basin", distance_km = .005)
  data = find_nhd(comid)
  
  ms <- data$flowpath %>% 
    sf::st_cast("POINT") %>% 
    group_by(comid) %>% 
    slice(n())
  
  siteID <- ms %>%
    filter(comid  == comid)

  # ms <- ms %>%
  #   filter(comid  != comid)
  
  # ds <- data$DM %>% 
  #   sf::st_cast("POINT") %>% 
  #   group_by(nhdplus_comid) %>% 
  #   slice(n()) %>% 
  #   filter(nhdplus_comid != comid)
  
  make_label = function(x){ paste0("COMID: ", x) }
  
  bb <- sf::st_bbox(data$catch) %>% as.numeric()
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB, 
                     group = "Base") %>%
    addProviderTiles(providers$Esri.WorldImagery, 
                     group = "Imagery") %>%
    addProviderTiles(providers$Esri.WorldShadedRelief, 
                     group = "Hillshade") %>%
    fitBounds( bb[1] - .01, bb[2] - .01, bb[3] + .01, bb[4] + .01) %>% 
    addPolygons( data = data$catch, 
                 col = 'gray') %>% 
    addPolylines(data = data$flowpath,   
                 col = 'skyblue') %>%
    # addPolylines(data = data$DM,   
    #              col = 'skyblue') %>%
    # addPolylines(data = siteID,
    #              col = 'blue') %>% 
    addCircleMarkers(data = siteID, 
                     radius = 1,
                     popup = make_label(comid),
                     col = 'red',
                     opacity = 1) %>% 
    # addCircleMarkers(data = ds, 
    #                  radius = .5,
    #                  popup = make_label(ds$comid),
    #                  col = 'orange',
    #                  opacity = 1) %>% 
    # addCircleMarkers(data = siteID, 
    #                  radius = 4,
    #                  popup = make_label(siteID$nhdplus_comid),
    #                  col = 'green',
    #                  opacity = 1) %>% 
    addLayersControl(baseGroups = c("Base", "Imagery", "Hillshade"))
  
}

make_map(comid = 101)
