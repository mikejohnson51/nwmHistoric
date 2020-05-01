#' @title Discover NHD COMID 
#' @param pt a sf point object
#' @param lat a numeric latitude
#' @param lon a numeric longitude
#' @param sf return the COMID (FALSE) or spatial catchment object (TRUE)
#' @return a sf object or numeric COMID
#' @export

discover_comid = function(AOI,  sf = FALSE){
  
  
  if(sf::st_geometry_type(AOI) == "POINT"){
    coords <- sf::st_transform(AOI, 4269) %>% sf::st_coordinates()
    bbox = paste(coords[2], coords[1], coords[2] + 1e-05, coords[1] + 1e-05, "urn:ogc:def:crs:EPSG:4269", sep = ",")
  }  else {
    coords = sf::st_transform(AOI, 4269) %>% sf::st_bbox()
    bbox = paste(coords[2], coords[1], coords[4], coords[3], "urn:ogc:def:crs:EPSG:4269", sep = ",")
  }
  url_base <- paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows", 
                     "?service=WFS", "&version=1.0.0", "&request=GetFeature", 
                     "&typeName=nhdplus:catchmentsp", "&outputFormat=application%2Fjson", 
                     "&srsName=EPSG:4269")
  
  url <- paste0(url_base, "&bbox=", bbox)
  
  content <- httr::RETRY("GET", url, times = 3, pause_cap = 60)
  
  if (content$status_code == 200) {
    out = tryCatch(
      rawToChar(content$content) %>%
        sf::read_sf() ,
      error = function(e) {
        e
      },
      warning = function(w) {
        w
      }
    )
  } else {
    message(content$status_code)
    out = NULL
  }
  
  if(sf){
    out
  } else {
    out$featureid
  }
}


