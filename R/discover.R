#' @title Discover NHD Features
#' @param AOI a sf point or polyfon object
#' @param feature if not NULL. What realization of an NHD object should be returned. Options include: catchment, flowline, and outlet.
#' @importFrom sf st_geometry_type st_transform st_coordinates st_bbox read_sf st_zm
#' @importFrom httr RETRY timeout
#' @importFrom dplyr select
#' @return a sf object or numeric COMID
#' @export

discover_nhd = function(AOI, feature = NULL) {
  
  AOI.type = st_geometry_type(AOI)
  query    = "catchmentsp"
  
  if (is.null(feature)) {
    feature = 'zzz'
    get.spatial = FALSE
  } else {
    get.spatial = TRUE
    realization.types = c('catchment', 'flowline', 'outlet')
    if (!feature %in% realization.types) {
      stop(feature, ' not a valid feature type for NHDPlus. \nOptions include:\n', 
           paste(realization.types, collapse = ", "))
    }
  
  }

  if(AOI.type != "POINT" && feature != "catchment"){ 
    query = "nhdflowline_network" 
  }

  if (AOI.type == "POINT") {
    coords <- st_coordinates(st_transform(AOI, 4269))
    bbox = paste(
      coords[2],
      coords[1],
      coords[2] + 1e-05,
      coords[1] + 1e-05,
      "urn:ogc:def:crs:EPSG:4269",
      sep = ","
    )
  }  else {
    coords = st_bbox(st_transform(AOI, 4269))
    bbox = paste(
      coords[2],
      coords[1],
      coords[4],
      coords[3],
      "urn:ogc:def:crs:EPSG:4269",
      sep = ",")
  }
  
  url <- paste0(
      "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows",
      "?service=WFS",
      "&version=1.0.0",
      "&request=GetFeature",
      "&typeName=nhdplus:",
      query,
      "&outputFormat=application%2Fjson",
      "&srsName=EPSG:4269",
      "&bbox=", 
      bbox)
  
  content <- RETRY("GET", url, times = 3, timeout(5))
  
  if (content$status_code == 200) {
    out = tryCatch(
      # Character to sf, drop Z of XYZ
      st_zm(read_sf(rawToChar(content$content))) ,
      error   = function(e) { e },
      warning = function(w) { w }
    )
  } else {
    message(content$status_code)
    out = NULL
  }
  
  if ('featureid' %in% names(out)) {
    out = dplyr::select(out, comid = featureid)
  } else {
    out = dplyr::select(out, comid)
  }
  
  if (!get.spatial) { 
    return(out$comid) 
  } else {
    if (AOI.type == 'POINT' && feature != "catchment") {
      out = paste0('https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/', out$comid)
      out = read_sf(out) %>% dplyr::select(comid)
    }
    
    if (feature == 'outlet') { out = find_outlets(out) }
    
    return(out)
  }
}

#' @title Identify NHD outlets from flowlines
#' @description Convert flowline NHD realization to outlets.
#' @param flowlines a sf LINESTRING or MULTILINESTRING object
#' @importFrom sf st_geometry_type st_cast st_transform st_crs st_line_sample st_zm
#' @return the input flowlines object with geometry changed to POINT objects representing the oulets
#' @export

find_outlets = function(flowlines){
  AOI.type = st_geometry_type(flowlines) %>% unique()
  if(!AOI.type %in% c("MULTILINESTRING", "LINESTRING")){
    stop("outlets can only be extracted from flowlines", call. = FALSE)
  } else {
    flowlines$geometry = suppressWarnings(
      flowlines$geometry %>%
      st_cast("LINESTRING") %>%
      st_transform(5070) %>%
      st_line_sample(sample = 1) %>%
      st_cast("POINT") %>%
      st_transform(st_crs(flowlines)) %>%
      st_zm()
    )
    
    return(flowlines)
  }
}

#' @title Discover USGS NWIS Stream Gages
#' @description \code{discover_nwis} returns a POINT feature class of NWIS gages reporting parameter code '00060' (streamflow) for an Area of Interest that ALSO report NWM data. 
#' Data is accessed through the NWIS web portal.
#' @param AOI A spatial geometry
#' @return sf object
#' @importFrom xml2 xml_root xml_children xml_attr read_xml
#' @importFrom fastmatch fmatch
#' @importFrom sf st_as_sf st_bbox
#' @importFrom dplyr filter
#' @export

discover_nwis = function(AOI = NULL){

  bb = round(sf::st_bbox(AOI), 7)
  
  url = paste0("https://waterservices.usgs.gov/nwis/site/?format=mapper&bBox=",
               bb$xmin, ",", 
               bb$ymin, ",", 
               bb$xmax, ",", 
               bb$ymax,
               "&parameterCd=00060&siteStatus=active")
  
  y = tryCatch(
    {read_xml(url) },
    warning = function(w) { NULL },
    error = function(e)   { NULL })
  
  if(is.null(y)){ 
    stop("No gages found in this AOI.") 
  } else {
    doc        <- xml_root(y)
    sc         <- xml_children(doc)
    sites      <- xml_children(sc)
    site_no    <- xml_attr(sites, "sno")
    station_nm <- xml_attr(sites, "sna")
    site_type  <- xml_attr(sites, "cat")
    lat        <- as.numeric(xml_attr(sites, "lat"))
    lon        <- as.numeric(xml_attr(sites, "lng"))
    agency_cd  <- xml_attr(sites, "agc")
    
    sites_sf <- data.frame(agency_cd, site_no, station_nm, site_type,
                     lat, lon, stringsAsFactors=FALSE) %>% 
                st_as_sf(coords = c("lon", "lat"), crs = 4269)
    
    sites_sf$comid = nwmHistoric::gage_lu$comid[fmatch(sites_sf$site_no, 
                                                       nwmHistoric::gage_lu$nwis)]
    
    sites_sf %>% filter(!is.na(comid))
  }
}


