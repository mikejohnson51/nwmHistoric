library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(shinycustomloader)

make_plots = function(x){
  
  vals = aggregate_ymd(x)

  p = ggplot(data = vals, aes(x = ymd, y = flow)) +
    geom_line(size = .1) +
    labs(x = 'Date', y = 'Streamflow (cms)') +
    theme_classic()

  return(p)
}

pt = NULL
lat  =  29.93038
lon  = -95.37706



server <- function(input, output,session) {
  
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(preferCanvas = TRUE, 
                                     updateWhenIdle = FALSE)) %>%
      setView(lon,lat,8) %>% 
      addTiles(group = "OSM") %>% 
      addProviderTiles(
        "CartoDB.Positron",    group = "Grayscale") %>%
      addProviderTiles(
        "Esri.WorldImagery",   group = "Imagery") %>%
      addWMSTiles(
        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        layers = "nexrad-n0r-900913",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = .15), 
        group = "Rainfall"
      ) %>% 
      addTerminator(group = "daylight") %>% 
      addLayersControl(
        baseGroups = c("Grayscale", "OSM", "Imagery"), 
        overlayGroups = c("Rainfall", "daylight"),
        options = layersControlOptions(collapsed = TRUE),
        position = 'topleft') %>% 
      addScaleBar("bottomleft") %>%
      addMiniMap( toggleDisplay = TRUE, minimized = TRUE) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "feet",
        primaryAreaUnit = "sqmiles",
        activeColor = "red",
        completedColor = "green" ) %>% 
      addEasyButtonBar(
        easyButton(
          icon='fa-crosshairs', title='Zoom to Risk Points',
          onClick=JS(paste0("function(btn, map){ map.setView(new L.LatLng(",lat,", ", lon,"), 8);}")))
      ) %>%
      hideGroup(c("daylight", "Rainfall")) %>% 
      leafem::addMouseCoordinates()
  })
  
  #Show popup on click
  observeEvent(input$map_click, {
    
    click <- input$map_click %>% 
      data.frame() %>% 
      dplyr::select(lat,lng)
  
    pt = sf::st_as_sf(click, 
                      coords = c("lng", "lat"), 
                      crs = '+proj=longlat +datum=WGS84')
    
    rev = AOI::geocode_rev(c(click$lat, click$lng))
    
    pt$osm_id = rev$osm_id
    pt$osm_type = rev$osm_type
    pt$name = rev$display_name
    pt$comid = discover_nhd(pt)

    data = find_nhd(pt$comid)
    
    ms <- data$flowpath %>% 
      sf::st_cast("POINT") %>% 
      group_by(comid) %>% 
      slice(n())
    
    siteID <- ms %>%
      filter(comid  == comid)
    
    make_label = function(x){ paste0("COMID: ", x) }
    
    bb <- sf::st_bbox(data$catch) %>% as.numeric()
    
    pt <<- pt 
    
    leafletProxy("map") %>% 
      clearMarkers() %>%
      clearShapes() %>% 
      addMarkers(data = pt, 
                 layerId = ~comid,
                 popup = paste(leafpop::popupTable(st_drop_geometry(pt), 
                                                   row.numbers = FALSE, 
                                                   feature.id = FALSE), "</b></br>", actionButton("selectlocation", "Plot this Reach", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())') )) %>% 
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
                       opacity = 1) 
  })
  
  observeEvent(input$button_click, {
    
    history = nwmHistoric::readNWMdata(pt$comid)
    
    
    showModal(modalDialog(
      title = HTML(paste(pt$name),
                   paste("<br>COMID:", pt$comid)),
      renderPlot({
        make_plots(history)
      }),
      downloadButton('foo', label = "Download PNG"),
      downloadButton('foo2', label = "Download CSV"),
      downloadButton('foo3', label = "Download RDS"),
      size = "l", easyClose = TRUE
    ))
  })
  
  output$foo = downloadHandler(
    filename = paste0('historic_nwm_comid_', pt$comid ,'.png'),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 12, height = 6,
                       res = 300, units = "in")
      }
      ggsave(file, plot = last_plot(), device = device)
    })
  
  output$foo2 = downloadHandler(
    filename = paste0('historic_nwm_comid_', pt$comid,'.csv'),
    content = function(file) {
      write.csv(history, file)
    })
    
    output$foo3 = downloadHandler(
      filename = paste0('historic_nwm_comid_', pt$comid, '.rds'),
      content = function(file) {
        saveRDS(history, file)
      })
}


