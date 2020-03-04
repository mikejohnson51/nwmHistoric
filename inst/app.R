library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(shinycustomloader)
devtools::load_all()

make_plots = function(x){
  
  history = nwmHistoric::extract_retro_url(x)
  
  vals = history %>%
    group_by(year, month, day) %>%
    summarise(flow = max(flow)) %>%
    ungroup() %>%
    mutate(date = as.Date(paste(year, month, day, sep = "-")))

  p = ggplot(data = vals, aes(x = date, y = flow)) +
    geom_line(size = .1) +
    labs(x = 'Date', y = 'Streamflow (cms)') +
    theme_classic()

  return(p)
}

pt = NULL
lat  =  29.93038
lon  = -95.37706

ui <- (
  fluidPage(
    titlePanel('NWM v2.0 Reanalysis: Tester'),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leafletOutput('map') %>% withLoader(loader="pacman")
  )
)

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
    pt$comid = nhdplusTools::discover_nhdplus_id(pt)
    
    pt <<- pt 
    
    leafletProxy("map") %>% 
      clearMarkers() %>%
      addMarkers(data = pt, 
                 layerId = ~comid,
                 popup = paste(leafpop::popupTable(st_drop_geometry(pt), 
                                                   row.numbers = FALSE, 
                                                   feature.id = FALSE), "</b></br>", actionButton("selectlocation", "Plot this Reach", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())') ))
  })
  
  observeEvent(input$button_click, {
    showModal(modalDialog(
      title = HTML(paste(pt$name),
                   paste("<br>COMID:", pt$comid)),
      renderPlot({
        make_plots(pt$comid)
      }),
      downloadButton('foo', label = "Download PNG"),
      downloadButton('foo2', label = "Download CSV"),
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
    }
  )
  
}

shinyApp(ui, server)
