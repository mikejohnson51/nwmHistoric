library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)
library(shinycustomloader)
devtools::load_all()

make_plots = function(x){
  
  history = nwmHistoric::extract_retro_url(comid = pt$comid)
  
  vals = history %>%
    group_by(year, month, day) %>%
    summarise(flow = max(flow)) %>%
    ungroup() %>%
    mutate(date = as.Date(paste(year, month, day, sep = "-")))
  
  ann_peak = history %>% 
    group_by(year) %>% 
    summarise(peak = max(flow))
  
  
  p = ggplot(data = vals, aes(x = date, y = flow)) +
    geom_line(size = .1) +
    labs(x = 'Date', y = 'Streamflow (cms)') +
    #geom_hline(yintercept = bf, col = 'darkred') +
    theme_classic()
  
  # p2 = ggplot(data = ann_exceed, 
  #             aes(x = as.Date(paste0(year, "-01-01")), 
  #                 y = value,
  #                 group = variable, col = variable)) +
  #   geom_col(color = "darkgray", fill = 'darkgray') + 
  #   geom_smooth(col = 'darkred') +
  #   ylim(c(0,max(ann_exceed$value))) +
  #   labs(x = "Year", y = "Number of bankfull exceedances") +
  #   
  #   scale_x_date(labels = scales::date_format("%Y")) +
  #   theme_classic()
  
  p3 = p #+ p2
  return(p3)
}
source('/Users/mikejohnson/Documents/retro_hydro/R/helpers.R')



X = seq(-97.36996, -93.64555, 9.259171e-05 )
Y = seq( 28.87556,  32.22229, 9.259171e-05)
pt = NULL
lat  =  29.93038
lon  = -95.37706

ui <- (
  fluidPage(
    titlePanel('NWM v2.0 Reanalysis'),
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
        baseGroups = c("Grayscale", "OSM", "Imagery"), #, "Rainfall", "Night/Day"),
        overlayGroups = c("Rainfall", "daylight", 'risk-point'),
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
    
    click <- input$map_click %>% data.frame() %>% dplyr::select(lat,lng)
  
    pt = sf::st_as_sf(click, coords = c("lng", "lat"), crs = '+proj=longlat +datum=WGS84')
    
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
      downloadButton('foo'),
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
  
}

shinyApp(ui, server)
