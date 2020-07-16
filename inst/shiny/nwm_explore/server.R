library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)

pt = NULL
lat  =  29.93038
lon  = -95.37706

make_plotly = function(comid){
  nwm = readNWMdata(comid) %>% aggregate_ymd()
  plot_ly(x=nwm$ymd , y=nwm$flow, mode = "lines",
          line = list(color = 'rgb(205, 12, 24)', width = .5)) %>% 
    layout(title = "National Water Model Streamflow",
           xaxis = list(title = "Date"),
           yaxis = list (title = "Flow (cms)")) %>%
    as.tags() %>%
    {tags$div(style="width:300px; ", .)} %>%
    as.character()
}

server <- function(input, output,session) {
  
  output$map <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(preferCanvas = TRUE, updateWhenIdle = FALSE)) %>%
      setView(lon,lat,8) %>% 
      addTiles(group = "OSM") %>% 
      addProviderTiles("CartoDB.Positron", group = "Grayscale") %>%
      leafem::addMouseCoordinates()
  })
  
  #Show popup on click
  observeEvent(input$map_click, {
    
    click <-  data.frame(input$map_click) %>% 
      dplyr::select(lat,lng)
    
    pt = sf::st_as_sf(click, coords = c("lng", "lat"), 
                      crs = 4326)
    
    pt$comid = discover_nhd(pt)
    data     = find_nhd(pt$comid)
    
    ms <- data$flowpath %>% 
      sf::st_cast("POINT") %>% 
      group_by(comid) %>% 
      slice(n())
    
    outlet <- filter(ms, comid  == comid)
    
    bb <- sf::st_bbox(data$catch) %>% as.numeric()
    
    pt <<- pt 
    
    leafletProxy("map") %>% 
      clearMarkers() %>%
      clearShapes() %>% 
      addMarkers(data = pt, 
                 layerId = ~comid) %>% 
      fitBounds( bb[1] - .01, bb[2] - .01, bb[3] + .01, bb[4] + .01) %>% 
      addPolygons(data = data$catch,color = 'gray') %>% 
      addPolylines(data = data$flowpath, color = 'skyblue') %>%
      addCircleMarkers(data = outlet, 
                       radius = 1, color = 'red', opacity = 1,
                       popup = make_plotly(comid)
      ) %>%
      onRender(
        "function(el,x) { this.on('popupopen', function() {HTMLWidgets.staticRender();}}") %>%
      add_deps("plotly") %>%
      htmltools::attachDependencies(plotly:::plotlyMainBundle(), append = TRUE) %>%
      htmltools::attachDependencies(crosstalk::crosstalkLibs(), append = TRUE) %>%
      browsable() 
  })
  
  observeEvent(input$button_click, {
    
    history = nwmHistoric::readNWMdata(pt$comid)
    
    
    showModal(modalDialog(
      title = HTML(paste(pt$name),
                   paste("<br>COMID:", pt$comid)),
      renderPlotly({
        plot_ly(x=nwm$ymd , y=nwm$flow, mode = "lines",
                line = list(color = 'rgb(205, 12, 24)', width = .5)) %>% 
          add_lines() %>% 
          layout(title = "National Water Model Streamflow",
                 xaxis = list(title = "Date"),
                 yaxis = list (title = "Flow (cms)")) %>% 
          as.tags() %>% 
          {tags$div(style="width:600px;", .)} %>%
          as.character()
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


