ui <- (
  fluidPage(
    titlePanel('NWM v2.0 Reanalysis: Explorer'),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leafletOutput('map')
  )
)