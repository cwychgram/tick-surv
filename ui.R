ui <- fluidPage(
  
  includeCSS("www/styles.css"),
  leafletOutput("map",
                width = "100%",
                height = "100vh")
  
)