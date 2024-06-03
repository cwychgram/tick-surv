ui <- fluidPage(
  title = "Parks and Other Recreational Lands in Maryland",
  includeCSS("www/styles.css"),
  leafletOutput("map",
                width = "100%",
                height = "100vh")
)