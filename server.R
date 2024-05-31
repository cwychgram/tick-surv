server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    pal <- colorFactor(palette = c("#B4D79E",
                                   "#5C8944",
                                   "#D79E9E",
                                   "#FFFFBE",
                                   "#C29ED7"),
                       domain = dnr$DESIG)
    
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addResetMapButton() %>%
      setView(-77.25, 38.85, zoom = 8.5) %>%
      addMapPane("drive", zIndex = 410) %>%
      addMapPane("counties", zIndex = 414) %>%
      addMapPane("parks", zIndex = 415) %>%
      addMapPane("points", zIndex = 416) %>%
      addLayersControl(overlayGroups = c("Counties", 
                                         "90-Minute Drive (JHSPH)",
                                         "MD DNR Land",
                                         "US NPS Land",
                                         "US FWS Land",
                                         "Local Parks",
                                         "Golf Courses"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addPolygons(data = counties,
                  label = ~NAMELSAD,
                  color = "#4E4E4E",
                  stroke = TRUE,
                  weight = 2.0,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.0,
                  fillColor = NA,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "Counties",
                  options = pathOptions(pane = "counties")) %>%
      addPolygons(data = drive, 
                  color = "#CCCCCC",
                  stroke = TRUE,
                  weight = 1.0,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.65,
                  fillColor = "#CCCCCC",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "90-Minute Drive (JHSPH)",
                  options = pathOptions(pane = "drive")) %>%
      addPolygons(data = dnr, 
                  label = ~DNRNAME,
                  color = "#4E4E4E",
                  stroke = TRUE,
                  weight = 0.75,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.85,
                  fillColor = ~pal(DESIG),
                  highlightOptions = highlightOptions(color = "#4E4E4E", 
                                                      weight = 3.0,
                                                      bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "MD DNR Land",
                  options = pathOptions(pane = "parks")) %>%
      addLegend(position = "topright",
                colors = c("#B4D79E",
                           "#5C8944",
                           "#D79E9E",
                           "#FFFFBE",
                           "#C29ED7"),
                labels = c("State Park",
                           "State Forest",
                           "Natural Resources Management Area",
                           "Wildlife Management Area",
                           "Natural Environment Area"),
                opacity = 1,
                group = "MD DNR Land",
                title = "MD DNR Land"
      ) %>%
      addPolygons(data = nps, 
                  label = ~UNIT_NAME,
                  color = "#4E4E4E",
                  stroke = TRUE,
                  weight = 0.75,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.85,
                  fillColor = "#9EBBD7",
                  highlightOptions = highlightOptions(color = "#4E4E4E", 
                                                      weight = 3.0,
                                                      bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "US NPS Land",
                  options = pathOptions(pane = "parks")) %>%
      addLegend(position = "topright",
                colors = c("#9EBBD7"),
                labels = c("US NPS Land"),
                opacity = 1,
                group = "US NPS Land"
      ) %>%
      addPolygons(data = fws, 
                  label = ~ORGNAME,
                  color = "#4E4E4E",
                  stroke = TRUE,
                  weight = 0.75,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.85,
                  fillColor = "#D7C29E",
                  highlightOptions = highlightOptions(color = "#4E4E4E", 
                                                      weight = 3.0,
                                                      bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "US FWS Land",
                  options = pathOptions(pane = "parks")) %>%
      addLegend(position = "topright",
                colors = c("#D7C29E"),
                labels = c("US FWS Land"),
                opacity = 1,
                group = "US FWS Land"
      ) %>%
      addCircleMarkers(data = golf,
                       label = ~Title,
                       radius = 3,
                       color = "#000000",
                       fillColor = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillOpacity = 1,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "14px",
                         direction = "auto"),
                       group = "Golf Courses",
                       options = pathOptions(pane = "points")) %>%
      addCircleMarkers(data = locparks,
                       label = ~PARK_NAME,
                       radius = 3,
                       color = "#000000",
                       fillColor = "#66ff00",
                       weight = 1,
                       opacity = 1,
                       fillOpacity = 1,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "14px",
                         direction = "auto"),
                       group = "Local Parks",
                       options = pathOptions(pane = "points")) %>%
      hideGroup(c("Golf Courses", "Local Parks"))
    
  })
  
}