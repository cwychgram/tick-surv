server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    lyme_popup <- paste(
      counties$NAMELSAD,
      "<br>",
      "Incidence per 100,000: ", counties$INCIDENCE, 
      "<br>",
      "Cases: ", counties$CASES,
      sep = "") %>%
      lapply(htmltools::HTML) 
    
    # lyme_bins <- dput(getJenksBreaks(counties$INCIDENCE, 6, subset = NULL))
    lyme_pal <-  colorQuantile(palette = "YlOrBr",
                          domain = counties$INCIDENCE,
                          n = 5)
    
    shapes <- c("circle", 
                "circle", 
                "rect",
                "rect",
                "rect",
                "rect",
                "rect",
                "rect",
                "rect")
    
    symbols <- setNames(Map(f = makeSymbol,
                            shape = shapes,
                            fillColor = c("#66ff00",
                                          "#000000",
                                          "#B4D79E",
                                          "#5C8944",
                                          "#D79E9E",
                                          "#FFFFBE",
                                          "#C29ED7",
                                          "#9EBBD7",
                                          "#D7C29E"),
                            color = c("#000000", 
                                      "#000000", 
                                      "#4E4E4E",
                                      "#4E4E4E",
                                      "#4E4E4E",
                                      "#4E4E4E",
                                      "#4E4E4E",
                                      "#4E4E4E",
                                      "#4E4E4E"),
                            opacity = 1.0,
                            fillOpacity = 1.0,
                            height = 24,
                            width = 24,
                            "stroke-width" = 1.0), 
                        shapes)
    
    pal <- colorFactor(palette = c("#B4D79E",
                                   "#5C8944",
                                   "#D79E9E",
                                   "#FFFFBE",
                                   "#C29ED7"),
                       domain = dnr$DESIG)
    
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25)) %>%
      addProviderTiles("CartoDB.Positron", group = "Neutral Basemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Color Basemap") %>%
      addResetMapButton() %>%
      addSearchOSM(options = searchOptions(collapsed = TRUE,
                                           position = "topleft")) %>%
      setView(-77.25, 38.85, zoom = 8.5) %>%
      addMapPane("drive", zIndex = 410) %>%
      addMapPane("counties", zIndex = 414) %>%
      addMapPane("lyme", zIndex = 415) %>%
      addMapPane("parks", zIndex = 416) %>%
      addMapPane("points", zIndex = 417) %>%
      addLayersControl(baseGroups = c("Neutral Basemap", 
                                      "Color Basemap"),
                       overlayGroups = c("Counties", 
                                         "90-Minute Drive from JHSPH",
                                         "State Department of Natural Resources Land",
                                         "US National Park Service Land",
                                         "US Fish and Wildlife Service Land",
                                         "Golf Courses",
                                         "Local Parks",
                                         "Lyme Disease Incidence (2022)",
                                         "Distance to Forest Edge (~10 second wait)",
                                         "Distance to Water (~10 second wait)",
                                         "Distance to Road (~10 second wait)"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-base').prepend('<label style=\"font-weight: bold\">Layer Control</label>');
        }
    ") %>%
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
                  group = "90-Minute Drive from JHSPH",
                  options = pathOptions(pane = "drive")) %>%
      addPolygons(data = dnr, 
                  label = ~DNRNAME,
                  color = "#4E4E4E",
                  stroke = TRUE,
                  weight = 1.00,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.90,
                  fillColor = ~pal(DESIG),
                  highlightOptions = highlightOptions(color = "#4E4E4E", 
                                                      weight = 3.0,
                                                      bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "State Department of Natural Resources Land",
                  options = pathOptions(pane = "parks")) %>%
      addPolygons(data = nps, 
                  label = ~UNIT_NAME,
                  color = "#4E4E4E",
                  stroke = TRUE,
                  weight = 1.0,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.90,
                  fillColor = "#9EBBD7",
                  highlightOptions = highlightOptions(color = "#4E4E4E", 
                                                      weight = 3.0,
                                                      bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "US National Park Service Land",
                  options = pathOptions(pane = "parks")) %>%
      addPolygons(data = fws, 
                  label = ~ORGNAME,
                  color = "#4E4E4E",
                  stroke = TRUE,
                  weight = 1.0,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.90,
                  fillColor = "#D7C29E",
                  highlightOptions = highlightOptions(color = "#4E4E4E", 
                                                      weight = 3.0,
                                                      bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "US Fish and Wildlife Service Land",
                  options = pathOptions(pane = "parks")) %>%
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
                       weight = 1.5,
                       opacity = 1,
                       fillOpacity = 1,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "14px",
                         direction = "auto"),
                       group = "Local Parks",
                       options = pathOptions(pane = "points")) %>%
      hideGroup(c("Local Parks", 
                  "Distance to Forest Edge (~10 second wait)",
                  "Distance to Water (~10 second wait)",
                  "Distance to Road (~10 second wait)",
                  "Lyme Disease Incidence (2022)")) %>%
      addLegendImage(
        images = symbols,
        labels = c("Local Park",
                   "Golf Course",
                   "State Park",
                   "State Forest",
                   "State Natural Resources Management Area",
                   "State Wildlife Management Area",
                   "State Natural Environment Area",
                   "US National Park Service Land",
                   "US Fish and Wildlife Service Land"),
        width = 10,
        height = 10,
        orientation = "vertical",
        title = htmltools::tags$div("Legend",
                                    style = 'font-weight: bold; text-align: left;'),
        position = "topright"
      ) %>%
      addPolygons(data = counties,
                  label = ~lyme_popup,
                  color = "#4E4E4E",
                  stroke = TRUE,
                  weight = 2.0,
                  smoothFactor = 1,
                  opacity = 1.0,
                  fillOpacity = 0.75,
                  fillColor = ~lyme_pal(INCIDENCE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"),
                  group = "Lyme Disease Incidence (2022)",
                  options = pathOptions(pane = "lyme"))
    
  })
  
  observe({
    selected_groups <- req(input$map_groups)
    if ("Lyme Disease Incidence (2022)" %in% selected_groups) {
      lyme_pal <-  colorQuantile(palette = "YlOrBr",
                                 domain = counties$INCIDENCE,
                                 n = 5)
      leafletProxy("map") %>%
        removeControl(layerId = "lyme_legend") %>%
        addLegend(position = "topleft",
                  pal = lyme_pal,
                  values = counties$INCIDENCE,
                  title = "Lyme Disease Incidence (2022)",
                  opacity = 0.75,
                  group = "Lyme Disease Incidence (2022)",
                  layerId = "lyme_legend"
        ) 
    }
    if ("Distance to Forest Edge (~10 second wait)" %in% selected_groups) {
      if (!exists("d2fe")) {
        d2fe <- read_stars("data/distance_to_forest_edge_3857.tif")
        assign("d2fe", d2fe, envir = .GlobalEnv)
      } 
      pal_d2fe <- colorNumeric(palette = "viridis",
                               na.color = "transparent",
                               domain = d2fe$distance_to_forest_edge_3857.tif,
                               reverse = TRUE)
      leafletProxy("map") %>%
        removeControl(layerId = "d2fe_legend") %>%
        addStarsImage(d2fe,
                      # project = FALSE,
                      colors = pal_d2fe,
                      group = "Distance to Forest Edge (~10 second wait)") %>%
        addLegend(position = "topleft",
                  pal = pal_d2fe,
                  values = d2fe$distance_to_forest_edge_3857.tif,
                  opacity = 1,
                  title = "Distance to Forest Edge (m)",
                  group = "Distance to Forest Edge (~10 second wait)",
                  layerId = "d2fe_legend")
    } 
    if ("Distance to Water (~10 second wait)" %in% selected_groups) {
      if (!exists("d2w")) {
        d2w <- read_stars("data/distance_to_water_3857.tif")
        assign("d2w", d2w, envir = .GlobalEnv)
      } 
      pal_d2w <- colorNumeric(palette = "viridis",
                               na.color = "transparent",
                               domain = d2w$distance_to_water_3857.tif,
                               reverse = TRUE)
      leafletProxy("map") %>%
        removeControl(layerId = "d2w_legend") %>%
        addStarsImage(d2w,
                      # project = FALSE,
                      colors = pal_d2w,
                      group = "Distance to Water (~10 second wait)") %>%
        addLegend(position = "topleft",
                  pal = pal_d2w,
                  values = d2w$distance_to_water_3857.tif,
                  opacity = 1,
                  title = "Distance to Water (m)",
                  group = "Distance to Water (~10 second wait)",
                  layerId = "d2w_legend")
    }
    if ("Distance to Road (~10 second wait)" %in% selected_groups) {
      if (!exists("d2r")) {
        d2r <- read_stars("data/distance_to_roads_3857.tif")
        assign("d2r", d2r, envir = .GlobalEnv)
      } 
      pal_d2r <- colorNumeric(palette = "viridis",
                              na.color = "transparent",
                              domain = d2r$distance_to_roads_3857.tif,
                              reverse = TRUE)
      leafletProxy("map") %>%
        removeControl(layerId = "d2r_legend") %>%
        addStarsImage(d2r,
                      # project = FALSE,
                      colors = pal_d2r,
                      group = "Distance to Road (~10 second wait)") %>%
        addLegend(position = "topleft",
                  pal = pal_d2r,
                  values = d2r$distance_to_roads_3857.tif,
                  opacity = 1,
                  title = "Distance to Road (m)",
                  group = "Distance to Road (~10 second wait)",
                  layerId = "d2r_legend")
    }
  })
  
}