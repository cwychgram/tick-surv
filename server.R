server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
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
      addResetMapButton() %>%
      addSearchOSM(options = searchOptions(collapsed = TRUE,
                                           position = "topleft")) %>%
      setView(-77.25, 38.85, zoom = 8.5) %>%
      addMapPane("basemap", zIndex = 0) %>%
      addMapPane("drive", zIndex = 410) %>%
      addMapPane("counties", zIndex = 414) %>%
      addMapPane("lyme", zIndex = 415) %>%
      addMapPane("parks", zIndex = 416) %>%
      addMapPane("points", zIndex = 417) %>%
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
        position = "bottomleft"
      )
  })
  
  observe({
    
    if (input$select_basemap == "Neutral") {
      leafletProxy("map", session) %>%
        addProviderTiles("CartoDB.Positron", 
                         group = "Neutral Basemap",
                         options = pathOptions(pane = "basemap"))
    } else {
      leafletProxy("map", session) %>%
        addProviderTiles("OpenStreetMap.Mapnik", 
                         group = "Color Basemap",
                         options = pathOptions(pane = "basemap"))
    }
    
  })
  
  observe({
    
    if (input$counties == TRUE) {
      leafletProxy("map", session) %>%
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
                    options = pathOptions(pane = "counties")) 
    } else {
      leafletProxy("map", session) %>%
        clearGroup("Counties")  
    }
    
  })
  
  observe({
    
    if (input$drive == TRUE) {
      leafletProxy("map", session) %>%
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
                    options = pathOptions(pane = "drive"))
    } else {
      leafletProxy("map", session) %>%
        clearGroup("90-Minute Drive from JHSPH")  
    }
    
  })
  
  observe({
    
    if (input$dnr == TRUE) {
      
      pal <- colorFactor(palette = c("#B4D79E",
                                     "#5C8944",
                                     "#D79E9E",
                                     "#FFFFBE",
                                     "#C29ED7"),
                         domain = dnr$DESIG)
      
      leafletProxy("map", session) %>%
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
                    options = pathOptions(pane = "parks"))
    } else {
      leafletProxy("map", session) %>%
        clearGroup("State Department of Natural Resources Land")
    }
    
  })
  
  observe({
    
    if (input$nps == TRUE) {
      leafletProxy("map", session) %>%
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
                    options = pathOptions(pane = "parks"))
    } else {
      leafletProxy("map", session) %>%
        clearGroup("US National Park Service Land")
    }
    
  })
  
  observe({
    
    if (input$fws == TRUE) {
      leafletProxy("map", session) %>%
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
                    options = pathOptions(pane = "parks"))
    } else {
      leafletProxy("map", session) %>%
        clearGroup("US Fish and Wildlife Service Land")
    }
    
  })
  
  observe({
    
    if (input$golf == TRUE) {
      leafletProxy("map", session) %>%
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
                         options = pathOptions(pane = "points"))
    } else {
      leafletProxy("map", session) %>%
        clearGroup("Golf Courses")
    }
    
  })
  
  observe({
    
    if (input$locparks == TRUE) {
      leafletProxy("map", session) %>%
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
                         options = pathOptions(pane = "points"))
    } else {
      leafletProxy("map", session) %>%
        clearGroup("Local Parks")
    }
    
  })
  
  observe({
    
    if (input$lyme == TRUE) {
      
      lyme_popup <- paste(
        counties$NAMELSAD,
        "<br>",
        "Incidence per 100,000: ", counties$INCIDENCE,
        "<br>",
        "Cases: ", counties$CASES,
        sep = "") %>%
        lapply(htmltools::HTML)
      
      lyme_bins <- c(0, 10, 25, 50, 150, 320)
      
      lyme_pal <-  colorBin(palette = "YlOrBr",
                            domain = counties$INCIDENCE,
                            bins = lyme_bins)
      
      leafletProxy("map", session) %>%
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
                    options = pathOptions(pane = "lyme")) %>%
        addLegend(position = "topleft",
                  pal = lyme_pal,
                  values = counties$INCIDENCE,
                  title = "Lyme Disease Incidence (2022)",
                  opacity = 0.75,
                  group = "Lyme Disease Incidence (2022)",
                  layerId = "lyme_legend"
        )
    } else {
      leafletProxy("map", session) %>%
        clearGroup("Lyme Disease Incidence (2022)") %>%
        removeControl(layerId = "lyme_legend")
    }
    
  })
  
  observe({
    
    if (input$d2fe == TRUE) {
      
      if (!exists("d2fe")) {
        d2fe <- read_stars("data/distance_to_forest_edge_3857.tif")
        
        pal_d2fe <- colorNumeric(palette = "viridis",
                                 na.color = "transparent",
                                 domain = d2fe$distance_to_forest_edge_3857.tif,
                                 reverse = TRUE)
        leafletProxy("map", session) %>%
          addStarsImage(d2fe,
                        colors = pal_d2fe,
                        group = "Distance to Forest Edge") %>%
          showGroup("Distance to Forest Edge") %>%
          addLegend(position = "topleft",
                    pal = pal_d2fe,
                    values = d2fe$distance_to_forest_edge_3857.tif,
                    opacity = 1,
                    title = "Distance to Forest Edge (m)",
                    group = "Distance to Forest Edge",
                    layerId = "d2fe_legend")
        
        assign("d2fe", d2fe, envir = .GlobalEnv)
        
      } else {
        
        pal_d2fe <- colorNumeric(palette = "viridis",
                                 na.color = "transparent",
                                 domain = d2fe$distance_to_forest_edge_3857.tif,
                                 reverse = TRUE)
        
        leafletProxy("map", session) %>%
          showGroup("Distance to Forest Edge") %>%
          addLegend(position = "topleft",
                    pal = pal_d2fe,
                    values = d2fe$distance_to_forest_edge_3857.tif,
                    opacity = 1,
                    title = "Distance to Forest Edge (m)",
                    group = "Distance to Forest Edge",
                    layerId = "d2fe_legend")
      }
    } else {
      leafletProxy("map", session) %>%
        hideGroup("Distance to Forest Edge") %>%
        removeControl(layerId = "d2fe_legend")
    }
    
  })
  
  observe({
    
    if (input$d2w == TRUE) {
      
      if (!exists("d2w")) {
        d2w <- read_stars("data/distance_to_water_3857.tif")
        
        pal_d2w <- colorNumeric(palette = "viridis",
                                na.color = "transparent",
                                domain = d2w$distance_to_water_3857.tif,
                                reverse = TRUE)
        leafletProxy("map", session) %>%
          addStarsImage(d2w,
                        colors = pal_d2w,
                        group = "Distance to Water") %>%
          showGroup("Distance to Water") %>%
          addLegend(position = "topleft",
                    pal = pal_d2w,
                    values = d2w$distance_to_water_3857.tif,
                    opacity = 1,
                    title = "Distance to Water (m)",
                    group = "Distance to Water",
                    layerId = "d2w_legend")
        
        assign("d2w", d2w, envir = .GlobalEnv)
        
      } else {
        
        pal_d2w <- colorNumeric(palette = "viridis",
                                na.color = "transparent",
                                domain = d2w$distance_to_water_3857.tif,
                                reverse = TRUE)
        
        leafletProxy("map", session) %>%
          showGroup("Distance to Water") %>%
          addLegend(position = "topleft",
                    pal = pal_d2w,
                    values = d2w$distance_to_water_3857.tif,
                    opacity = 1,
                    title = "Distance to Water (m)",
                    group = "Distance to Water",
                    layerId = "d2w_legend")
      }
    } else {
      leafletProxy("map", session) %>%
        hideGroup("Distance to Water") %>%
        removeControl(layerId = "d2w_legend")
    }
    
  })
  
  observe({
    
    if (input$d2r == TRUE) {
      
      if (!exists("d2r")) {
        d2r <- read_stars("data/distance_to_roads_3857.tif")
        
        pal_d2r <- colorNumeric(palette = "viridis",
                                na.color = "transparent",
                                domain = d2r$distance_to_roads_3857.tif,
                                reverse = TRUE)
        leafletProxy("map", session) %>%
          addStarsImage(d2r,
                        colors = pal_d2r,
                        group = "Distance to Road") %>%
          showGroup("Distance to Road") %>%
          addLegend(position = "topleft",
                    pal = pal_d2r,
                    values = d2r$distance_to_roads_3857.tif,
                    opacity = 1,
                    title = "Distance to Road (m)",
                    group = "Distance to Road",
                    layerId = "d2r_legend")
        
        assign("d2r", d2r, envir = .GlobalEnv)
        
      } else {
        
        pal_d2r <- colorNumeric(palette = "viridis",
                                na.color = "transparent",
                                domain = d2r$distance_to_roads_3857.tif,
                                reverse = TRUE)
        
        leafletProxy("map", session) %>%
          showGroup("Distance to Road") %>%
          addLegend(position = "topleft",
                    pal = pal_d2r,
                    values = d2r$distance_to_roads_3857.tif,
                    opacity = 1,
                    title = "Distance to Road (m)",
                    group = "Distance to Road",
                    layerId = "d2r_legend")
      }
    } else {
      leafletProxy("map", session) %>%
        hideGroup("Distance to Road") %>%
        removeControl(layerId = "d2r_legend")
    }
    
  })
  
  observe({
    
    if (input$elev == TRUE) {
      
      if (!exists("elev")) {
        elev <- read_stars("data/elevation_3857.tif")
        
        pal_elev <- colorNumeric(palette = "viridis",
                                 na.color = "transparent",
                                 domain = elev$elevation_3857.tif,
                                 reverse = TRUE)
        leafletProxy("map", session) %>%
          addStarsImage(elev,
                        colors = pal_elev,
                        group = "Elevation") %>%
          showGroup("Elevation") %>%
          addLegend(position = "topleft",
                    pal = pal_elev,
                    values = elev$elevation_3857.tif,
                    opacity = 1,
                    title = "Elevation (m)",
                    group = "Elevation",
                    layerId = "elev_legend")
        
        assign("elev", elev, envir = .GlobalEnv)
        
      } else {
        
        pal_elev <- colorNumeric(palette = "viridis",
                                 na.color = "transparent",
                                 domain = elev$elevation_3857.tif,
                                 reverse = TRUE)
        
        leafletProxy("map", session) %>%
          showGroup("Elevation") %>%
          addLegend(position = "topleft",
                    pal = pal_elev,
                    values = elev$elevation_3857.tif,
                    opacity = 1,
                    title = "Elevation (m)",
                    group = "Elevation",
                    layerId = "elev_legend")
      }
    } else {
      leafletProxy("map", session) %>%
        hideGroup("Elevation") %>%
        removeControl(layerId = "elev_legend")
    }
    
  })
  
}