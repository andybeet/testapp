############
## Server ##
############

server <-  function(input, output, session) {
  
#################
## Leaflet map ##
#################

output$catch_map = renderLeaflet({
  # initiates rendering. This all remains same for whole instance of app
  leaflet() %>%
    setView(lng = -68.73742, lat = 42.31386, zoom = 6) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250))

})

################
#    Plot map  #
################  

    observeEvent(input$plot_data,{
      
      # clear curnt figure of points
      leafletProxy('catch_map') %>% clearGroup(group = 'year Obs')
      
      yr <- as.character(input$year_from)
      species <- input$species
      
      latlongs <- gearData[[species]][[yr]]
      #print(latlongs[1:10,])
    
     
      leafletProxy('catch_map') %>%
        addCircleMarkers(group = "year Obs",lng = latlongs$GIS_LON,lat = latlongs$GIS_LAT, radius = 20.0*latlongs$catch/(max(latlongs$catch)), 
                         fillColor = '#E59828', fillOpacity = 1.0, stroke = FALSE)
    })

################
## Stat Areas ##
################

    ## Add button
    observeEvent(input$stat_areas_add, {
      
      if(input$stat_areas == 'stat') {
        ## Grab stat areas
        addArea = statAreas[statAreas$Id %in% selectAreas,]
        
        ## Labels (at this point filtering is not needed, but added in case it becomes important later on)
        #pltLabels = stat_area_labels %>% filter(area %in% addArea@data$Id)           
        
      }
      
      leafletProxy('catch_map') %>% clearGroup(group = 'Indicator') %>% 
        addPolygons(group = 'Indicator', data = addArea, stroke = TRUE, color = '#ffffff', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)

    })
    
    ## Remove
    observeEvent({ input$stat_areas_remove; input$select_stat_areas }, {
      
      ## Remove map objects
      leafletProxy('catch_map') %>% clearGroup(group = 'Indicator')
      
    }, ignoreNULL = FALSE)
    
    
    ##############################
    ###### EPU checkboxes ########
    ##############################
    
    # 10 minute squares
    observeEvent(input$EPU_10, {
      
      data <- raster::subset(EPU10MinSq,EPU=="GB")
      
      if (input$EPU_10 == T) {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_10') %>%
          addPolygons(group = 'EPU_10',data = data, stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, 
                      fillColor = "#CBC9C8", fillOpacity = 0.3)
      } else {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_10')
      }
      
    })
    
    
    observeEvent(input$EPU_GB, {

      addAreas <- raster::subset(EPU10MinSq,EPU=="GB")
      
      if (input$EPU_GB == T) {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_GB') %>%
          addPolygons(group = 'EPU_GB',data = addAreas,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
      } else {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_GB')
      }
      
    })
    
    observeEvent(input$EPU_GOM, {
      
      addAreas <- raster::subset(EPU10MinSq,EPU=="GOM")
      
      if (input$EPU_GOM == T) {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_GOM') %>%
          addPolygons(group = 'EPU_GOM',data = addAreas,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
      } else {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_GOM')
      }
      
    })
    
    observeEvent(input$EPU_MAB, {
      
      addAreas <- raster::subset(EPU10MinSq,EPU=="MAB")
      
      if (input$EPU_MAB == T) {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_MAB') %>%
          addPolygons(group = 'EPU_MAB',data = addAreas,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
      } else {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_MAB')
      }
      
    })
    
    observeEvent(input$EPU_SS, {
      
      addAreas <- raster::subset(EPU10MinSq,EPU=="SS")
      
      if (input$EPU_SS == T) {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_SS') %>%
          addPolygons(group = 'EPU_SS',data = addAreas,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
      } else {
        leafletProxy("catch_map") %>% clearGroup(group = 'EPU_SS')
      }
      
    })
    
    
    #######################
    ## 75m Depth Contour ##
    #######################
    
    observeEvent(input$contour75m_remove, {
      
      ## Remove contour
      leafletProxy('catch_map') %>% removeShape(layerId = 'Contour_75m')
      
    })  
    
    observeEvent(input$contour75m_add, {
      
      ## Add contour
      leafletProxy('catch_map') %>% addPolylines(layerId = 'Contour_75m', data = contour75m, color = 'grey', weight = 1)  
      
    })    

########################
## Export leaflet map ##
########################

    output$export_map = downloadHandler(
        
        filename = function() {

            paste0(Sys.Date(), '_exported_map.pdf')
            
        },

        content = function(file) {

            mapshot(x = narw_map(), file = file)

        }
    
    )
           
}