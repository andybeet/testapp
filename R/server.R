#' Server function
#'
#' code to execute server scripts
#'
#' @param input input for app
#' @param output output for app
#' @param session session info
#' 
#'@importFrom shiny observeEvent
#'@importFrom leaflet clearGroup leafletProxy addCircleMarkers addPolygons addPolylines removeShape
#'@importFrom magrittr %>%
#'
#'
#'
#'@export

server <-  function(input, output, session) {
  
  ## Read in 75m depth contour (~40 fathoms)
  contour75m <-  sf::st_read(dsn = here::here("GIS","Contours"), layer = '75m_contour',quiet=T)
  setcrs <- sf::st_crs(contour75m)
  
  ## Stat Areas
  statAreas <-  sf::st_read(dsn = here::here("GIS","Statistical_Areas"), layer = 'Statistical_Areas_Simplified',quiet=T)
  statAreas <- sf::st_transform(statAreas,crs = setcrs)
  ## Unique Stat Areas for selectInput
  selectAreas <-  sort(statAreas$Id)
  
  ## 10min square EPUs
  EPU10MinSq <- sf::st_read(dsn = here::here("GIS","EPU_WITH_ESTUARIES"), layer = 'EPUS_FULL',quiet=T)
  EPU10MinSq <- sf::st_transform(EPU10MinSq,crs = setcrs)
  
  # Species we have data for
  speciesList <- readRDS(here::here("data","speciesList.rds"))
  # Location of catch by species, and year
  gearData <- NULL
  for (ifile in 1:length(speciesList$species)){
    gearData[[as.character(speciesList$species[ifile])]] <- readRDS(paste0("data/",speciesList$fname[ifile]))
  }
  

#################
## Leaflet map ##
#################

output$catch_map = leaflet::renderLeaflet({
  # initiates rendering. This all remains same for whole instance of app
  leaflet::leaflet() %>%
    leaflet::setView(lng = -68.73742, lat = 42.31386, zoom = 6) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>%
    leaflet::addScaleBar(position = 'bottomright', options = leaflet::scaleBarOptions(maxWidth = 250))

})

################
#    Plot map  #
################  

    shiny::observeEvent(input$plot_data,{
      
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