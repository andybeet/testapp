## Load libraries 
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(sf)
library(here)
library(raster)

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

