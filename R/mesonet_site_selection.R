library(reticulate)
library(rgee)
library(cptcity)
library(raster)
library(stars)
library(sf)
library(tidyverse)
library(leaflet.extras2)

source('~/usace-umrb/R/ee_functions.R')

use_condaenv("gee-base", conda = "auto",required = TRUE)
ee = import("ee")
ee_Initialize(email = 'zhoylman@gmail.com', drive = TRUE)

#import grid
hex_grid = st_read('~/usace-umrb/output/umrb_hex_500mi2_albers.shp') %>%
  st_geometry() %>%
  sf_as_ee() 

#import umrb
umrb_outline = st_read('/home/zhoylman/MCO/USACE/umrb_ace/UMRB_outline.shp') %>%
  st_geometry() %>%
  st_transform(4326)

# import modal soils
modal_statsgo_hex = ee$FeatureCollection('users/zhoylman/umrb_hex_500mi2_albers_modal_statsgo')

#put it all to nice vectors and lists for looping if desired
clim_names = c('swe', 'pr', 'tmmx', 'def', 'aet')
grid_names = c('hex')
base = paste0(ee_get_assethome(), '/usace-umrb/UMRB_USACE_clim_conditional')
ee_hex_assets = apply(expand.grid(base,clim_names, grid_names[1]), 1, paste, collapse="_") 
terrain_asset = ee$Image(paste0(ee_get_assethome(), '/usace-umrb/UMRB_USACE_terrain_conditional'))

#precompute the final maskes merged for export
#just climate
selection = ee$Image(ee_hex_assets[1])$
  updateMask(ee$Image(ee_hex_assets[2]))$
  updateMask(ee$Image(ee_hex_assets[3]))$
  #updateMask(ee$Image(ee_hex_assets[4]))$
  #updateMask(ee$Image(ee_hex_assets[5]))$
  updateMask(terrain_asset)

#with soils
selection_w_soils = ee$Image(ee_hex_assets[1])$
  updateMask(ee$Image(ee_hex_assets[2]))$
  updateMask(ee$Image(ee_hex_assets[3]))$
  #updateMask(ee$Image(ee_hex_assets[4]))$
  #updateMask(ee$Image(ee_hex_assets[5]))$
  updateMask(terrain_asset)$
  clip(modal_statsgo_hex)

#grid for visualiztions
grid_map = Map$addLayer(
  eeObject = ee$Image()$paint(hex_grid, 0, 1),
  visParams = {},
  name = "Grid"
)
#plot it if you want
Map$setCenter(lon = -103,lat = 46,zoom = 6)
Map$addLayer(
  eeObject = selection,
  visParams = {},
  name = "Valid Pixels"
) + 
  Map$addLayer(
    eeObject = selection_w_soils,
    visParams = list(palette = (c("#FF0000"))),
    name = "Valid Pixels (soils)"
  ) +grid_map

#export the final assets for easy viewing on EE app
assetid = paste0(ee_get_assethome(), 
                 '/usace-umrb/UMRB_USACE_selection_hex_soils')
task_img <- ee_image_to_asset(
  image = selection_w_soils,
  assetId = assetid,
  overwrite = TRUE,
  scale = 30,
  region = ee_roi$geometry(),
  maxPixels = 100000000000
)

task_img$start()

assetid = paste0(ee_get_assethome(), 
                 '/usace-umrb/UMRB_USACE_selection_hex_clim')
task_img <- ee_image_to_asset(
  image = selection,
  assetId = assetid,
  overwrite = TRUE,
  scale = 30,
  region = ee_roi$geometry(),
  maxPixels = 100000000000
)

task_img$start()

#final map for export (local HTML file)
grid_map = Map$addLayer(
  eeObject = ee$Image()$paint(albers_grid, 0, 1),
  visParams = {},
  name = "Grid"
)
roads_map = Map$addLayer(
  eeObject = ee$Image()$paint(ee$FeatureCollection('users/zhoylman/Roads'), 0, 1),
  visParams = {list(palette = (c("#ff00ff ")))},
  name = "Roads",
  shown = F
)
publicLands_map = Map$addLayer(
  eeObject = ee$FeatureCollection('users/zhoylman/Montana_PublicLands'),
  visParams = list(
    pointRadius = 10,
    color = c("00FFFF")
  ),
  name = "Public Lands",
  opacity = 0.75,
  shown = F
)
Map$setCenter(lon = -103,lat = 46,zoom = 6)
Map$addLayer(
  eeObject = ee$Image('users/zhoylman/usace-umrb/UMRB_USACE_selection_albers_final'),
  visParams = {},
  name = "Valid Pixels"
) + Map$addLayer(
  eeObject = ee$Image('users/zhoylman/usace-umrb/UMRB_USACE_selection_albers_final_soils'),
  visParams = list(palette = (c("#228B22"))),
  name = "Valid Pixels (soils)"
) + grid_map + publicLands_map + roads_map
