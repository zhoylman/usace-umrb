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

narr_221_template = raster('~/usace-umrb/data/narr_221_template.tif')

hex_grid = st_read('/home/zhoylman/usace-umrb/output/station_selection_grids/hex_grid.shp') %>%
  st_geometry() %>%
  sf_as_ee() 

rect_grid = st_read('/home/zhoylman/usace-umrb/output/station_selection_grids/rect_grid.shp') %>%
  st_geometry() %>%
  sf_as_ee()

umrb_outline = st_read('/home/zhoylman/MCO/USACE/umrb_ace/UMRB_outline.shp') %>%
  st_geometry() %>%
  st_transform(4326)

modal_statsgo = ee$FeatureCollection('users/zhoylman/hex_modal_statsgo')

Map$setCenter(lon = -103,lat = 46,zoom = 6)
Map$addLayer(
  eeObject = ee$Image()$paint(modal_statsgo, 0, 1),
  visParams = {},
  name = "Valid Pixels"
)

clim_names = c('swe', 'pr', 'tmmx', 'def', 'aet')
grid_names = c('hex', 'rect')
base = paste0(ee_get_assethome(), '/usace-umrb/UMRB_USACE_clim_conditional')
ee_hex_assets = apply(expand.grid(base,clim_names, grid_names[1]), 1, paste, collapse="_") 
ee_rect_assets = apply(expand.grid(base,clim_names, grid_names[2]), 1, paste, collapse="_") 
terrain_asset = ee$Image(paste0(ee_get_assethome(), '/usace-umrb/UMRB_USACE_terrain_conditional'))

grid = 'hex' 

if(grid == 'hex'){
  selection = ee$Image(ee_hex_assets[1])$
    updateMask(ee$Image(ee_hex_assets[2]))$
    updateMask(ee$Image(ee_hex_assets[3]))$
    updateMask(ee$Image(ee_hex_assets[4]))$
    updateMask(ee$Image(ee_hex_assets[5]))$
    updateMask(terrain_asset)
  
  selection_w_soils = ee$Image(ee_hex_assets[1])$
    updateMask(ee$Image(ee_hex_assets[2]))$
    updateMask(ee$Image(ee_hex_assets[3]))$
    updateMask(ee$Image(ee_hex_assets[4]))$
    updateMask(ee$Image(ee_hex_assets[5]))$
    updateMask(terrain_asset)$
    clip(modal_statsgo)
    
  grid_map = Map$addLayer(
    eeObject = ee$Image()$paint(hex_grid, 0, 1),
    visParams = {},
    name = "Grid"
  )
  
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
} else {
  selection = ee$Image(ee_rect_assets[1])$
    updateMask(ee$Image(ee_rect_assets[2]))$
    updateMask(ee$Image(ee_rect_assets[3]))$
    updateMask(ee$Image(ee_rect_assets[4]))$
    updateMask(ee$Image(ee_rect_assets[5]))$
    updateMask(terrain_asset)
  
  selection = ee$Image(ee_rect_assets[1])$
    updateMask(ee$Image(ee_rect_assets[2]))$
    updateMask(ee$Image(ee_rect_assets[3]))$
    updateMask(ee$Image(ee_rect_assets[4]))$
    updateMask(ee$Image(ee_rect_assets[5]))$
    updateMask(terrain_asset)
  
  grid_map = Map$addLayer(
    eeObject = ee$Image()$paint(rect_grid, 0, 1),
    visParams = {},
    name = "Grid"
  )
  
  Map$setCenter(lon = -103,lat = 46,zoom = 6)
  Map$addLayer(
    eeObject = selection,
    visParams = {},
    name = "Valid Pixels"
  ) + grid_map
}
