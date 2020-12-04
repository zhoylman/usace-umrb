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

ee_roi = st_read("/home/zhoylman/mesonet-dashboard/data/shp/states.shp") %>%
  dplyr::filter(STATE_ABBR %in% c('MT', 'WY', 'ND', 'SD', 'NE')) %>%
  #st_intersection(umrb_outline)%>%
  st_geometry() %>%
  sf_as_ee()

hex_grid = st_read('/home/zhoylman/usace-umrb/output/station_selection_grids/hex_grid.shp') %>%
  st_geometry() %>%
  sf_as_ee() 

rect_grid = st_read('/home/zhoylman/usace-umrb/output/station_selection_grids/rect_grid.shp') %>%
  st_geometry() %>%
  sf_as_ee()

umrb_outline = st_read('/home/zhoylman/MCO/USACE/umrb_ace/UMRB_outline.shp') %>%
  st_geometry() %>%
  st_transform(4326)

modal_statsgo_hex = ee$FeatureCollection('users/zhoylman/hex_modal_statsgo')
modal_statsgo_rect = ee$FeatureCollection('users/zhoylman/rect_modal_statsgo')

clim_names = c('swe', 'pr', 'tmmx', 'def', 'aet')
grid_names = c('hex', 'rect')
base = paste0(ee_get_assethome(), '/usace-umrb/UMRB_USACE_clim_conditional')
ee_hex_assets = apply(expand.grid(base,clim_names, grid_names[1]), 1, paste, collapse="_") 
ee_rect_assets = apply(expand.grid(base,clim_names, grid_names[2]), 1, paste, collapse="_") 
terrain_asset = ee$Image(paste0(ee_get_assethome(), '/usace-umrb/UMRB_USACE_terrain_conditional'))

grid = 'rect' 
export = F

if(grid == 'hex'){
  selection = ee$Image(ee_hex_assets[1])$
    updateMask(ee$Image(ee_hex_assets[2]))$
    updateMask(ee$Image(ee_hex_assets[3]))$
    #updateMask(ee$Image(ee_hex_assets[4]))$
    #updateMask(ee$Image(ee_hex_assets[5]))$
    updateMask(terrain_asset)
  
  selection_w_soils = ee$Image(ee_hex_assets[1])$
    updateMask(ee$Image(ee_hex_assets[2]))$
    updateMask(ee$Image(ee_hex_assets[3]))$
    #updateMask(ee$Image(ee_hex_assets[4]))$
    #updateMask(ee$Image(ee_hex_assets[5]))$
    updateMask(terrain_asset)$
    clip(modal_statsgo_hex)
  
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
    #updateMask(ee$Image(ee_rect_assets[4]))$
    #updateMask(ee$Image(ee_rect_assets[5]))$
    updateMask(terrain_asset)$
    clip(hex_grid)
  
  selection_w_soils = ee$Image(ee_rect_assets[1])$
    updateMask(ee$Image(ee_rect_assets[2]))$
    updateMask(ee$Image(ee_rect_assets[3]))$
    #updateMask(ee$Image(ee_rect_assets[4]))$
    #updateMask(ee$Image(ee_rect_assets[5]))$
    updateMask(terrain_asset)$
    clip(modal_statsgo_rect)$
    clip(hex_grid)
  
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
  ) + Map$addLayer(
    eeObject = selection_w_soils,
    visParams = list(palette = (c("#FF0000"))),
    name = "Valid Pixels (soils)"
  ) + grid_map
  
  if(export == T){
    assetid = paste0(ee_get_assethome(), 
                     '/usace-umrb/UMRB_USACE_selection_rect_final_soils')
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
                     '/usace-umrb/UMRB_USACE_selection_rect_final')
    task_img <- ee_image_to_asset(
      image = selection,
      assetId = assetid,
      overwrite = TRUE,
      scale = 30,
      region = ee_roi$geometry(),
      maxPixels = 100000000000
    )
    
    task_img$start()
  }
}


#final map for export
grid_map = Map$addLayer(
  eeObject = ee$Image()$paint(rect_grid, 0, 1),
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
  eeObject = ee$Image('users/zhoylman/usace-umrb/UMRB_USACE_selection_rect_final'),
  visParams = {},
  name = "Valid Pixels"
) + Map$addLayer(
  eeObject = ee$Image('users/zhoylman/usace-umrb/UMRB_USACE_selection_rect_final_soils'),
  visParams = list(palette = (c("#228B22"))),
  name = "Valid Pixels (soils)"
) + grid_map + publicLands_map + roads_map
