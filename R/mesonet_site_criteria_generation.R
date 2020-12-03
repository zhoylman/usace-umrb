library(reticulate)
library(rgee)
library(cptcity)
library(raster)
library(stars)
library(sf)
library(tidyverse)
library(leaflet.extras2)
library(soilDB)
library(magrittr)
library(spdplyr)

#import custom ee functions, climatologies and clipping
source('/home/zhoylman/usace-umrb/R/ee_functions.R')

#set up enviorment and initialize ee
use_condaenv("gee-base", conda = "auto",required = TRUE)
#this causes an error, not sure why, but you must import this module
#to inintialize ee
ee = import("ee")
ee_Initialize(email = 'zhoylman@gmail.com', drive = TRUE)

#import grids and convert to ee object
#hex
hex_grid = st_read('/home/zhoylman/usace-umrb/output/station_selection_grids/hex_grid.shp') %>%
  st_geometry() %>%
  sf_as_ee()
#rect
rect_grid = st_read('/home/zhoylman/usace-umrb/output/station_selection_grids/rect_grid.shp') %>%
  st_geometry() %>%
  sf_as_ee()

# Define a region of interest with sf (states)
# for some reason the complex urmb geometry causes 
#ee python api time-out error??? No problem.. States work
ee_roi = st_read("/home/zhoylman/mesonet-dashboard/data/shp/states.shp") %>%
  dplyr::filter(STATE_ABBR %in% c('MT', 'WY', 'ND', 'SD', 'NE')) %>%
  #st_intersection(umrb_outline)%>%
  st_geometry() %>%
  sf_as_ee()

###########################################
################  TERRAIN #################
###########################################
#SRTM elevation
elevation_mask = ee$Image('USGS/SRTMGL1_003')$
  rename('elevation')$
  #clip to the ee_roi
  clip(ee_roi)$
  #less than 5500ft
  lte(1676.4)$
  #selfMask for clipping other vars
  selfMask()

#define terrain mask: slightly more restrictive, for site selection
# but we dont want this to clip the climate data for percentile comps
terrain_mask = elevation_mask$
  #only on slopes less than 10 degrees
  updateMask(ee$Terrain$slope(ee$Image('USGS/SRTMGL1_003'))$
               clip(ee_roi)$
               lte(7)$
               selfMask())

## export terrain mask to asset if wanted..
# assetid <- paste0(ee_get_assethome(), '/usace-umrb/UMRB_USACE_terrain_conditional')
# task_img <- ee_image_to_asset(
#   image = terrain_mask,
#   assetId = assetid,
#   overwrite = TRUE,
#   scale = 30,
#   region = ee_roi$geometry(),
#   maxPixels = 100000000000,
# )
# task_img$start()

###########################################
################  CLIMATE #################
###########################################
#swe is easy, precomputed climatology from topofire, uploaded to asset
swe = ee$Image('users/zhoylman/max_swe_climatology_1986-2015_umrb') %>%
  #run clip mask (this is a custom function in source above)
  #clips to ee_roi, masks to elevation, renames
  clip_mask(., 'swe_climatology', ee_roi, elevation_mask)

#compute climatologies of each of the input climate vars
#clip_mask is a custom function for clipping the data to elevational constraints 
#prior to percentile computations

#cliamtology years
years_climatology = ee$List$sequence(1981,2010)
#base dataset
terraClimate = ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")

pr_climatology = compute_sum_climatology(years_climatology, terraClimate$select('pr')) %>%
  clip_mask(., 'pr_climatology', ee_roi, elevation_mask)

tmmx_climatology = compute_mean_climatology(years_climatology, terraClimate$select('tmmx'))$
  multiply(.1) %>%
  clip_mask(., 'tmmx_climatology', ee_roi, elevation_mask)

def_climatology = compute_sum_climatology(years_climatology, terraClimate$select('def'))$
  multiply(.1) %>%
  clip_mask(., 'def_climatology', ee_roi, elevation_mask)

aet_climatology = compute_sum_climatology(years_climatology, terraClimate$select('aet'))$
  multiply(.1) %>%
  clip_mask(., 'aet_climatology', ee_roi, elevation_mask)

## for loop put em all in a nice lists
clim_list = list(swe, pr_climatology, tmmx_climatology, 
                 def_climatology, aet_climatology)

grids = list(hex_grid, rect_grid)
# define some vectors for name generation for saving the data
clim_names = c('swe', 'pr', 'tmmx', 'def', 'aet')
grid_names = c('hex', 'rect')

###########################################
####### EXPORT CLIM MASKS IF NEEDED #######
###########################################

visualize_clim = FALSE

for(i in 1:length(clim_names)){
  for(g in 1:length(grid_names)){
    #compute upper quantile of clim vals within grids
    upper_grid = percentile_comp(var = clim_list[[i]], grid = grids[[g]], 
                                 percentile_int = 95, scale = 4000)
    #compute lower quantile of clim vals within grids
    lower_grid = percentile_comp(var = clim_list[[i]], grid = grids[[g]], 
                                 percentile_int = 5, scale = 4000)
    #compute valid pixels within percentile grids
    valid_pixels = clim_list[[i]]$
      gte(lower_grid)$
      selfMask()$
      updateMask(clim_list[[i]]$lte(upper_grid))
    
    if(visualize_clim == TRUE){
      #visualize if you so please
      Map$setCenter(lon = -110,lat = 47,zoom = 6)
      
      #add all layers to the map view.. working on a 
      #nicer way to do this.... also takes a while to 
      #render... 
      raw_data_map = Map$addLayer(
        eeObject = clim_list[[i]],
        visParams = list(
          min = 0,
          max = 1000,
          palette = (c("#FF0000","#FFFFFF","#0000FF"))
        ),
        name = "Raw Data"
      )  
      
      lower_map = Map$addLayer(
        eeObject = lower_grid,
        visParams = list(min = 0, max = 1000, palette = c("#FF0000","#FFFFFF","#0000FF")),
        name = "Lower Percentile"
      )  
      
      upper_map = Map$addLayer(
        eeObject = upper_grid,
        visParams = list(min = 0, max = 1000, palette = c("#FF0000","#FFFFFF","#0000FF")),
        name = "Upper Percentile"
      ) 
      
      valid_px_map = Map$addLayer(
        eeObject = valid_pixels,
        visParams = list(min = 0, max = 1, palette = c("black")),
        name = "selected"
      ) 
      
      grid_map = Map$addLayer(
        eeObject = ee$Image()$paint(grids[[g]], 0, 1),
        visParams = {},
        name = "Grid"
      )
      
      #display the map 
      raw_data_map + 
        lower_map +
        upper_map +
        valid_px_map + 
        grid_map
    }
    
    #export clim conditional grids as ee assets for later merger
    #compute asset name
    assetid = paste0(ee_get_assethome(), 
                     '/usace-umrb/UMRB_USACE_clim_conditional_', 
                     clim_names[i], '_', grid_names[g])
    task_img <- ee_image_to_asset(
      image = valid_pixels,
      assetId = assetid,
      overwrite = TRUE,
      scale = 4000,
      region = ee_roi$geometry()
    )
    
    task_img$start()
  }
}

###########################################
################   SOILS  #################
###########################################
#import states again for viz
states = st_read('~/usace-umrb/data/states.shp') %>%
  filter(STATE_ABBR %in% c('MT',"SD","ND",'WY','NE')) 

#import hex and convert to sp for STATSGO data retrieval
hex = st_read('/home/zhoylman/usace-umrb/output/station_selection_grids/hex_grid.shp') %>%
  st_transform(4326) %>%
  st_geometry() %>%
  as_Spatial()

#list to store STATSGO for each hex cell
statsgo_hex = list()

#Define a function to pull in STATSGO and 
#return polygons with dominant soil type
get_modal_statsgo = function(x){
  #get statsgo geometry
  statsgo_geom <- SDA_spatialQuery(
    x, 
    what = 'geom', 
    db = 'STATSGO', 
    geomIntersection = TRUE
  )
  #find the max geom using area, grouped by mukey
  max_unit = statsgo_geom %>%
    filter(mukey == statsgo_geom %>%
             as.data.frame() %>%
             group_by(mukey) %>%
             summarize(sum = sum(area_ac)) %>%
             filter(sum == max(sum)) %$%
             mukey)
  return(max_unit)
}

#loop through all the cells 
# cant use apply on sp objects?! LAME!
for(i in 1:length(hex)){
  statsgo_hex[[i]] = get_modal_statsgo(hex[i])
  print(i)
  if(i == length(hex)){
    statsgo_hex = do.call('rbind', statsgo_hex)
  }
}
#plot and check
plot(hex)
plot(statsgo_hex, add = T, col = 'red')

#convert to sf and export
statsgo_hex_sf = st_as_sf(statsgo_hex)
st_write(statsgo_hex_sf, '/home/zhoylman/usace-umrb/output/station_selection_grids/hex_modal_statsgo.shp')


###########################################
####### SOIL WATER HOLDING CAPACITY  ######
###########################################

#not using this currently in the final grid generation
soil_data = ee$Image('users/zhoylman/statsgo_AWC_conus_240m')%>%
  clip_mask(., 'soil_awc', ee_roi, elevation_mask)

visualize_soil = TRUE

for(g in 1:length(grid_names)){
  #compute upper quantile of soil vals within grids
  upper_grid = percentile_comp(var = soil_data, grid = grids[[g]], 
                               percentile_int = 80, scale = 240)
  #compute lower quantile of soil vals within grids
  lower_grid = percentile_comp(var = soil_data, grid = grids[[g]], 
                               percentile_int = 20, scale = 240)
  #compute valid pixels within percentile grids
  valid_pixels = soil_data$
    gte(lower_grid)$
    selfMask()$
    updateMask(soil_data$lte(upper_grid))
  
  if(visualize_soil == TRUE){
    #visualize if you so please
    Map$setCenter(lon = -110,lat = 47,zoom = 6)
    
    #add all layers to the map view.. working on a 
    #nicer way to do this.... also takes a while to 
    #render... 
    raw_data_map = Map$addLayer(
      eeObject = soil_data,
      visParams = list(
        min = 0,
        max = 20,
        palette = (c("#FF0000","#FFFFFF","#0000FF"))
      ),
      name = "Raw Data"
    )  
    
    lower_map = Map$addLayer(
      eeObject = lower_grid,
      visParams = list(min = 0, max = 20, palette = c("#FF0000","#FFFFFF","#0000FF")),
      name = "Lower Percentile"
    )  
    
    upper_map = Map$addLayer(
      eeObject = upper_grid,
      visParams = list(min = 0, max = 20, palette = c("#FF0000","#FFFFFF","#0000FF")),
      name = "Upper Percentile"
    ) 
    
    valid_px_map = Map$addLayer(
      eeObject = valid_pixels,
      visParams = list(min = 0, max = 1, palette = c("black")),
      name = "selected"
    ) 
    
    grid_map = Map$addLayer(
      eeObject = ee$Image()$paint(grids[[g]], 0, 1),
      visParams = {},
      name = "Grid"
    )
    
    #display the map 
    raw_data_map + 
      lower_map +
      upper_map +
      valid_px_map + 
      grid_map
  }
  
  # compute asset name
  assetid <- paste0(ee_get_assethome(), 
                    '/usace-umrb/UMRB_USACE_soils_conditional_', 
                    grid_names[g])
  #export
  task_img <- ee_image_to_asset(
    image = valid_pixels,
    assetId = assetid,
    overwrite = TRUE,
    scale = 240,
    region = ee_roi$geometry()
  )
  
  task_img$start()
}