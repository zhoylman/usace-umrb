library(sf)
library(rgeos)
library(tidyverse)
library(magrittr)

`%notin%` = Negate(`%in%`)

#downloaded from https://psl.noaa.gov/data/gridded/data.narr.pressure.html
#nc = brick('/home/zhoylman/Downloads/air.mon.mean.nc')
#temp = nc[[1]]
#writeRaster(temp, '/home/zhoylman/usace-umrb/data/narr_221_template.tif')
narr_221_template = raster('~/usace-umrb/data/narr_221_template.tif')

#import clipping criteria
#Umrb boundry
umrb = st_read('~/usace-umrb/data/UMRB_outline.shp') %>%
  rmapshaper::ms_simplify() %>%
  st_transform(st_crs(5070))

#Under 5500ft shp
under_5500 = st_read('~/usace-umrb/data/umrb_dem_5500.shp') %>%
  rmapshaper::ms_simplify() %>%
  st_transform(st_crs(5070))%>%
  st_make_valid() %>%
  sf::st_intersection(., umrb) %>%
  st_geometry()

#states
states = st_read('~/usace-umrb/data/states.shp') %>%
  filter(STATE_ABBR %in% c('MT',"SD","ND",'WY','NE')) %>%
  #filter(STATE_ABBR %in% c('SD')) %>%
  st_transform(st_crs(5070))

#total domain 
domain = umrb %>%
  #st_intersection(states) %>%
  st_intersection(under_5500)

plot(domain$geometry, main = paste0('UMRB Domain Area = ',round(st_area(domain %>% st_union())*3.861e-7), ' mi²'))

conus = states = st_read('~/usace-umrb/data/states.shp') %>%
  filter(STATE_ABBR %notin% c('VI', 'AK', 'HI')) %>%
  #filter(STATE_ABBR %in% c('SD')) %>%
  st_transform(st_crs(5070))

## A function to identify polygons that retain >= XX% of their after clipping
intersects_percent <- function(x,y,percent){
  x %>%
    dplyr::mutate(id = 1:n(),
                  full_area = sf::st_area(geometry)) %>%
    dplyr::left_join(x %>%
                       dplyr::mutate(id = 1:n()) %>%
                       sf::st_intersection(y) %>%
                       dplyr::mutate(cropped_area = sf::st_area(geometry)) %>%
                       sf::st_drop_geometry()) %>%
    dplyr::mutate(included = ((cropped_area/full_area) %>%
                                units::drop_units() %>%
                                magrittr::is_weakly_greater_than(percent/100)),
                  included = tidyr::replace_na(included, FALSE)) %>%
    dplyr::arrange(id) %$%
    included
}

###################################################
########### ALBERS EQUAL AREA HEXAGONAL ########### 
###################################################
#define area
A = 1.295e+9 # 500mi2 in m2

# Corresponding cellsize (length between focal points (hex centers)) :
CS = 2 * sqrt(A/((3*sqrt(3)/2))) * sqrt(3)/2

#set seed for reproducability
#compute base grid for CONUS so that it can be adopted if desired
set.seed(10)
base_grid = spsample(conus %>% as_Spatial(), type="hexagonal", cellsize=CS) %>% #, cellsize=1000)
  HexPoints2SpatialPolygons()%>%
  st_as_sf()

plot(base_grid$geometry, border = 'red')
plot(conus %>% st_geometry(), add = T)

st_write(base_grid, '~/usace-umrb/output/conus_hex_500mi2_albers.shp')

#compute final hex grid depending conditional constraints
final_grid =
  base_grid %>%
  dplyr::mutate(UMRB = intersects_percent(base_grid, umrb %>% st_transform(st_crs(5070)) %>%  sf::st_union(), 45),
                Elevation = intersects_percent(base_grid, under_5500 %>% st_transform(st_crs(5070)) %>%  sf::st_union(), 45),
                Included = as.logical(UMRB * Elevation)) %>%
  filter(Included == T)

plot(umrb$geometry %>% st_transform(st_crs(5070)), main = paste0('\nAlbers Equal Area (epsg: 5070)\n 500mi² Total Number = ', length(final_grid$geometry)))
plot(states$geometry %>% st_transform(st_crs(5070)), add = T)
plot(under_5500 %>% st_transform(st_crs(5070)), add = T)
plot(final_grid$geometry,add = T, border = 'red')

st_write(final_grid, '~/usace-umrb/output/umrb_hex_500mi2_albers.shp')