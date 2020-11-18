library(sf)
library(rgeos)
library(tidyverse)
library(magrittr)

#downloaded from https://psl.noaa.gov/data/gridded/data.narr.pressure.html
#nc = brick('/home/zhoylman/Downloads/air.mon.mean.nc')
#temp = nc[[1]]
#writeRaster(temp, '/home/zhoylman/usace-umrb/data/narr_221_template.tif')
narr_221_template = raster('~/usace-umrb/data/narr_221_template.tif')

umrb = st_read('~/usace-umrb/data/UMRB_outline.shp') %>%
  rmapshaper::ms_simplify() %>%
  st_transform(st_crs(narr_221_template))

under_5500 = st_read('~/usace-umrb/data/umrb_dem_5500.shp') %>%
  rmapshaper::ms_simplify() %>%
  st_transform(st_crs(umrb))%>%
  st_make_valid() %>%
  sf::st_intersection(., umrb) %>%
  st_geometry()

states = st_read('~/usace-umrb/data/states.shp') %>%
  filter(STATE_ABBR %in% c('MT',"SD","ND",'WY','NE')) %>%
  #filter(STATE_ABBR %in% c('SD')) %>%
  st_transform(st_crs(umrb))

## A function to identify polygons that retain >= 50% of their after clipping
intersects_50 <- function(x,y){
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
                                magrittr::is_weakly_greater_than(0.5)),
                  included = tidyr::replace_na(included, FALSE)) %>%
    dplyr::arrange(id) %$%
    included
}

intersects_100 <- function(x,y){
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
                                magrittr::is_weakly_greater_than(0.99999999999)),
                  included = tidyr::replace_na(included, FALSE)) %>%
    dplyr::arrange(id) %$%
    included
}

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

#generate square grid 
rect_grig = narr_221_template %>%
  crop(., umrb) %>%
  spex::polygonize()

#compute base hex grid
A = 1e+9 #1.076e+10 = 1000km2 to ft2 (Lambert base unit), 1.394e+10 = 500mi2
# Corresponding cellsize (length between focal points (hex centers)) :
CS = 2 * sqrt(A/((3*sqrt(3)/2))) * sqrt(3)/2

set.seed(10)
base_grid = spsample(umrb %>% as_Spatial(), type="hexagonal", cellsize=CS) %>% #, cellsize=1000)
  HexPoints2SpatialPolygons()%>%
  st_as_sf() %>%
  st_transform(st_crs(umrb))

## Get the final grid by identifying cells that are >50% in the area of interest
final_grid =
  base_grid %>%
  dplyr::mutate(UMRB = intersects_100(base_grid, umrb %>% sf::st_union()),
                States = intersects_100(base_grid, states %>% sf::st_union()),
                Elevation = intersects_100(base_grid, under_5500 %>% sf::st_union()),
                Included = as.logical(UMRB * States * Elevation)) %>%
  filter(Included == T)

#
final_grid_rect =
  rect_grig %>%
  dplyr::mutate(UMRB = intersects_100(rect_grig, umrb %>% sf::st_union()),
                States = intersects_100(rect_grig, states %>% sf::st_union()),
                Elevation = intersects_percent(rect_grig, under_5500 %>% sf::st_union(),98),
                Included = as.logical(UMRB * States * Elevation)) %>%
  filter(Included == T)
  
i = st_intersects(final_grid_rect, states, sparse = F) %>%
  as.data.frame() %>%
  colSums() %>%
  as.data.frame()
rownames(i) = unique(states$STATE_NAME)

i


plot(umrb$geometry, main = paste0('\n221 Regional North American Grid (Lambert Conformal)\nTotal Number = ', length(final_grid_rect$geometry)))
plot(states$geometry, add = T)
plot(final_grid_rect$geometry,add = T, border = 'red')

plot(umrb$geometry, main = paste0('\n221 Regional North American Grid (Lambert Conformal)\nHexigonal Grid Total Number = ', length(final_grid$geometry)))
plot(states$geometry, add = T)
plot(final_grid$geometry,add = T, border = 'red')

st_write(final_grid, '~/usace-umrb/output/station_selection_grids/hex_grid.shp')
st_write(final_grid_rect, '~/usace-umrb/output/station_selection_grids/rect_grid.shp')
