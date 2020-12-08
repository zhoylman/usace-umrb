library(sf)
library(rgeos)
library(tidyverse)
library(magrittr)

#downloaded from https://psl.noaa.gov/data/gridded/data.narr.pressure.html
#nc = brick('/home/zhoylman/Downloads/air.mon.mean.nc')
#temp = nc[[1]]
#writeRaster(temp, '/home/zhoylman/usace-umrb/data/narr_221_template.tif')

narr_221_template = raster('~/usace-umrb/data/narr_221_template.tif')

#import clipping criteria
#Umrb boundry
umrb = st_read('~/usace-umrb/data/UMRB_outline.shp') %>%
  rmapshaper::ms_simplify() %>%
  st_transform(st_crs(narr_221_template))

#Under 5500ft shp
under_5500 = st_read('~/usace-umrb/data/umrb_dem_5500.shp') %>%
  rmapshaper::ms_simplify() %>%
  st_transform(st_crs(umrb))%>%
  st_make_valid() %>%
  sf::st_intersection(., umrb) %>%
  st_geometry()

#states
states = st_read('~/usace-umrb/data/states.shp') %>%
  filter(STATE_ABBR %in% c('MT',"SD","ND",'WY','NE')) %>%
  #filter(STATE_ABBR %in% c('SD')) %>%
  st_transform(st_crs(umrb))

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

#generate square grid 
rect_grig = narr_221_template %>%
  crop(., umrb) %>%
  spex::polygonize()

#compute base hex grid
A = 1.4e+9#1e+9 #1.076e+10 = 1000km2 to ft2 (Lambert base unit), 1.394e+10 = 500mi2
#A = 1.2e+9
# Corresponding cellsize (length between focal points (hex centers)) :
CS = 2 * sqrt(A/((3*sqrt(3)/2))) * sqrt(3)/2

set.seed(10)
base_grid = spsample(umrb %>% as_Spatial(), type="hexagonal", cellsize=CS) %>% #, cellsize=1000)
  HexPoints2SpatialPolygons()%>%
  st_as_sf() %>%
  st_transform(st_crs(umrb))

# base Albers
umrb_extent = extent(st_read('~/usace-umrb/data/UMRB_outline.shp') %>%
               rmapshaper::ms_simplify() %>%
               st_transform(st_crs(5070)) %>% 
               extent())

## Get the final grid by identifying cells that are >50% in the area of interest
# final_grid =
#   base_grid %>%
#   dplyr::mutate(UMRB = intersects_100(base_grid, umrb %>% sf::st_union()),
#                 States = intersects_100(base_grid, states %>% sf::st_union()),
#                 Elevation = intersects_100(base_grid, under_5500 %>% sf::st_union()),
#                 Included = as.logical(UMRB * States * Elevation)) %>%
#   filter(Included == T)

#compute final hex grid depending conditional constraints
final_grid =
  base_grid %>%
  dplyr::mutate(States = intersects_percent(base_grid, states %>% sf::st_union(), 30),
                Elevation = intersects_percent(base_grid, under_5500 %>% sf::st_union(),60),
                Included = as.logical(Elevation * States)) %>%
  filter(Included == T)

#Rectangular Grid
final_grid_rect =
  rect_grig %>%
  dplyr::mutate(States = intersects_percent(rect_grig, states %>% sf::st_union(), 50),
                Elevation = intersects_percent(rect_grig, under_5500 %>% sf::st_union(),95),
                Included = as.logical(States * Elevation)) %>%
  filter(Included == T)
  
#Albers
cell_size = 22 # in miles

albers_base = 
  raster(xmn=umrb_extent[1], xmx=umrb_extent[2], ymn=umrb_extent[3], ymx=umrb_extent[4],
         res = c(1609.34 * cell_size, 1609.34 * cell_size),
         crs = sp::CRS("+init=epsg:5070")) %>%
  spex::polygonize() %>%
  dplyr::filter(sf::st_intersects(st_read('~/usace-umrb/data/UMRB_outline.shp') %>%
                                    rmapshaper::ms_simplify() %>%
                                    st_transform(st_crs(5070)),
                                  sparse = FALSE)[,1])

plot(st_read('~/usace-umrb/data/UMRB_outline.shp') %>%
       rmapshaper::ms_simplify() %>%
       st_transform(st_crs(5070)) %>%
       st_geometry(), col = 'red'); plot(albers_base, add = T)

albers_final = albers_base %>%
  dplyr::mutate(States = intersects_percent(albers_base, states %>% st_transform(st_crs(5070))%>% sf::st_union(), 50),
                Elevation = intersects_percent(albers_base, under_5500 %>% st_transform(st_crs(5070)) %>% sf::st_union(),50),
                Included = as.logical(States * Elevation)) %>%
  filter(Included == T)

plot(umrb$geometry %>% st_transform(st_crs(5070)), main = paste0('\nAlbers Equal Area (epsg: 5070)\nTotal Number = ', length(albers_final$geometry)))
plot(states$geometry %>% st_transform(st_crs(5070)), add = T)
plot(under_5500 %>% st_transform(st_crs(5070)), add = T)
plot(albers_final$geometry,add = T, border = 'red')

plot(umrb$geometry, main = paste0('\n221 Regional North American Grid (Lambert Conformal)\nTotal Number = ', length(final_grid_rect$geometry)))
plot(states$geometry, add = T)
plot(under_5500, add = T)
plot(final_grid_rect$geometry,add = T, border = 'red')

plot(umrb$geometry, main = paste0('\n221 Regional North American Grid (Lambert Conformal)\nTotal Number = ', length(final_grid_rect$geometry)))
plot(states$geometry, add = T)
plot(under_5500, add = T)
plot(final_grid_rect$geometry,add = T, border = 'red')

plot(umrb$geometry, main = paste0('\n221 Regional North American Grid (Lambert Conformal)\nHexigonal Grid Total Number = ', length(final_grid$geometry)))
plot(states$geometry, add = T)
plot(under_5500, add = T)
plot(final_grid$geometry,add = T, border = 'red')

i = st_intersects(final_grid_rect, states, sparse = F) %>%
  as.data.frame() %>%
  colSums() %>%
  as.data.frame()
rownames(i) = unique(states$STATE_NAME)

i

st_write(final_grid, '~/usace-umrb/output/station_selection_grids/hex_grid.shp')
st_write(final_grid_rect, '~/usace-umrb/output/station_selection_grids/rect_grid.shp')
st_write(albers_final, '~/usace-umrb/output/station_selection_grids/albers_grid.shp')
