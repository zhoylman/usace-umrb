library(sf)
library(rgeos)
library(tidyverse)
library(magrittr)

umrb = st_read('~/usace-umrb/data/UMRB_outline.shp') %>%
  rmapshaper::ms_simplify()

under_5500 = st_read('~/usace-umrb/data/umrb_dem_5500.shp') %>%
  rmapshaper::ms_simplify() %>%
  st_transform(st_crs(umrb))%>%
  st_make_valid() %>%
  sf::st_intersection(., umrb) %>%
  dplyr::select(geometry) 

states = st_read('~/usace-umrb/data/states.shp') %>%
  filter(STATE_ABBR %in% c('MT',"SD","ND",'WY','NE')) %>%
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

#compute base hex grid
A = 1.076e+10 #1000km2 to ft2 (Lambert base unit)
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
  dplyr::mutate(UMRB = intersects_50(base_grid, umrb %>% sf::st_union()),
                States = intersects_50(base_grid, states %>% sf::st_union()),
                Elevation = intersects_50(base_grid, under_5500 %>% sf::st_union()),
                Included = as.logical(UMRB * States * Elevation)) %>%
  dplyr::select(UMRB,
                States,
                Elevation,
                Included) %>%
  filter(Included == T)
  

i = st_intersection(final_grid, states)

i$ids = sapply(i$origins, function(x) paste0(as.character(final_grid$id)[x], collapse = ","))


plot(states$geometry)
plot(final_grid$geometry,add = T)

st_write(final_grid, '~/usace-umrb/output/hex_grid/hex_grid.geojson')
