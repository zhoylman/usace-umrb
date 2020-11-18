library(sf)
library(rgeos)

umrb = st_read('~/usace-umrb/data/UMRB_outline.shp') %>%
  as_Spatial()

states = st_read('~/usace-umrb/data/states.shp')

set.seed(10)
hex_grid = spsample(umrb, type="hexagonal", cellsize=150000) %>% #, cellsize=1000)
  HexPoints2SpatialPolygons()%>%
  st_as_sf()

hex_grid$id = 1:length(hex_grid$geometry)

plot(umrb)
plot(hex_grid,add = T)

st_write(hex_grid, '~/usace-umrb/output/hex_grid/hex_grid.geojson')
