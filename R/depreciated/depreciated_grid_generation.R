
#generate square grid 
rect_grig = narr_221_template %>%
  crop(., umrb) %>%
  spex::polygonize()

#compute base hex grid

#Rectangular Grid
final_grid_rect =
  rect_grig %>%
  dplyr::mutate(States = intersects_percent(rect_grig, states %>% sf::st_union(), 50),
                Elevation = intersects_percent(rect_grig, under_5500 %>% sf::st_union(),95),
                Included = as.logical(States * Elevation)) %>%
  filter(Included == T)

#Albers
cell_size = 22.360679775 # in miles - 500mi2

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
  dplyr::mutate(States = intersects_percent(albers_base, states %>% st_transform(st_crs(5070))%>% sf::st_union(), 40),
                Elevation = intersects_percent(albers_base, under_5500 %>% st_transform(st_crs(5070)) %>% sf::st_union(),40),
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