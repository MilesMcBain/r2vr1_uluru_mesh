library(sf)
library(raster)
library(tidyverse)

##### Shapes #####

# mymaps.google.com is a handy tool for determining bounding boxes.
uluru_bbox_poly <- 
  st_bbox(c(ymin = -25.39521, 
            ymax = -25.2922, 
            xmax = 131.09814, 
            xmin = 130.97454), 
            crs = st_crs("+proj=longlat +ellps=WGS84")) %>%
    st_as_sfc() 
    # An sf bbox is not geometry... this converts it :S

# Contours from all over Uluru-Kata Tjuta National Park                    
uluru_kata_shape <- 
read_sf("./data/GEODATA_TOPO250K_TILE_DATA/G5208/Shapefiles/Elevation/g5208_contours.shx")

uluru_contours <- 
    filter(uluru_kata_shape, 
           as.vector(
             st_contains(uluru_bbox_poly, 
                        uluru_kata_shape$geometry, 
                        sparse = FALSE)
           ))

# Find the outer contour by finding the one with the highest longitude?
uluru_outline <- 
  uluru_contours %>%
  mutate(max_y = map_dbl(geometry,
                         ~max(as.matrix(.)[,1]) )) %>% 
  arrange(desc(max_y)) %>% 
  filter(row_number() == 1)

  plot(uluru_contours["FEATTYPE"], 
    main = "Uluru Elevation Contours")
  plot(uluru_outline["FEATTYPE"], 
    main = "Outline Contour")

# check type of uluru outline
uluru_outline$geometry

as.matrix(uluru_outline$geometry[[1]])

# convert to polygon
uluru_outline_poly <- 
  uluru_outline %>%
  st_geometry() %>%
  pluck(1) %>%
  as.matrix() %>%
  list() %>%
  list() %>%
  st_multipolygon() %>%
  st_sfc()

st_crs(uluru_outline_poly) <- st_crs(uluru_contours)

##### Rasters #####
nt_raster <- raster("./data/ELVIS_CLIP.tif")

# sanity check - do these overlap?
# convert uluru_outline_poly to raster CRS

uluru_outline_poly <-
  st_transform(uluru_outline_poly, crs(nt_raster)@projargs)

# plot and overlap
plot(nt_raster, main="Uluru outline over elevation raster")
lines(as(uluru_outline_poly, 'Spatial'))


##### Triangulating A Shape #####

uluru_trimesh <- sf_to_trimesh(uluru_outline_poly, 2500)
plot(uluru_trimesh, main = "Triangular mesh of Uluru outline polygon")

##### Adding raster Elevation #####
