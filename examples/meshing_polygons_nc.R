
library(sf)
library(raster)
library(RTriangle)
library(tidyverse)
source("./helpers/sf_to_trimesh.R")

# Some Fake  Polygon Data
p1 <- rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)) # block
p2 <- rbind(c(0.25,0.25), c(0.75,0.25), c(0.75,0.75), c(0.25,0.75), c(0.25,0.25)) # hole
p3 <- rbind(c(0,1), c(1,1), c(1,2), c(0,2), c(0,1)) # block
p4 <- rbind(c(0.25,1.25), c(0.75,1.25), c(0.75,1.75), c(0.25,1.75), c(0.25,1.25)) # hole
p5 <- rbind(c(2,1.1), c(3,1.1), c(3,2), c(2,2), c(2, 1.1)) # mini-block
p6 <- rbind(c(2.25,1.25), c(2.75,1.25), c(2.75,1.75), c(2.25,1.75), c(2.25,1.25)) # hole
p7 <- rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0)) # block

mpol_1 <- st_multipolygon(list(list(p1,p2), list(p3,p4)))
mpol_2 <- st_multipolygon(list(list(p5,p6), list(p7)))

# An sf comprised of multiple polygons with shared boundaries containing holes
my_sf <- st_sfc(mpol_1, 
                mpol_2)

# Plot the fake data
plot(my_sf)

# North Carlina State data. A geometry combining many polygons.
nc <- read_sf(system.file("shape/nc.shp", package="sf"))

# Plot North Carolina
plot(nc["AREA"])

# Compare triangulation using union vs combine to obtain a single multipolygon
unioned_df <-  
  my_sf %>%
  st_union()

nc_combined_df <-
  nc %>%
  st_geometry() %>%
  st_combine()

# Internal boundaries are resolved.
plot(sf_to_trimesh(unioned_df, n_tris = 1000))

# Internal boundaries remain and appear in triangulation structure
plot(sf_to_trimesh(nc_combined_df, n_tris = 1000))
