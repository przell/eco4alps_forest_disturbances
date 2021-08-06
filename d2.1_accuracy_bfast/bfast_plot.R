# plot bfast output

# libs ------------------------------------------------------------------------
library(stars)
library(sf)
library(mapview)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)

# data ------------------------------------------------------------------------
pth_brks = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast/brks_2016_2020_start_2018_level_0.001.tif"
brks = read_stars(pth_brks)
brks[[1]][brks[[1]] == 0] = NA
pth_magn =  "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast/magn_2016_2020_start_2018_level_0.001.tif"
magn = read_stars(pth_magn)
pth_val_obs =  "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast/val_obs_2016_2020_start_2018_level_0.001.tif"
val_obs = read_stars(pth_val_obs)
#res = read_stars(c(pth_brks, pth_magn, pth_valid_obs))

pth_wind = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/area_32632.shp"
wind = st_read(pth_wind)
wind = st_transform(wind, crs = st_crs(brks))

aoi = wind[127, ]
aoi_bbox = st_as_sfc(st_bbox(aoi))

brks = brks[aoi_bbox]
magn = magn[aoi_bbox]
val_obs = val_obs[aoi_bbox]





m <- mapview(aoi, color = "black", lwd = 2, col.regions = "transparent", alpha.regions = 0.1)
m@map = m@map %>%
  addWMSTiles(group = 'EuracMosaic',
              "http://saocompute.eurac.edu/geoserver/ows?SERVICE=WMS",
              layers  = 'SENTINEL2:S2_MOSAIC_2019_ST_stretch_mask_3857',
              options = WMSTileOptions(format = "image/png", transparent = TRUE), 
              attribution = "") %>% 
  mapview:::mapViewLayersControl(names = c("EuracMosaic"))
m # + mapview(brks): error, adding raster before puts it in the background

m = mapview(aoi, color = "black", lwd = 2, col.regions = "transparent", alpha.regions = 0.1)
m + mapview(brks)
m + mapview(magn)
m + mapview(val_obs)

#mapview(brks) + mapview(aoi, color = "black", lwd = 2, col.regions = "transparent", alpha.regions = 0.1)
