# hansen forest change layer

# TO DO:
# write a tif for every year as binary mask

# libs -------------------------------------------------------------------------
library(sf)
library(stars)
library(dplyr)
library(mapview)

# aoi --------------------------------------------------------------------------
pth_aoi = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast/brks_2016_2020_start_2018_level_0.001.tif"
aoi = read_stars(pth_aoi)
plot(aoi)
aoi = st_warp(src = aoi, crs = st_crs(4326))
plot(aoi)
aoi[[1]][aoi[[1]] == 0] = NA
aoi[[1]][aoi[[1]] < 2018.5] = 0
aoi[[1]][aoi[[1]] > 2019.5] = 1
aoi_rd = floor(aoi)
aoi_rd[[1]] = as.factor(aoi_rd[[1]])
plot(aoi_rd)

# hansen data ------------------------------------------------------------------
pth_fc = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/Hansen_GFC-2020-v1.8_lossyear_50N_010E.tif"
fc = read_stars(pth_fc, proxy = TRUE)
fc = fc[aoi]
fc = st_as_stars(fc)
fc[[1]][fc[[1]] == 0] = NA
fc[[1]] = as.factor(fc[[1]])
plot(fc)
fc
system(paste0("gdalinfo ", pth_fc))

# looky loocky -----------------------------------------------------------------
mapview(fc) + mapview(aoi_rd)
# what to do about the false poitives?



