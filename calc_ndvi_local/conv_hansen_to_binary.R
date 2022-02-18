# hansen forest change layer

# TO DO:
# - subset to aoi
# - warp to ndvi
# - remove changes before 2018 --> hr fty map copernicus is 2018
# - keep yearly changes from 2018

# libs -------------------------------------------------------------------------
library(sf)
library(stars)
library(dplyr)
library(mapview)

# dummy ndvi for extent --------------------------------------------------------
path_ndvi = c("/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi")
fls_ndvi = list.files(path_ndvi, pattern = "_ndvi_", full.names = TRUE)
ndvi = read_stars(fls_ndvi[[1]])
ext_ndvi = st_as_sfc(st_bbox(ndvi))

# forest mask ------------------------------------------------------------------
pth_fc = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/Hansen_GFC-2020-v1.8_lossyear_50N_010E.tif"
fc = read_stars(pth_fc, proxy = TRUE)
# st_crs(fc)
# st_crs(ndvi)
# st_crs(ext_ndvi)

# read only bbox
fc = fc[st_bbox(st_transform(ext_ndvi, st_crs(fc)))]
# mapview(st_bbox(fc))

# read to r
fc = st_as_stars(fc)

# convert values 
# 0 -> 1 (no forest change): not relevant for mask
# 1-17 -> 1 (changes before 2018) -> copernicus fty hr layer for forest mask is 2018
# 18-20 -> 2018 - 2020 (changes that will be used to mask pixels according to start of)
fc[[1]][fc[[1]] == 0] = NA # reset to 1 afterwards
fc[[1]][fc[[1]] < 18] = NA # resete to 1 afterwards
fc[[1]] = as.integer(fc[[1]] + 2000)
fc[[1]][is.na(fc[[1]])] = 1

# resample to ndvi by nearest neighbour
fc = stars::st_warp(src = fc, dest = ndvi, method = "near")

# write to disk
pth_out = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/Hansen_GFC-2020-v1.8_lossyear_50N_010E_ndvi_mask.tif"
write_stars(fc, pth_out)

# plot(fc)
# plot(ndvi)





# old testing ================================================================== 


# # aoi --------------------------------------------------------------------------
# pth_aoi = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast/brks_2016_2020_start_2018_level_0.001.tif"
# aoi = read_stars(pth_aoi)
# plot(aoi)
# aoi = st_warp(src = aoi, crs = st_crs(4326))
# plot(aoi)
# aoi[[1]][aoi[[1]] == 0] = NA
# aoi[[1]][aoi[[1]] < 2018.5] = 0
# aoi[[1]][aoi[[1]] > 2019.5] = 1
# aoi_rd = floor(aoi)
# aoi_rd[[1]] = as.factor(aoi_rd[[1]])
# plot(aoi_rd)
# 
# # hansen data ------------------------------------------------------------------
# pth_fc = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/Hansen_GFC-2020-v1.8_lossyear_50N_010E.tif"
# fc = read_stars(pth_fc, proxy = TRUE)
# fc = fc[aoi]
# fc = st_as_stars(fc)
# fc[[1]][fc[[1]] == 0] = NA
# fc[[1]] = as.factor(fc[[1]])
# plot(fc)
# fc
# system(paste0("gdalinfo ", pth_fc))
# 
# # write binary hansen layer...
# # not needed... using hansen as is on the fly in the bfast script.
# 
# # looky loocky -----------------------------------------------------------------
# mapview(fc) + mapview(aoi_rd)
# # what to do about the false poitives?



