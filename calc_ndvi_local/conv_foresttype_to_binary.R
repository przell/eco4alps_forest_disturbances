# forest type to binary

# libs -------------------------------------------------------------------------
library(stars)
library(sf)
library(dplyr)

# dummy ndvi for extent --------------------------------------------------------
path_ndvi = c("/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi")
fls_ndvi = list.files(path_ndvi, pattern = "_ndvi_", full.names = TRUE)
ndvi = read_stars(fls_ndvi[[1]])
ext_ndvi = st_as_sfc(st_bbox(ndvi))

# forest mask ------------------------------------------------------------------
pth_forest = "/mnt/CEPH_PROJECTS/ECO4Alps/Land_Cover/Land_cover_data/HR_Layer/FTY_2018_010m_32632.tif"
forest = stars::read_stars(pth_forest, proxy = TRUE)
# st_crs(forest)
# st_crs(ndvi)
# st_crs(ext_ndvi)

# read only bbox
forest = forest[st_bbox(st_transform(ext_ndvi, st_crs(forest)))]
#mapview(st_bbox(forest))

# read to r
forest = st_as_stars(forest)
forest

# convert to binary
# FTY_2018_010m_32632.tif 
# 0      :41308           
# 1      :30280           
# 2      :28412    
# -> set 0 to NA, and everything else to 1, since 1 is already there only 2 to 1
forest[[1]][forest[[1]] == 0] = NA
forest[[1]][forest[[1]] == 2] = 1

# convert from factor to numeric
forest$FTY_2018_010m_32632.tif = as.numeric(as.character(forest$FTY_2018_010m_32632.tif))


# resample to ndvi by nearest neighbour
forest = stars::st_warp(src = forest, dest = ndvi, method = "near")

# write to disk
plot(forest)
pth_out = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/FTY_2018_030m_binary_ndvi_mask.tif"
write_stars(forest, pth_out)

plot(forest)
plot(ndvi)
plot(forest * ndvi)



