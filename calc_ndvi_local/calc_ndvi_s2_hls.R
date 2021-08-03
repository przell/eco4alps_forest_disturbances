# calc s2 ndvi t32tps hls
# harmonized landsat sentinel v1.4

# libs -------------------------------------------------------------------------
library(stars)
library(sf)
library(mapview)
library(dplyr)
library(stringr)
library(parallel)

# testing area -----------------------------------------------------------------
# couple of windthrow areas welschenofen
pth_area = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/area_32632.shp"
area = st_read(pth_area)
mapview(st_bbox(area))
area_bbox = st_bbox(area)

# area_bbox = st_bbox(c(xmin = 679210, 
#                       xmax = 700270, 
#                       ymax = 5151760, 
#                       ymin = 5126080), 
#                     crs = st_crs(32632))
# mapview(area_bbox)

# s2 list ----------------------------------------------------------------------
# make file list
fls_s2 = list.files("/mnt/CEPH_PROJECTS/AI4EBV/EO/HLS/32TPS", 
                    pattern = "HLS.S30.*hdf$", 
                    recursive = T, 
                    full.names = TRUE)
list_s2 = tibble(pth = fls_s2, 
                 base_name = basename(fls_s2), 
                 year_doy = gsub(".*T32TPS\\.(20[0-2][0-9][0-3][0-9][0-9])\\.v1\\.4.*", "\\1", fls_s2))
list_s2 = list_s2 %>% 
  mutate(year = substr(year_doy, 1, 4),
         doy = substr(year_doy, 5, 7), 
         date = as.Date(as.numeric(doy)-1, origin = paste0(year, "-01-01")))

list_s2 = list_s2 %>% 
  mutate(date = as.character(format(date, "%Y%m%d")))


list_s2 = list_s2 %>% arrange(date)

sum(duplicated(list_s2$year_doy)) # this shouldn't be at all i guess
nrow(list_s2)
range(list_s2$date)

# calc ndvi ---------------------------------------------------------------------------
# defs
calc_ndvi = function(x) (x[2] - x[1])/(x[2] + x[1])
gdal_translate = function(file_in, file_out){
  message(Sys.time(), " | translating: ", file_out)
  gdal_cmd = paste0("gdal_translate ",
                    "-projwin 679210 5151760 700270 5126080 ", # here the bbox is set!!!
                    file_in, " ", 
                    file_out)
  message(gdal_cmd)
  system(gdal_cmd)
  return(file_out)
}

path_out = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi"
aoi = area_bbox
# if the area gets too large i get this error upon st_write:

# initiate cluster
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
clusterExport(cl = cl, 
              varlist = c("calc_ndvi", "aoi", "gdal_translate", "path_out"), 
              envir = environment())
clusterEvalQ(cl = cl, expr = {
  library(stars)
  library(sf)
  library(dplyr)
})

# apply in parallel
a = Sys.time()
fls_ndvi = parLapply(cl = cl, X = purrr::transpose(list_s2), fun = function(x){
  
  # define temporary output bands
  tmp_out_red = paste0(path_out, "/", "tmp_red_", x$date, ".tif")
  tmp_out_nir = paste0(path_out, "/", "tmp_nir_", x$date, ".tif")
  #tmp_out_qa = paste0(path_out, "/", "tmp_QA_", x$date, ".tif")
  tmp_out_qa = paste0(path_out, "/", x$date, "_qa_", 
                      tools::file_path_sans_ext(x$base_name), ".tif")
  
  # pull bands from hdf, cut and make to tif
  gdal_translate(file_in = paste0("HDF4_EOS:EOS_GRID:'", x$pth, "':Grid:B04"), 
                 file_out = tmp_out_red)
  gdal_translate(file_in = paste0("HDF4_EOS:EOS_GRID:'", x$pth, "':Grid:B8A"), # B8A is narrow nir also avlbl in ls8 
                 file_out = tmp_out_nir)
  gdal_translate(file_in = paste0("HDF4_EOS:EOS_GRID:'", x$pth, "':Grid:QA"), 
                 file_out = tmp_out_qa)
  
  # read red and nir and calc ndvi as proxy
  ndvi = read_stars(c(tmp_out_red, tmp_out_nir), along = "bands", proxy = TRUE)
  ndvi = st_apply(X = ndvi, MARGIN = c("x", "y"), FUN = calc_ndvi)
  
  # write the ndvi
  name_out = paste0(path_out, "/", x$date, "_ndvi_", 
                    tools::file_path_sans_ext(x$base_name), ".tif")
  stars::write_stars(obj = ndvi, 
                     dsn = name_out, 
                     driver = "GTiff")
   
  # ndvi_msk = read_stars(c(name_out, tmp_out_qa), along = "bands", proxy = TRUE)
  # stars::write_stars(obj = ndvi_msk, 
  #                    dsn = name_out, 
  #                    driver = "GTiff")
  # 
  
  return(name_out)
})

stopCluster(cl)
b = Sys.time() - a
b

nrow(list_s2) == length(fls_ndvi)

# todo in analyis skript :
# set ndvi range to 0-1 upon read rest NA
#  decode bitmask (frisi)
#  rename mask when decoded


# set range of ndvi to 0-1 -----------------------------------------------------
path_ndvi = c("/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi")
fls_ndvi = list.files(path_ndvi, pattern = "_ndvi_", full.names = TRUE)

n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
clusterEvalQ(cl = cl, expr = {
  library(stars)
  library(sf)
  library(dplyr)
})

# apply in parallel
a = Sys.time()
fls_ndvi01 = parLapply(cl = cl, X = fls_ndvi, fun = function(x){
  
  ndvi = read_stars(x)
  ndvi[[1]][(ndvi[[1]] > 1)] = NA
  ndvi[[1]][(ndvi[[1]] < 0)] = NA
  
  stars::write_stars(obj = ndvi, 
                     dsn = x, 
                     driver = "GTiff")
  return(x)
})

stopCluster(cl)
b = Sys.time() - a
b

length(fls_ndvi) == length(fls_ndvi01)

# create binary mask from bitmask ----------------------------------------------
# here we use the most strict masking (but not aerosols) to make a binary mask
# https://hls.gsfc.nasa.gov/wp-content/uploads/2019/01/HLS.v1.4.UserGuide_draft_ver3.1.pdf
path_msk = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi"
fls_msk = list.files(path_msk, pattern = "_qa_", full.names = T)

n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
clusterEvalQ(cl = cl, expr = {
  library(stars)
  library(sf)
  library(dplyr)
})

# apply in parallel
a = Sys.time()
fls_msk01 = parLapply(cl = cl, X = fls_msk, fun = function(x){
  
  msk = read_stars(x)
  msk = st_apply(X = msk, MARGIN = c("x", "y"), FUN = function(x){
    ifelse(sum(as.integer(intToBits(x))[1:5]) == 0, 1, NA)
  })
  stars::write_stars(obj = msk, 
                     dsn = x, 
                     driver = "GTiff")
  return(x)
})

stopCluster(cl)
b = Sys.time() - a
b

length(fls_msk) == length(fls_msk01)

