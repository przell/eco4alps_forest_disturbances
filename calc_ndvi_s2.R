# calc s2 ndvi t32tps

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


# fmask list -------------------------------------------------------------------
pth_fmask = "/mnt/CEPH_PRODUCTS/FMASK/SENTINEL2/T32TPS"
fls_fmask = list.files(pth_fmask, pattern = "_Fmask4.tif", recursive = T, full.names = TRUE)

list_fmask = tibble(pth_fmask = fls_fmask, 
                    pth_base = paste0(sub("SAFE.*", "", fls_fmask), "SAFE"))

list_fmask = list_fmask %>% 
  mutate(safe_name = basename(pth_base), 
         sensor = substr(x = basename(pth_base), start = 1, stop = 3), 
         date = substr(x = basename(pth_base), start = 12, stop = 19))

list_fmask = list_fmask %>% arrange(date)

sum(duplicated(list_fmask$safe_name))
sum(duplicated(list_fmask$date))

# s2 list ----------------------------------------------------------------------
# make file list
fls_s2 = list.files("/mnt/CEPH_PRODUCTS/EURAC_L2A/SENTINEL2/alps/T32TPS", 
                    pattern = "B04_10m.jp2", 
                    recursive = T, 
                    full.names = TRUE)
list_s2 = tibble(pth_base = sub("GRANULE.*", "", fls_s2)  , 
                 pth_b04 = fls_s2, 
                 pth_b08 = stringr::str_replace(string = fls_s2, pattern = "_B04_10m", replacement = "_B08_10m"))
list_s2 = list_s2 %>% 
  mutate(safe_name = basename(pth_base),
         sensor = substr(x = basename(pth_base), start = 1, stop = 3), 
         date = substr(x = basename(pth_base), start = 12, stop = 19))

list_s2 = list_s2 %>% arrange(date)

sum(duplicated(list_s2$safe_name)) # this shouldn't be at all i guess

list_s2 = list_s2 %>% filter(!duplicated(safe_name))
sum(duplicated(list_s2$date))

# join dates fmask and s2 ----------------------------------------------------

list_s2 = list_s2 %>% mutate(join_name = substr(safe_name, 12, nchar(safe_name)))
list_fmask = list_fmask %>% mutate(join_name = substr(safe_name, 12, nchar(safe_name)))

# keep only files where fmask and s2 is available
list_s2_fmask = inner_join(list_s2, list_fmask, by = c("join_name", "date", "sensor"), 
                           suffix = c("_s2", "_fmask"))

nrow(list_fmask)
nrow(list_s2)
nrow(list_s2_fmask)

sum(duplicated(list_s2_fmask$date))
list_s2_fmask = list_s2_fmask %>% filter(!duplicated(date))

range(list_s2$date)
range(list_fmask$date)
range(list_s2_fmask$date)

# calc ndvi ---------------------------------------------------------------------------
# defs
path_out = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/02_s2_ndvi_local_eurac"
calc_ndvi = function(x) (x[2] - x[1])/(x[2] + x[1])
aoi = area_bbox
# if the are gets too large i get this error upon st_write:

# aoi = st_buffer(st_centroid(st_as_sfc(st_bbox(s2))), dist = 40000, endCapStyle = "SQUARE")
# mapview(aoi) + mapview(st_bbox(s2))

# initiate cluster
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
clusterExport(cl = cl, 
              varlist = c("calc_ndvi", "aoi", "path_out"), 
              envir = environment())
clusterEvalQ(cl = cl, expr = {
  library(stars)
  library(sf)
  library(dplyr)
})

# apply in parallel
a = Sys.time()
fls_ndvi = parLapply(cl = cl, X = purrr::transpose(list_s2_fmask), fun = function(x){
  name_out = paste0(path_out, "/", x$date, "_ndvi_" , x$safe_name_s2, ".tif")
  message(Sys.time(), " | processing ndvi | ", basename(name_out))
  
  # calc ndvi
  s2 = read_stars(c(x$pth_b04, x$pth_b08), along = "bands", proxy = TRUE)
  s2 = s2[aoi]
  s2 = st_apply(X = s2, MARGIN = c("x", "y"), FUN = calc_ndvi)
  
  stars::write_stars(obj = s2, 
                     dsn = name_out, 
                     driver = "GTiff")
  return(name_out)
  
})

# run fmask addition
# also do vinschgau

stopCluster(cl)
b = Sys.time() - a
b

# create ndvi fmask list -------------------------------------------------------
pth_ndvi = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/02_s2_ndvi_local_eurac/"
fls_ndvi = list.files(pth_ndvi, pattern = "SAFE.tif", full.names = TRUE)

length(fls_ndvi) == nrow(list_s2_fmask)

list_ndvi = tibble(pth_ndvi = fls_ndvi, 
                   #date = substr(basename(fls_ndvi), 1, 8), 
                   join_name = substr(basename(fls_ndvi), 26, nchar(basename(fls_ndvi))-4))


setdiff(list_ndvi$join_name, list_s2_fmask$join_name)
setdiff(list_s2_fmask$join_name, list_ndvi$join_name)


list_ndvi_fmask = inner_join(list_ndvi, list_s2_fmask[c("join_name", "pth_fmask", "date")], by = "join_name")
nrow(list_ndvi_fmask) == nrow(list_ndvi)

# merge ndvi and fmask layers --------------------------------------------------

gdal_merge = function(file_1, file_2, file_out){
  message(Sys.time(), " | merging: ", file_out)
  gdal_cmd = paste0("gdal_merge.py ", 
                    "-seperate ",
                    "-tap ",
                    #"-ul_lr 679210 5151760 700270 5126080 ", # not working :(
                    "-o ", file_out, " ", 
                    file_1, " ", 
                    file_2)
  system(gdal_cmd)
  return(file_out)
}

gdal_warp = function(file_in, file_out){
  message(Sys.time(), " | cutting to extent: ", file_out)
  gdal_cmd = paste0("gdalwarp ", 
                    "-te 679210 5126080 700270 5151760 ",
                    "-overwrite ", 
                    file_in, " ", 
                    file_out)
  system(gdal_cmd)
  return(file_out)
}

out_pth = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/02_s2_ndvi_local_eurac/ndvi_novalevante_fmask/"

list_ndvi_merged = lapply(X = purrr::transpose(list_ndvi_fmask), FUN = function(x){
  file_out = paste0(out_pth, basename(x$pth_ndvi))
  tmp_file = paste0(out_pth, "tmp_", basename(x$pth_ndvi))
  
  
  merge_file = gdal_merge(file_1 = x$pth_ndvi, 
                          file_2 = x$pth_fmask, 
                          file_out = tmp_file)
  
  warp_file = gdal_warp(file_in = merge_file, file_out = file_out)
  
  message(Sys.time(), " | removing: ", merge_file)
  file.remove(merge_file)
  
  return(warp_file)
  
})

ndvi_fmask = stars::read_stars(list_ndvi_merged[[400]])
plot(ndvi_fmask)





# to do:
# mask with fmask! -> align dates -> add as binary layer!
# GET BFAST RUNNING
# mask with forrest? -> only when loading
# write whole tile?







