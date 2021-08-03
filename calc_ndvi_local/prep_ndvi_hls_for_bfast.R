# prep ndvi hls for bfast
# use s2, l8
# mask with fmask binary mask
# mask with forest type binary mask


# libs
library(stars)
library(sf)
library(dplyr)
library(parallel)

# file list ndvi ---------------------------------------------------------------
path_ndvi = c("/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi", 
              "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_l30_v014_ndvi")
fls_ndvi = list.files(path_ndvi, pattern = "_ndvi_", full.names = TRUE)
fls_ndvi = tibble(pth = fls_ndvi,
                      sensor = substr(basename(fls_ndvi), 19, 21),
                      date = as.Date(substr(basename(fls_ndvi), 1, 8), format = "%Y%m%d")) %>% 
  arrange(date)

# file list fmask binary -------------------------------------------------------
path_mask = path_ndvi
fls_mask = list.files(path_mask, pattern = "_qa_", full.names = TRUE)
fls_mask = tibble(pth = fls_mask, 
                  sensor = substr(basename(fls_mask), 17, 19), 
                  date = as.Date(substr(basename(fls_mask), 1, 8), format = "%Y%m%d")) %>% 
  arrange(date)


setdiff(fls_mask$date, fls_ndvi$date)
setdiff(fls_ndvi$date, fls_mask$date)
identical(fls_mask$date, fls_ndvi$date)


# combine lists
fls_comb = inner_join(x = fls_ndvi, y = fls_mask, 
                      by = c("date", "sensor"), suffix = c("_ndvi", "_mask")) %>% 
  arrange(date)
nrow(fls_comb) == nrow(fls_ndvi)
nrow(fls_comb) == nrow(fls_mask)


# forest binary (unique file) --------------------------------------------------
path_forest = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/FTY_2018_030m_binary_ndvi_mask.tif"

# pth out ----------------------------------------------------------------------
path_out = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/04_hls_combined_v014_ndvi_fmask_forest/"

# load all files time step by timestep and mask --------------------------------
# initiate cluster
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
clusterExport(cl = cl, 
              varlist = c("path_out", "path_forest"), 
              envir = environment())
clusterEvalQ(cl = cl, expr = {
  library(stars)
  library(sf)
  library(dplyr)
})

a = Sys.time()
fls_ndvi = parLapply(cl = cl, X = purrr::transpose(fls_comb), fun = function(x){
  
  # read red and nir and calc ndvi as proxy
  ndvi = read_stars(c(x$pth_ndvi, x$pth_mask, path_forest), along = "bands", proxy = TRUE)
  ndvi = st_apply(X = ndvi, MARGIN = c("x", "y"), FUN = function(x){
    x[1]*x[2]*x[3]
  })
  
  # write the ndvi
  name_out = paste0(path_out, "/", basename(x$pth_ndvi))
  stars::write_stars(obj = ndvi, 
                     dsn = name_out, 
                     driver = "GTiff")
  return(name_out)
})

stopCluster(cl)
b = Sys.time() - a
b




