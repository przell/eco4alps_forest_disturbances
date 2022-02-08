# run bfast on whole scene

# libs -------------------------------------------------------------------------
library(stars)
library(sf)
library(mapview)
library(dplyr)
library(stringr)
library(parallel)
library(bfast)
library(pbapply)
library(lubridate)
library(xts)
library(zoo)
library(forecast)
library(ggplot2)

# path to results --------------------------------------------------------------
pth_out = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast/"

# s2/ls8 ndvi hls --------------------------------------------------------------
# it is masked to the copernicus high resolution forest forest type 2018
# https://land.copernicus.eu/pan-european/high-resolution-layers/forests
# "/mnt/CEPH_PROJECTS/ECO4Alps/Land_Cover/Land_cover_data/HR_Layer/FTY_2018_010m_32632.tif"

# file list ndvi fmask
path_ndvi = c("/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/04_hls_combined_v014_ndvi_fmask_forest")
fls_ndvi = list.files(path_ndvi, pattern = "_ndvi_", full.names = TRUE)
fls_ndvi = tibble(pth = fls_ndvi,
                  sensor = substr(basename(fls_ndvi), 19, 21),
                  date = as.Date(substr(basename(fls_ndvi), 1, 8), format = "%Y%m%d"))

fls_ndvi = fls_ndvi %>% dplyr::filter(lubridate::year(date) >= 2016) %>% arrange(date)


#
# testing --------------------------------------------------------------------
# bbox = st_bbox(read_stars(fls_ndvi$pth[[1]]))
# aoi = sf::st_make_grid(x = bbox, n = c(6,8))
# mapview(aoi) + mapview(bbox)
# aoi = aoi[12]
# 
# ndvi_prox = read_stars(fls_ndvi$pth, along = "t", proxy = TRUE)
# ndvi_prox = stars::st_set_dimensions(.x = ndvi_prox, which = "t",
#                                      values = fls_ndvi$date)
# ndvi = st_as_stars(ndvi_prox[aoi])
# plot(ndvi %>% slice("t", 2))

# one px bfast for testing
# ndvi_px = ndvi[st_centroid(st_as_sfc(st_bbox(ndvi)))]
# ndvi_px_vals = ndvi_px %>% slice("t", 1:509) %>% pull() %>% c()
# lsts_px = bfastts(ndvi_px_vals, ndvi_dates[1:509], type = c("irregular"))
# 
# res_px = bfastmonitor(data = lsts_px,
#                       start = 2018, 
#                       formula=response~harmon, 
#                       order = 1, 
#                       history = "all", level = c(0.005, 0.005), 
#                       verbose = F)
# res_px = bfastmonitor(data = lsts_px,
#                       start = 2018,  
#                       formula=response~season, 
#                       sbins = 5, 
#                       history = "all", level = c(0.005, 0.005), 
#                       verbose = F)
# 
# res_px$breakpoint
# res_px$magnitude
# sum(!is.na(res_px$data)) 
# plot(res_px)
# testing (done) ----

# spatial bfast monitor function -----------------------------------------------
spatial_bfm = function(pixels, dates, start_monitor = 2018, level = c(0.05, 0.05), 
                       val = "breakpoint") {
  # error handling
  #stopifnot(length(pixels) == length(dates)) 
  #stopifnot(val %in% c("breakpoint", "magnitude"))
  
  # create ts object for bfast
  lsts = bfastts(pixels, dates, type = c("irregular"))
  
  # make sure there are enough observations
  if (sum(!is.na(lsts)) < 100){
    return(NA)
  }
  
  # run bfast and return the selected value into the raster
  res = bfastmonitor(lsts, 
               start_monitor, 
               formula = response~harmon, 
               order = 1, 
               history = "all", 
               level = level,
               verbose = F)[[val]]
  if(is.na(res)){
    return(0)
  }
  
  return(res)
  
}


# brks = st_apply(ndvi, c("x", "y"), PROGRESS = TRUE, function(x){
#   spatial_bfm(pixels = x, dates = fls_ndvi$date, start_monitor = 2018, val = "breakpoint")
# })
# valid_px = st_apply(ndvi, c("x", "y"), function(x){sum(!is.na(x))}, PROGRESS=TRUE)
# cnt_px = length(brks[[1]])
# cnt_forest = sum(!is.na(brks[[1]]))
# cnt_noforest = sum(is.na(brks[[1]]))
# cnt_forest + cnt_noforest == cnt_px
# cnt_brk = sum(brks[[1]] > 0, na.rm = T)
# cnt_no_brk = sum(brks[[1]] == 0, na.rm = T)
# cnt_brk + cnt_no_brk == cnt_forest

# load ndvi --------------------------------------------------------------------
ndvi_prox = read_stars(fls_ndvi$pth, along = "t", proxy = T)
ndvi_prox = stars::st_set_dimensions(.x = ndvi_prox, which = "t", 
                                     values = fls_ndvi$date)
st_crs(ndvi_prox)
st_bbox(ndvi_prox)
range(fls_ndvi$date)

# run bfast --------------------------------------------------------------------
# params
level = c(0.001, 0.001) # 0.001 // Significance levels of the monitoring and ROC (if selected) procedure, i.e., probability of type I error.
start_monitor = 2020

# setup clusters
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)

clusterExport(cl = cl, 
              varlist = c("spatial_bfm", "fls_ndvi", "pth_out", "start_monitor", "level"), 
              envir = environment())

clusterEvalQ(cl = cl, expr = {
  library(stars)
  library(sf)
  library(dplyr)
  library(bfast)
  library(pbapply)
})

a = Sys.time()
out_name = paste0(pth_out, "brks_2016_2020_start_", start_monitor, "_level_", level[1], ".tif")
brks = st_apply(X = ndvi_prox, MARGIN = c("x", "y"), CLUSTER = cl, function(x){
  spatial_bfm(pixels = x, dates = fls_ndvi$date, start_monitor = start_monitor, 
              level = level, val = "breakpoint")
})
write_stars(brks, out_name)
Sys.time() - a 

out_name = paste0(pth_out, "magn_2016_2020_start_", start_monitor, "_level_", level[1], ".tif")
magn = st_apply(X = ndvi_prox, MARGIN = c("x", "y"), CLUSTER = cl, function(x){
  spatial_bfm(pixels = x, dates = fls_ndvi$date, start_monitor = start_monitor, 
              level = level, val = "magnitude")
})
write_stars(magn, out_name)
Sys.time() - a

out_name = paste0(pth_out, "val_obs_2016_2020_start_", start_monitor, "_level_", level[1], ".tif")
val_obs = st_apply(ndvi_prox, c("x","y"), function(x){sum(!is.na(x))})
write_stars(val_obs, out_name)
Sys.time() - a

stopCluster(cl)



# END



# old testing: bfast on patches, ndvi ts and plots etc ---- 
# # ndvi msk has the correct values
# ndvi_hls_valpx = st_apply(ndvi_hls, c("x","y"), function(x){
#   sum(!is.na(x))})
# plot(ndvi_hls_valpx)
# # look what happens with them from here!
# 
# # get mean ndvi for windthrow poly in 2017-2019
# ndvi_ts = st_apply(ndvi_hls, c("t"), median, na.rm = TRUE) # 
# ndvi_ts = tibble(ndvi = ndvi_ts %>% pull() %>% c(), 
#                  date = st_get_dimension_values(ndvi_hls, "t"))
# 
# ggplot(ndvi_ts %>% filter(lubridate::year(date) == 2018), aes(x=date, y=ndvi)) +
#   geom_line() +
#   geom_point()
# 
# ggplot(ndvi_ts, aes(x=date, y=ndvi)) +
#   geom_line() +
#   geom_point()  + 
#   geom_vline(xintercept = as.Date("2018-10-28"), col = "red")
# 
# 
# 
# # bfast on one pixel
# ndvi_dates = st_get_dimension_values(ndvi_hls, "t")
# diff.Date(ndvi_dates)
# pixels = ndvi_hls[st_point_on_surface(aoi)] %>% pull() %>% c()
# summary(pixels)
# 
# lsts <- bfastts(pixels, ndvi_dates, type = c("irregular"))
# plot(lsts)
# 
# 
# # lsts_lin = round(na.approx(lsts), 4)
# # lsts_per = round(na.interp(lsts), 4) # bfast monitor not needed!!!!!
# # plot(lsts)
# # plot(lsts_lin)
# # plot(lsts_per)
# # plot(lsts_week)
# 
# 
# bfm_res = bfastmonitor(lsts, 2018, 
#                        formula = response~harmon,# response~season #response~trend, # response~trend+harmon, response~harmon
#                        order = 1, # decide 1 or 2 
#                        history = "all", #date could be specified e.g one year, all = history without breaks, don't have disturbance in history
#                        verbose = T)
# # harmon order = 3, reducing will lead to less seasonality = waves per year -> for forrest maybe 2 or 1
# # season sbins = 3, num of seasonal dummies -> 4 
# 
# plot(bfm_res)
# 
# 
# # IMPROVE ERROR HANDLING, CHECK LENGHT OF NON NA AND WRITE TO FILE
# 
# SpatialBFM = function(pixels)
# {
#   #lsts = ts(pixels, c(2016, 1), frequency=30.666667)
#   lsts = bfastts(pixels, ndvi_dates, type = c("irregular")) # check how this looks then
#   if (sum(!is.na(lsts)) < 100){
#     return(NA)
#   }
#   bfastmonitor(lsts, 2018, formula=response~harmon, order = 1, history = "all", 
#                verbose = F)$breakpoint
# }
# 
# 
# plot(ndvi_hls %>% slice("t", 207))
# valid_obs = st_apply(ndvi_hls, c("x", "y"), function(x){sum(!is.na(x))}, PROGRESS=TRUE)
# plot(valid_obs)
# StarsResult = st_apply(ndvi_hls, c("x", "y"), SpatialBFM, PROGRESS = TRUE)
# StarsResult
# plot(StarsResult)
# StarsResult %>% pull() %>% c() %>% hist()
# 
# 
# library(mapview)
# library(leaflet)
# m <- mapview(aoi)
# m@map = m@map %>% 
#   addWMSTiles(group = 'EuracMosaic',
#               #"https://watersgeo.epa.gov/arcgis/services/NHDPlus_NP21/NHDSnapshot_NP21/MapServer/WmsServer?",
#               "http://saocompute.eurac.edu/geoserver/ows?SERVICE=WMS",
#               layers  = 'SENTINEL2:S2_MOSAIC_2019_ST_stretch_mask_3857',
#               options = WMSTileOptions(format = "image/png", transparent = TRUE),
#               attribution = "") %>% mapview:::mapViewLayersControl(names = c("EuracMosaic"))
# m

# old testing (end) ------------------------------------------------------------










