# run bfast on whole scene

# TO DO THIS RECALC NDVI WITH ALL MASKINGS APPLIED!!!
# FORREST AND FMASK

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

# s2/ls8 ndvi hls ----------------------------------------------------------------
# file list ndvi fmask
path_ndvi_hls = c("/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi", 
                  "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_l30_v014_ndvi")
fls_ndvi_hls = list.files(path_ndvi_hls, pattern = "_ndvi_", full.names = TRUE)
fls_ndvi_hls = tibble(pth = fls_ndvi_hls,
                      sensor = substr(basename(fls_ndvi_hls), 19, 21),
                      date = as.Date(substr(basename(fls_ndvi_hls), 1, 8), format = "%Y%m%d"))

fls_ndvi_hls_in = fls_ndvi_hls %>% dplyr::filter(lubridate::year(date) >= 2016) %>% arrange(date)


path_mask_hls = path_ndvi_hls
fls_mask_hls = list.files(path_mask_hls, pattern = "_qa_", full.names = TRUE)
fls_mask_hls = tibble(pth = fls_mask_hls, 
                      sensor = substr(basename(fls_mask_hls), 17, 19), 
                      date = as.Date(substr(basename(fls_mask_hls), 1, 8), format = "%Y%m%d"))

fls_mask_hls_in = fls_mask_hls %>% dplyr::filter(lubridate::year(date) >= 2016) %>% arrange(date)

setdiff(fls_mask_hls_in$date, fls_ndvi_hls_in$date)
setdiff(fls_ndvi_hls_in$date, fls_mask_hls_in$date)
identical(fls_mask_hls_in$date, fls_ndvi_hls_in$date)

read_ndvi_hls = function(list_pth = fls_ndvi_hls_in$pth,
                         list_mask = fls_mask_hls_in$pth,
                         list_date = fls_ndvi_hls_in$date, 
                         aoi){
  # read as proxy
  ndvi_prox = read_stars(list_pth, proxy = TRUE, along = "t")
  ndvi_prox = stars::st_set_dimensions(.x = ndvi_prox, which = "t", 
                                       values = list_date)
  
  # loop through patches here
  if(!is.null(aoi)) {
    aoi = st_transform(aoi, st_crs(ndvi_prox))
    ndvi_prox = ndvi_prox[aoi]
  }
  
  # read to r
  ndvi = st_as_stars(ndvi_prox)
  ndvi[[1]][(ndvi[[1]] > 1)] = NA
  ndvi[[1]][(ndvi[[1]] < 0)] = NA
  
  # create mask
  fmask_prox = read_stars(list_mask, along = "t", proxy = TRUE)
  if(!is.null(aoi)){
    fmask_prox = fmask_prox[aoi]
  }
  
  fmask = st_as_stars(fmask_prox)
  fmask = st_apply(X = fmask, MARGIN = c("x", "y", "t"), FUN = function(x){
    ifelse(sum(as.integer(intToBits(x))[1:5]) == 0, 1, NA)
    # WHAT TO DO ON BIT 6-7 Aerosols? Is climatology good or bad
  })
  
  # apply mask to ndvi
  ndvi_msk = ndvi * fmask
  
  return(ndvi_msk)
} 


# read ndvi --------------------------------------------------------------------
bbox = st_bbox(read_stars(fls_ndvi_hls_in$pth[[1]]))
aoi = sf::st_make_grid(x = bbox, n = c(6,8))
mapview(aoi) + mapview(bbox)
aoi = aoi[12]

# aoi = NULL

ndvi_hls = read_ndvi_hls(list_pth = fls_ndvi_hls_in$pth, 
                         list_mask = fls_mask_hls_in$pth, 
                         list_date = fls_ndvi_hls_in$date, 
                         aoi = aoi)

ndvi_hls[[1]][ndvi_hls[[1]] <= 0] = NA
ndvi_hls[[1]][ndvi_hls[[1]] >= 1] = NA
aoi = st_transform(aoi, crs = st_crs(ndvi_hls))

# forest mask ------------------------------------------------------------------
pth_forest = "/mnt/CEPH_PROJECTS/ECO4Alps/Land_Cover/Land_cover_data/HR_Layer/FTY_2018_010m_32632.tif"
forest = stars::read_stars(pth_forest, proxy = TRUE)
forest = forest[st_bbox(st_transform(aoi, st_crs(st_crs(forest))))]
#mapview(st_bbox(forest))
forest = st_as_stars(forest)
#plot(forest)
forest[[1]][forest[[1]] == 0] = NA
forest[[1]][forest[[1]] == 2] = 1

# msk ndvi with forest ---------------------------------------------------------
ndvi_hls = ndvi_hls * forest

# recalc ndvi to be prepared completely for bfast run

# run bfast --------------------------------------------------------------------
SpatialBFM = function(pixels)
{
  #lsts = ts(pixels, c(2016, 1), frequency=30.666667)
  lsts = bfastts(pixels, ndvi_dates, type = c("irregular")) # check how this looks then
  if (sum(!is.na(lsts)) < 100){
    return(NA)
  }
  bfastmonitor(lsts, 2018, formula=response~harmon, order = 1, history = "all", 
               verbose = F)$breakpoint
}


ndvi_prox = read_stars(fls_ndvi_hls_in$pth, along = "t", proxy = T)
ndvi_prox = stars::st_set_dimensions(.x = ndvi_prox, which = "t", 
                                     values = fls_ndvi_hls_in$date)
ndvi_dates = fls_ndvi_hls_in$date

n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
clusterExport(cl = cl, 
              varlist = c("SpatialBFM", "ndvi_dates"), 
              envir = environment())
clusterEvalQ(cl = cl, expr = {
  library(stars)
  library(sf)
  library(dplyr)
  library(bfast)
  library(pbapply)
})

ndvi_prox = st_apply(X = ndvi_prox, MARGIN = c("x", "y"), SpatialBFM, CLUSTER = cl)
write_stars(ndvi_prox, "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/bfast_prox.tif")

stopCluster(cl)


valid_obs = st_apply(ndvi_msk, c("x", "y"), function(x){sum(!is.na(x))}, PROGRESS=TRUE)
StarsResult = st_apply(ndvi_msk, c("x", "y"), SpatialBFM, PROGRESS = TRUE)
StarsResult
plot(StarsResult)
StarsResult %>% pull() %>% c() %>% hist()

# accuracy
# before
# break_lag: 1, 2, 3, 6 , >6 months
# no_break

# bfast reporter

# input 
# ndvi collection, cloud mask, forrest mask
# vaja storm damage shps
# control shps

# workflow 
# loop through damage shps 
# (loop through control shps)
# count total pixels = forest pixels in aoi
# count total observation dates
# count valid observations (per year, per season) -> boxplot
# count snow, cloud pixels (from mask)
# get ndvi ts for aoi mean
# get bfast breakpoints for aoi
# % before
# % break_lag: 1, 2, 3, 6 , >6 months
# % no_break
# get magnitude 

# later in project checks:
# winter
# walddichte 2-3 klassen
# datendichte anzahl der punkte
# outliers -> spike filter
# spatial resolution 3x3 vs pixel
# 






# parallelizatiooooon!!!
st_apply(CLUSTER)

# breakpoint

# magnitude

# no valid pixels

# function to convert decimal year to date


# analysis ---------------------------------------------------------------------

# ndvi msk has the correct values
ndvi_hls_valpx = st_apply(ndvi_hls, c("x","y"), function(x){
  sum(!is.na(x))})
plot(ndvi_hls_valpx)
# look what happens with them from here!

# get mean ndvi for windthrow poly in 2017-2019
ndvi_ts = st_apply(ndvi_hls, c("t"), median, na.rm = TRUE) # 
ndvi_ts = tibble(ndvi = ndvi_ts %>% pull() %>% c(), 
                 date = st_get_dimension_values(ndvi_hls, "t"))

ggplot(ndvi_ts %>% filter(lubridate::year(date) == 2018), aes(x=date, y=ndvi)) +
  geom_line() +
  geom_point()

ggplot(ndvi_ts, aes(x=date, y=ndvi)) +
  geom_line() +
  geom_point()  + 
  geom_vline(xintercept = as.Date("2018-10-28"), col = "red")



# bfast on one pixel
ndvi_dates = st_get_dimension_values(ndvi_hls, "t")
diff.Date(ndvi_dates)
pixels = ndvi_hls[st_point_on_surface(aoi)] %>% pull() %>% c()
summary(pixels)

lsts <- bfastts(pixels, ndvi_dates, type = c("irregular"))
plot(lsts)


# lsts_lin = round(na.approx(lsts), 4)
# lsts_per = round(na.interp(lsts), 4) # bfast monitor not needed!!!!!
# plot(lsts)
# plot(lsts_lin)
# plot(lsts_per)
# plot(lsts_week)


bfm_res = bfastmonitor(lsts, 2018, 
                       formula = response~harmon,# response~season #response~trend, # response~trend+harmon, response~harmon
                       order = 1, # decide 1 or 2 
                       history = "all", #date could be specified e.g one year, all = history without breaks, don't have disturbance in history
                       verbose = T)
# harmon order = 3, reducing will lead to less seasonality = waves per year -> for forrest maybe 2 or 1
# season sbins = 3, num of seasonal dummies -> 4 

plot(bfm_res)


# IMPROVE ERROR HANDLING, CHECK LENGHT OF NON NA AND WRITE TO FILE

SpatialBFM = function(pixels)
{
  #lsts = ts(pixels, c(2016, 1), frequency=30.666667)
  lsts = bfastts(pixels, ndvi_dates, type = c("irregular")) # check how this looks then
  if (sum(!is.na(lsts)) < 100){
    return(NA)
  }
  bfastmonitor(lsts, 2018, formula=response~harmon, order = 1, history = "all", 
               verbose = F)$breakpoint
}


plot(ndvi_hls %>% slice("t", 207))
valid_obs = st_apply(ndvi_hls, c("x", "y"), function(x){sum(!is.na(x))}, PROGRESS=TRUE)
plot(valid_obs)
StarsResult = st_apply(ndvi_hls, c("x", "y"), SpatialBFM, PROGRESS = TRUE)
StarsResult
plot(StarsResult)
StarsResult %>% pull() %>% c() %>% hist()

# accuracy
# before
# break_lag: 1, 2, 3, 6 , >6 months
# no_break

# bfast reporter

# input 
# ndvi collection, cloud mask, forrest mask
# vaja storm damage shps
# control shps

# workflow 
# loop through damage shps 
# (loop through control shps)
# count total pixels = forest pixels in aoi
# count total observation dates
# count valid observations (per year, per season) -> boxplot
# count snow, cloud pixels (from mask)
# get ndvi ts for aoi mean
# get bfast breakpoints for aoi
# % before
# % break_lag: 1, 2, 3, 6 , >6 months
# % no_break
# get magnitude 

# later in project checks:
# winter
# walddichte 2-3 klassen
# datendichte anzahl der punkte
# outliers -> spike filter
# spatial resolution 3x3 vs pixel
# 







library(leaflet)
m <- mapview(aoi)
m@map = m@map %>% 
  addWMSTiles(group = 'EuracMosaic',
              #"https://watersgeo.epa.gov/arcgis/services/NHDPlus_NP21/NHDSnapshot_NP21/MapServer/WmsServer?",
              "http://saocompute.eurac.edu/geoserver/ows?SERVICE=WMS",
              layers  = 'SENTINEL2:S2_MOSAIC_2019_ST_stretch_mask_3857',
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              attribution = "") %>% mapview:::mapViewLayersControl(names = c("EuracMosaic"))
m


# forest -----------------------------------------------------------------------


# forest density ---------------------------------------------------------------











