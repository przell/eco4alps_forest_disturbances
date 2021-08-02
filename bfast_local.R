# use bfast with locally calculated s2 ndvi
# 

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

# storms -----------------------------------------------------------------------

pth_stt = "/mnt/CEPH_BASEDATA/GIS/REGIONAL/SOUTHTYROL/BOUNDARIES/SouthTyrol/Bezirksgemeinschaften_WGS84.shp"
stt = st_read(pth_stt)
stt = st_transform(stt, crs = 32632)


# large storm archive ----
# this shapefile is invalid, can't be used with sf
# has attributes
# pth_wind = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/9555008/FORWIND_v2.shp"
# wind = st_read(pth_wind)
# wind = wind %>% dplyr::filter(StormName == "Vaia")
# wind = wind %>% dplyr::filter(Country == "IT")
# 
# wind = st_transform(wind, crs = 32632)
# wind = st_intersection(x = wind, y = st_as_sfc(st_bbox(stt)))
# 
# wind = st_buffer(wind, 0)
# wind = st_make_valid(wind)
# #table(st_is_valid(wind))
# #wind_area = st_intersection(wind[3, ], st_transform(area, crs = st_crs(wind)))
# 
# wind = wind %>% select(id = Id_poly, event_date = EventDate,
#                        storm_name = StormName, area = Area, damage_degree = Damage_deg)
# wind = wind %>% mutate(event_date = gsub(pattern = "/", replacement = "-", x = event_date))
# table(wind$event_date)
# ----

# couple of windthrow areas welschenofen
# event date 2018-10-28
pth_area = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/area_32632.shp"
area = st_read(pth_area)
area_bbox = st_bbox(area)

# # no disturbance manually
# library(mapedit)
# no_dist = mapedit::drawFeatures()
# no_dist = no_dist %>% st_transform(st_crs(area))

mapview(st_bbox(area)) + mapview(area) + mapview(st_bbox(stt)) + mapview(no_dist, col.reg = "red")


# forest mask
pth_forest = "/mnt/CEPH_PROJECTS/ECO4Alps/Land_Cover/Land_cover_data/HR_Layer/FTY_2018_010m_32632.tif"
forest = stars::read_stars(pth_forest, proxy = TRUE)
forest = forest[st_bbox(area)]
mapview(st_bbox(forest))
forest = st_as_stars(forest)
plot(forest)
forest[[1]][forest[[1]] == 0] = NA
forest[[1]][forest[[1]] == 2] = 1

# use this for masking


# put reading ndvi_eurac into function with spatial subset
# put reading ndvi_hls into function with spatial subset


# s2 ndvi eurac ----------------------------------------------------------------
# file list ndvi fmask
path_ndvi = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/02_s2_ndvi_local_eurac/ndvi_novalevante_fmask"
fls_ndvi = list.files(path_ndvi, full.names = TRUE)
fls_ndvi = tibble(pth = fls_ndvi, 
                  date = as.Date(substr(basename(fls_ndvi), 1, 8), format = "%Y%m%d"), 
                  sensor = substr(basename(fls_ndvi), 15, 17))

fls_ndvi_in = fls_ndvi %>% dplyr::filter(lubridate::year(date) >= 2016)



read_ndvi_eurac = function(list_pth = fls_ndvi_in$pth, 
                           list_date = fls_ndvi_in$date, 
                           aoi){
  # read as proxy
  ndvi_prox = read_stars(list_pth, proxy = TRUE, along = "t")
  ndvi_prox = stars::st_set_dimensions(.x = ndvi_prox, which = "t", 
                                       values = list_date)
  
  # loop through patches here
  aoi_bbox = st_bbox(aoi)
  ndvi_prox = ndvi_prox[aoi_bbox]
  
  # read to r
  ndvi = st_as_stars(ndvi_prox)
  ndvi[[1]][(ndvi[[1]] > 1)] = NA
  ndvi[[1]][(ndvi[[1]] < 0)] = NA
  
  
  # create mask
  fmask = ndvi %>% slice("band", 2)
  fmask[[1]][fmask[[1]] > 0] = NA # set 1-4 to NA (clouds, snow etc.)
  fmask = fmask + 1 # set 0 to 1 so that 1 is validdata
  
  # apply mask to ndvi
  ndvi_msk = ndvi %>% slice("band", 1)
  ndvi_msk = ndvi_msk * fmask
  
  return(ndvi_msk)
} 


# s2 ndvi eurac ----------------------------------------------------------------
# file list ndvi fmask
path_ndvi_hls = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi"
fls_ndvi_hls = list.files(path_ndvi_hls, pattern = "ndvi", full.names = TRUE)
fls_ndvi_hls = tibble(pth = fls_ndvi_hls, 
                     date = as.Date(substr(basename(fls_ndvi_hls), 1, 8), format = "%Y%m%d"))

fls_ndvi_hls_in = fls_ndvi_hls %>% dplyr::filter(lubridate::year(date) >= 2016)

path_mask_hls = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/03_hls_s30_v014_ndvi"
fls_mask_hls = list.files(path_mask_hls, pattern = "QA", full.names = TRUE)
fls_mask_hls = tibble(pth = fls_mask_hls, 
                      date = as.Date(substr(basename(fls_mask_hls), 8, 15), format = "%Y%m%d"))

fls_mask_hls_in = fls_mask_hls %>% dplyr::filter(lubridate::year(date) >= 2016)

setdiff(fls_mask_hls_in$date, fls_ndvi_hls_in$date)
setdiff(fls_ndvi_hls_in$date, fls_mask_hls_in$date)
identical(fls_mask_hls_in$date, fls_ndvi_hls_in$date)

read_ndvi_hls = function(list_pth = fls_ndvi_hls_in$pth,
                         list_mask = fls_mask_hls_in$pth,
                         list_date = fls_ndvi_in$date, 
                         aoi){
  # read as proxy
  ndvi_prox = read_stars(list_pth, proxy = TRUE, along = "t")
  ndvi_prox = stars::st_set_dimensions(.x = ndvi_prox, which = "t", 
                                       values = list_date)
  
  # loop through patches here
  aoi = st_transform(aoi, st_crs(ndvi_prox))
  aoi_bbox = st_bbox(aoi)
  ndvi_prox = ndvi_prox[aoi]
  
  # read to r
  ndvi = st_as_stars(ndvi_prox)
  ndvi[[1]][(ndvi[[1]] > 1)] = NA
  ndvi[[1]][(ndvi[[1]] < 0)] = NA
  
  # create mask
  fmask_prox = read_stars(list_mask, proxy = TRUE)
  fmask_prox = fmask_prox[aoi]
  fmask = st_as_stars(fmask_prox)
  fmask = st_apply(X = fmask, MARGIN = c("x", "y"), FUN = function(x){
    ifelse(sum(as.integer(intToBits(x))[1:5]) == 0, 1, NA)
    # WHAT TO DO ON BIT 6-7 Aerosols? Is climatology good or bad
  })
  
  # apply mask to ndvi
  ndvi_msk = ndvi * fmask
  
  return(ndvi_msk)
} 


# read ndvi --------------------------------------------------------------------
aoi = area[127, ]

ndvi_eurac = read_ndvi_eurac(list_pth = fls_ndvi_in$pth, 
                             list_date = fls_ndvi_in$date, 
                             aoi = aoi)

# check that dates are the same on both input for hls: mask and ndvi
ndvi_hls = read_ndvi_hls(list_pth = fls_ndvi_hls_in$pth, 
                         list_mask = fls_mask_hls_in$pth, 
                         list_date = fls_ndvi_hls_in$date, 
                         aoi = aoi)


# analysis ---------------------------------------------------------------------

# ndvi msk has the correct values
ndvi_msk_valpx = st_apply(ndvi_msk, c("x","y"), function(x){
  sum(!is.na(x))})
plot(ndvi_msk_valpx)
# look what happens with them from here!

# get mean ndvi for windthrow poly in 2017-2019
ndvi_msk_cut = ndvi_msk[aoi]
ndvi_msk_cut[[1]][ndvi_msk_cut[[1]] <= 0] = NA
ndvi_msk_cut = st_apply(ndvi_msk_cut, c("t"), median, na.rm = TRUE) # 
ndvi_ts = tibble(ndvi = ndvi_msk_cut %>% pull() %>% c(), 
                 date = st_get_dimension_values(ndvi_msk_cut, "t"))

ggplot(ndvi_ts %>% filter(lubridate::year(date) == 2020), aes(x=date, y=ndvi)) +
  geom_line() +
  geom_point()

ggplot(ndvi_ts, aes(x=date, y=ndvi)) +
  geom_line() +
  geom_point()  + 
  geom_vline(xintercept = as.Date("2018-10-28"), col = "red")



# bfast on one pixel
ndvi_dates = st_get_dimension_values(ndvi_msk, "t")
diff.Date(ndvi_dates)
pixels = ndvi_msk[st_point_on_surface(aoi)] %>% pull() %>% c()
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

plot(bfm_res) # where do all these 1 and 0 come from? # THE TS LÖÖKS LIKE SHIT WITH ALL 1 and 0???


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


plot(ndvi_msk %>% slice("t", 207))
valid_obs = st_apply(ndvi_msk, c("x", "y"), function(x){sum(!is.na(x))}, PROGRESS=TRUE)
plot(valid_obs)
StarsResult = st_apply(ndvi_msk, c("x", "y"), SpatialBFM, PROGRESS = TRUE)
StarsResult
plot(StarsResult[aoi])
StarsResult[aoi] %>% pull() %>% c() %>% hist()


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
#  - cumsum of 






# checks:
# winter
# walddichte 2-3 klassen
# datendichte anzahl der punkte
# outliers -> spike filter
# spatial resolution 3x3 vs pixel
# 









