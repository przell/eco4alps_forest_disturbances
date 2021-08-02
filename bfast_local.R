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
# # this shapefile is invalid, can't be used with sf
# # has attributes
# pth_wind = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/9555008/FORWIND_v2.shp"
# wind = st_read(pth_wind)
# # wind = wind %>% dplyr::filter(StormName == "Vaia")
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
pth_area = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/area_32632.shp"
area = st_read(pth_area)
area_bbox = st_bbox(area)

# no disturbance manually
library(mapedit)
no_dist = mapedit::drawFeatures()
no_dist = no_dist %>% st_transform(st_crs(area))

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


# s2 ndvi masked with fmask and forest hrl -------------------------------------
# file list ndvi fmask
path_ndvi = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/02_s2_ndvi_local_eurac/ndvi_novalevante_fmask"
fls_ndvi = list.files(path_ndvi, full.names = TRUE)
fls_ndvi = tibble(pth = fls_ndvi, 
                  date = as.Date(substr(basename(fls_ndvi), 1, 8), format = "%Y%m%d"), 
                  sensor = substr(basename(fls_ndvi), 15, 17))

fls_ndvi_in = fls_ndvi %>% dplyr::filter(lubridate::year(date) >= 2016)

# read as proxy
ndvi_prox = read_stars(fls_ndvi_in$pth, proxy = TRUE, along = "t")
ndvi_prox = stars::st_set_dimensions(.x = ndvi_prox, which = "t", 
                                     values = fls_ndvi_in$date)

# st_dimensions(ndvi)
# st_get_dimension_values(ndvi, "t")





# subset spatially
# THIS IS WHERE TO LOOP THROUGH AREA
aoi = area[1, ]
aoi = no_dist
aoi_bbox = st_bbox(aoi)

ndvi_prox = ndvi_prox[aoi_bbox]

# read to r
ndvi = st_as_stars(ndvi_prox)
mapview(st_bbox(ndvi)) + mapview(aoi)

# create mask
fmask = ndvi %>% slice("band", 2)
fmask[[1]][fmask[[1]] > 0] = NA # set 1-4 to NA (clouds, snow etc.)
fmask = fmask + 1 # set 0 to 1 so that 1 is validdata

# apply mask
ndvi_msk = ndvi %>% slice("band", 1)
#plot(ndvi_msk %>% slice("t", 200))
ndvi_msk = ndvi_msk * fmask
#plot(ndvi_msk %>% slice("t", 200))

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
  geom_point()


# bfast on one pixel
ndvi_dates = st_get_dimension_values(ndvi_msk, "t")
diff.Date(ndvi_dates)
pixels = ndvi_msk[st_point_on_surface(aoi)] %>% pull() %>% c()
summary(pixels)

lsts <- bfastts(pixels, ndvi_dates, type = c("irregular"))
plot(lsts)


# lsts_lin = round(na.approx(lsts), 4)
# lsts_per = round(na.interp(lsts), 4) # bfast monitor not needed!!!!!
plot(lsts)
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
StarsResult = st_apply(ndvi_msk, c("x", "y"), SpatialBFM, PROGRESS = FALSE)
StarsResult
plot(StarsResult)


# forest -----------------------------------------------------------------------


# forest density ---------------------------------------------------------------









# checks:
# winter
# walddichte 2-3 klassen
# datendichte anzahl der punkte
# outliers -> spike filter
# spatial resolution 3x3 vs pixel
# 









