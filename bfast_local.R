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

# large storm archive ----
# # this shapefile is invalid, can't be used with sf
# # has attributes
# pth_wind = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/9555008/FORWIND_v2.shp"
# wind = st_read(pth_wind)
# wind = wind %>% dplyr::filter(StormName == "Vaia")
# wind = st_transform(wind, crs = 32632)
# wind = st_buffer(wind, 0)
# wind = st_make_valid(wind)
# #table(st_is_valid(wind))
# #wind_area = st_intersection(wind[3, ], st_transform(area, crs = st_crs(wind)))
# 
# wind = wind %>% select(id = Id_poly, event_date = EventDate, 
#                        storm_name = StormName, area = Area, damage_degree = Damage_deg)
# wind = wind %>% mutate(event_date = gsub(pattern = "/", replacement = "-", x = event_date))
# ----

# couple of windthrow areas welschenofen
pth_area = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/area_32632.shp"
area = st_read(pth_area)
mapview(st_bbox(area))
area_bbox = st_bbox(area)

# s2 ndvi masked with fmask ----------------------------------------------------

# file list
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
ndvi_prox = ndvi_prox[st_bbox(area[1, ])]

# read to r
ndvi = st_as_stars(ndvi_prox)
mapview(st_bbox(ndvi)) + mapview(area[1, ])

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
# look what happens with them from here!


# get mean ndvi for windthrow poly in 2017-2019
ndvi_msk_cut = ndvi_msk[area[1, ]]
ndvi_msk_cut[[1]][ndvi_msk_cut[[1]] <= 0] = NA
ndvi_msk_cut = st_apply(ndvi_msk_cut, c("t"), median, na.rm = TRUE) # 
ndvi_ts = tibble(ndvi = ndvi_msk_cut %>% pull() %>% c(), 
                 date = st_get_dimension_values(ndvi_msk_cut, "t"))

ggplot(ndvi_ts %>% filter(lubridate::year(date) == 2019), aes(x=date, y=ndvi)) +
  geom_line() +
  geom_point()

ggplot(ndvi_ts, aes(x=date, y=ndvi)) +
  geom_line() +
  geom_point()

ndvi_dates = st_get_dimension_values(ndvi, "t")
diff.Date(ndvi_dates)

pixels = ndvi[st_point_on_surface(area[1, ])] %>% pull() %>% c()
summary(pixels) # where does 255 come from
pixels[pixels > 1] = NA
pixels[pixels < 0] = NA

lsts <- bfastts(pixels, ndvi_dates, type = c("irregular")) # what is this doing is it changing, adding values? compare values to input values!

# lsts_lin = round(na.approx(lsts), 4)
# lsts_per = round(na.interp(lsts), 4) # bfast monitor not needed!!!!!


# aggregate.daily.to.weekly <- function(daily.ts) {
#   
#   dates      <- as.Date(date_decimal(as.numeric(time(daily.ts))))
#   
#   xts.daily  <- xts(daily.ts, order.by = dates)
#   
#   xts.weekly <- round(xts::apply.weekly(xts.daily, median),4)  # xts
#   
#   start(xts.weekly)
#   ts.weekly <- ts(data = xts.weekly, 
#                   # define the start and end (Year, Week)    
#                   start = c(as.numeric(format(start(xts.weekly), "%Y")),
#                             as.numeric(format(start(xts.weekly), "%W"))), 
#                   end   = c(as.numeric(format(end(xts.weekly), "%Y")), 
#                             as.numeric(format(end(xts.weekly), "%W"))), 
#                   frequency = 52)
#   
#   return(ts.weekly)
# }
# lsts_week = aggregate.daily.to.weekly(lsts_per)

plot(lsts)
plot(lsts_lin)
plot(lsts_per)
plot(lsts_week)


bfm_res = bfastmonitor(lsts, 2018, 
                       formula = response~harmon,# response~season #response~trend, # response~trend+harmon, response~harmon
                       order = 1, 
                       history = "all", #date could be specified e.g one year, all = history without breaks, don't have disturbance in history
                       verbose = T)
# harmon order = 3, reducing will lead to less seasonality = waves per year -> for forrest maybe 2 or 1
# season sbins = 3, num of seasonal dummies -> 4 

plot(bfm_res) # where do all these 1 and 0 come from?



SpatialBFM = function(pixels)
{
  #lsts = ts(pixels, c(2016, 1), frequency=30.666667)
  lsts = bfastts(pixels, ndvi_dates, type = c("irregular"))
  #lsts = round(na.approx(lsts), 4)
  print(length(lsts))
  if (length(lsts) < 50){
    return(NA)
  }
  bfastmonitor(lsts, 2018, formula=response~harmon, order = 1, history = "all", 
               verbose = T)$breakpoint
}


st_apply(ndvi_msk[area[1, ]], c("x", "y"), function(x){length(!is.na(x))}, PROGRESS=TRUE)
StarsResult = st_apply(ndvi_msk[area[1, ]], c("x", "y"), SpatialBFM, PROGRESS=TRUE)




bf_res <- bfast(lsts_week, h = 10/length(lsts_per), 
                season = "harmonic", breaks = 1, max.iter = 2)
plot(bf_res)

# forest -----------------------------------------------------------------------
pth_forest = "/mnt/CEPH_PROJECTS/ECO4Alps/Land_Cover/Land_cover_data/HR_Layer/FTY_2018_010m_32632.tif"
forest = stars::read_stars(pth_forest, proxy = TRUE)
forest = forest[st_bbox(fmask)]
mapview(st_bbox(forest))

# forest density ---------------------------------------------------------------




# fmask ------------------------------------------------------------------------
pth_fmask = "/mnt/CEPH_PRODUCTS/FMASK/SENTINEL2/T32TPS/S2A_MSIL1C_20150704T101006_N0204_R022_T32TPS_20150704T101337.SAFE/L1C_T32TPS_A000162_20150704T101337_Fmask4.tif"
fmask = read_stars(pth_fmask)
plot(fmask)
fmask[[1]][fmask[[1]] > 0] =  1

mapview(st_bbox(fmask)) + mapview(st_bbox(wind))

st_crs(fmask)
wind_fmask = st_intersection(x = st_as_sfc(st_bbox(fmask)), y = wind)

nrow(wind)
length(wind_fmask)
mapview(st_bbox(wind_fmask)) + mapview(wind_fmask[1:5])

# make file list and compare dates!




# apply bfast ------------------------------------------------------------------



# checks:
# winter
# walddichte 2-3 klassen
# datendichte anzahl der punkte
# outliers -> spike filter
# spatial resolution 3x3 vs pixel
# 









