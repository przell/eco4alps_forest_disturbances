# eco4alps
# forrest disturbance
# bfast with sentinel 2

# resources on bfast
# https://philippgaertner.github.io/2018/04/bfast-preparation/
# https://github.com/Open-EO/openeo-usecases/tree/master/WUR_BFAST_UseCase/R

# s1 collections to use 
# openEO -> Eurac Backend -> S2_L2A_T32TPS (Sen2Cor) + S2_Cloudless_Alps (Cloud Mask von Mattia; Sen2Cloudless) / oder Scene Classification
# openEO -> EODC Backend -> S2 L2A -> openEO platform UC6 Preprocessed?

# libs
library("sf")
library("stars")
library("pbapply")
library("bfast")
library("dplyr")

library("mapview")


# load reference regions -------------------------------------------------------
# area welschenofen from vaia storm
# no attributes
pth_area = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/area_32632.shp"
area = st_read(pth_area)
mapview(st_bbox(area))
mapview(area)
# area

# global wind damage database 
# this shapefile is invalid, can't be used with sf 
# has attributes
pth_wind = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/9555008/FORWIND_v2.shp"
wind = st_read(pth_wind) 
#wind = wind %>% dplyr::filter(Country == "IT")
wind = wind %>% dplyr::filter(StormName == "Vaia")
mapview(st_bbox(wind))
# wind
table(wind$EventDate)
mapview(head(wind))

wind = st_transform(wind, crs = 3035)
wind = st_buffer(wind, 0)
wind = st_make_valid(wind)
#table(st_is_valid(wind))
#wind_area = st_intersection(wind[3, ], st_transform(area, crs = st_crs(wind)))

# load s1 data -----------------------------------------------------------------
# don't know what to use here and how... all in SAFE format
# pth_s2 = "/mnt/CEPH_PRODUCTS/EURAC_L2A/SENTINEL2/alps/T32TPS"
# fls_s2 = list.files(pth_s2)
# View(as.data.frame(fls_s2))

# get through openEO

# login
eurac_host = "https://openeo.eurac.edu"
conf = read.csv("./pwd/openeo_eurac_conf.csv")
conf = list(client_id = conf$client_id, secret = conf$secret)
eurac = connect(host = euracHost)
prov = list_oidc_providers()$Eurac_EDP_Keycloak
login(login_type = "oidc", 
      provider = prov$Eurac_EDP_Keycloak, 
      config = conf, 
      con = eurac)
eurac$isLoggedIn()

# aoi
aoi = area[274, ]
#aoi = area
mapview(st_bbox(aoi)) + mapview(aoi)

# poly...
bbox = aoi %>% st_transform(4326) %>% st_bbox()
# or point?
# bbox = aoi %>% st_transform(4326) %>% st_centroid() %>% st_bbox()

bbox = list(west = bbox[[1]],
            east = bbox[[3]],
            south = bbox[[2]],
            north = bbox[[4]])

# period
time_range = list("2016-01-01T00:00:00.000Z", 
                  "2018-12-31T00:00:00.000Z")

# collection
collection_name = ""


# load collection
p = processes()

data = p$load_collection(id = collection_name, 
                         spatial_extent = bbox,
                         temporal_extent = time_range)

# filter bands

# ndvi
spectral_reduce = p$reduce_dimension(data = data, dimension = "bands",reducer = function(data,context) {
  B08 = data[1]
  B04 = data[2]
  B02 = data[3]
  (2.5 * (B08 - B04)) / sum(B08, 6 * B04, -7.5 * B02, 1)
})

# cloudmasking?

# save result as json
result = p$save_result(data = data, format="netCDF")

# compute result
compute_result(result,
               format = "netCDF",
               output_file = "eco4alps_vaia.nc", 
               con = eurac)



area_ts = read_ncdf("openeo_interactive_area.nc", var = "DOY") # this is actually NDVI, will be adapted
st_crs(area_ts) = st_crs(3035) # assign projection manually
area_ts$DOY[area_ts$DOY < -1] = NA # assign the NA value manually


# test ----------------------------
ndvi = bfast::ndvi
str(ndvi)
class(ndvi)
head(ndvi)
tplot(ndvi)


# test stars ------------------------------------------
# Get some data
# data(bale3g.v1, package="gimms")
# # Sample plot
# gts = ts(c(bale3g.v1[1,1]), start=c(1981, 7), frequency=24)
# plot(bfastmonitor(gts, 2010))

# modis ndvi
pth_ndvi = "/mnt/CEPH_PROJECTS/EURAC_NDVI_MODIS/int16/"
years = 2001:2020
pth_years = paste0(pth_ndvi, "/", years)
names(pth_years) = years
files_years = list.files(pth_years, pattern = ".tif", full.names = TRUE)

# extract year and doy from file name
files_years = data.frame(path = files_years, 
                         doy = gsub(pattern = ".*MCD4\\.([0-3][0-9][0-9])\\.eurac.*", 
                                    replacement = "\\1", 
                                    x = files_years), 
                         year = gsub(pattern = ".*complete\\.([2][0][0-9][0-9])\\.MCD4.*", 
                                     replacement = "\\1", 
                                     x = files_years), 
                         stringsAsFactors = FALSE)

files_years$date = as.Date(as.numeric(files_years$doy)-1, 
                           origin = as.Date(ISOdate(files_years$year, 1, 1)))

# read as stars with subset
# vhi2read = vhi_df %>% filter(year %in% c(2001:2019), doy %in% seq(from = 124, to = 244, by = 4))
ndvi = read_stars(files_years$path, proxy = TRUE, along = "time") 
ndvi = stars::st_set_dimensions(.x = ndvi, which = "time",
                                values = files_years$date)
ndvi = ndvi %>% slice("band", 1)
aoi = st_transform(aoi, crs = st_crs(ndvi))
ndvi = ndvi[aoi]

ndvi = st_as_stars(ndvi)

# (ndvi_ras = as(ndvi %>% slice("time", 350), "Raster"))
# mapview(ndvi_ras) + mapview(area[274, ])

# pull ts for a pixel
ndvi_ts = ndvi[,4,6,] %>% pull() %>% c()
ndvi_ts = data.frame(date = as.Date(st_get_dimension_values(ndvi, "time")), 
                     ndvi = ndvi_ts)
library(ggplot2)
ggplot(data = ndvi_ts, mapping = aes(x = date, y = ndvi)) +
  geom_line()

# make ts regular (missing date files get NA)
date_range = lapply(2001:2020, function(x){
  seq(as.Date(paste0(x, "-01-04")), as.Date(paste0(x, "-12-30")), by = 4)
})
missings = lapply(date_range, function(x){
  x[!x %in% ndvi_ts$date] 
})

missings = data.frame(date = as.Date(unlist(missings)), ndvi = NA, row.names = NULL)
ndvi_ts_reg = rbind(ndvi_ts, missings)
ndvi_ts_reg = ndvi_ts_reg %>% dplyr::arrange(date)

# create ts object
ts_dates = ts(ndvi_ts$ndvi, start = c(2001, 4), frequency = 96)

# create ts object via bfast
ts_dates = bfast::bfastts(data = ndvi_ts$ndvi, dates = ndvi_ts$date, type = "irregular")

# want to start monitoring in 2016. And use 2001 to 2015 as the history periond
bfastmonitor(ts_dates, 2016)$breakpoint # this is what happens in the function
plot(bfastmonitor(ts_dates, 2016))


# THERE IS ALSO THE SPATIALBFAST PACKAGE: http://www.loicdutrieux.net/bfastSpatial/
# Define the pixel-wise function


SpatialBFM = function(pixels)
{
  lsts = ts(pixels, c(2001, 4), frequency = 96)
  bfastmonitor(lsts, 2016)$breakpoint
}


# ## Same but with stars object
# bale_stars = st_as_stars(bale3g.v1)
# StarsResult = st_apply(bale_stars, c("x", "y"), SpatialBFM, PROGRESS=TRUE)
StarsResult = st_apply(ndvi, c("x", "y"), SpatialBFM, PROGRESS=TRUE)


StarsResult = st_apply(ndvi, c("x", "y"), function(x){
  lsts = bfast::bfastts(data = pixels, dates = dates)
})

plot(StarsResult)

# lessons learned
# bfast needs ts object
# ts object needs to be regular time series
# ts frequency is defined in hours
# got it to work on a dataframe/vector
# for some reason when creating the ts object with bfastts() on daily it gives correct breakpoint
# when creating ts on 4d frequency by hand it gives a different (wrong) result
# changing the start date affects the result
# didn't get it to work directyl on stars object


#################################
# https://gist.github.com/GreatEmerald/d9269d2b8c270bf8e8a1ebe9462e54d5
library(bfast)
library(raster)
install.packages("gimms")
# Get some data
data(bale3g.v1, package="gimms")
# Sample plot
gts = ts(c(bale3g.v1[1,1]), start=c(1981, 7), frequency=24)
plot(bfastmonitor(gts, 2010))

# Define the pixel-wise function
SpatialBFM = function(pixels)
{
  lsts = ts(pixels, c(1981, 7), frequency=24)
  bfastmonitor(lsts, 2010)$breakpoint
}

## raster-based
RasterResult = calc(bale3g.v1, SpatialBFM, progress="text")
plot(RasterResult)

## Same but with stars object
library(stars)
bale_stars = st_as_stars(bale3g.v1)

StarsResult = st_apply(bale_stars, c("x", "y"), SpatialBFM, PROGRESS=TRUE)
plot(StarsResult)


