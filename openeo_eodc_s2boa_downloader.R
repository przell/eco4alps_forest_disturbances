

# libs ----
#library(remotes)
#remotes::install_github(repo="Open-EO/openeo-r-client",ref="develop", dependencies=TRUE)
packageVersion("openeo") # dev version needed
library(openeo)
library(stars)
library(sf)
library(mapview)
library(mapedit)
library(ggplot2)
library(plotly)
library(jsonlite)
library(dplyr)

# login ----
host = "https://openeo.eodc.eu/" # "https://openeo.eodc.eu/v1.0"
user = "openeo-user"
password = "openeo-56XA!"

con = openeo::connect(host = host)
con = openeo::connect(host = host, 
                      user = user, 
                      password = password, 
                      login_type = "basic")

openeo::login(user = user, 
              password = password, 
              login_type = "basic", 
              con = con)

con$isConnected()
con$isLoggedIn()



# describe collection ----
list_collections()
describe_collection("boa_sentinel_2")
collection_viewer("boa_sentinel_2")

# extent of collection ----
extent_viewer = function(collection){
  extent = openeo::describe_collection(collection)$extent$spatial
  extent = sf::st_bbox(obj = c(xmin = extent[1], 
                               xmax = extent[3], 
                               ymax = extent[4], 
                               ymin = extent[2]), 
                       crs = sf::st_crs(4326))
  mapview::mapview(extent)
}

extent_viewer("boa_sentinel_2")

# process ----
#' get an aoi. use the square tool to draw an area.
pnts = mapedit::drawFeatures()
mapview(pnts)

bbox = st_bbox(pnts)
bbox = list(west = bbox[[1]],
            east = bbox[[3]],
            south = bbox[[2]],
            north = bbox[[4]])

#' define the time range
time_range = list("2018-01-01T00:00:00.000Z", 
                  "2018-12-31T00:00:00.000Z")

#' load the collection for the selected aoi.
data = p$load_collection(id = ado_colls_names$ADO_NDVI_MODIS_231m_3035, 
                         spatial_extent = bbox,
                         temporal_extent = time_range)

#' for collections with temporal and band dimension netCDF is suitable.
result = p$save_result(data = data, format = "netCDF")

#' compute the result. change format and extension according to "save_result".
compute_result(result,
               format = "netCDF",
               output_file = "eodc_s2.nc", 
               con = con)

#' read the result 
area_ts = read_ncdf("eodc_s2.nc") # , var = "DOY") # but bandname here if you want
# hopefully projection works from eodc...
# st_crs(area_ts) = st_crs(3035) # assign projection manually
# hopefully na is represented logically correct
# area_ts$DOY[area_ts$DOY < -1] = NA # assign the NA value manually

#' plot interactively
mapview(st_bbox(area_ts))
pts_ts = st_as_sf(area_ts, as_points = TRUE, merge = FALSE)
mapview(pts_ts, zcol = "DOY.V30") # this is doy 30*4 = 120






