

# libs ----
#library(remotes)
#remotes::install_github(repo="Open-EO/openeo-r-client",ref="develop", dependencies=TRUE)
packageVersion("openeo") # dev version needed
library(openeo)
library(stars)
library(sf)
library(mapview)
# library(mapedit)
# library(ggplot2)
# library(plotly)
# library(jsonlite)
library(dplyr)

# login ------------------------------------------------------------------------
host = "https://openeo.eodc.eu/v1.0"
host = "https://openeo.eodc.eu/" # 
user = "openeo-user"
password = "openeo-56XA!"

# con = openeo::connect(host = host)
con = openeo::connect(host = host, 
                      user = user, 
                      password = password, 
                      login_type = "basic")

con$isConnected()
con$isLoggedIn()

# describe collection ----------------------------------------------------------
list_collections()
describe_collection("boa_sentinel_2")
collection_viewer("boa_sentinel_2")

# extent of collection ---------------------------------------------------------
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

# process ----------------------------------------------------------------------
# select collection
collection = "boa_sentinel_2"

# get an aoi. use the square tool to draw an area.
pnts = mapedit::drawFeatures()
mapview(pnts)

bbox = st_bbox(pnts)
bbox = list(west = bbox[[1]],
            east = bbox[[3]],
            south = bbox[[2]],
            north = bbox[[4]])

# jenesien
# bbox = list(west = 11.25841,
#             east = 11.33892,
#             south = 46.54007,
#             north = 46.58447)

# define the time range
time_range = list("2018-01-01T00:00:00.000Z", 
                  "2018-12-31T00:00:00.000Z")
time_range = list("2018-01-01", 
                  "2018-12-31")

# define the bands
sel_bands = list("band_4", "band_8")

# load processes of backend to object
p = processes()

# load the collection for the selected aoi.
data = p$load_collection(id = collection, 
                         spatial_extent = bbox,
                         temporal_extent = time_range,
                         bands = sel_bands)

# for collections with temporal and band dimension netCDF is suitable.
# openeo::list_file_formats()
result = p$save_result(data = data, format = "netCDF")

# print process graph as JSON
graph = as(result, "Graph")
graph


# get result -------------------------------------------------------------------

# synchronous
# compute the result. change format and extension according to "save_result".
compute_result(result,
               format = "netCDF",
               output_file = "./openeo_tmp/eodc_s2.nc", 
               con = con)

# batch
job_id = create_job(graph = result,
                    title = "eodc_s2",
                    description = "eodc_s2",
                    format = "netCDF")
start_job(job = job_id)
list_jobs(con = eurac)
result_obj = list_results(job = job_id)
download_results(job = job_id, folder = "./openeo_tmp/eodc_s2.nc")



# read the result --------------------------------------------------------------
area_ts = read_ncdf("eodc_s2.nc") # , var = "DOY") # but bandname here if you want
# hopefully projection works from eodc...
# st_crs(area_ts) = st_crs(3035) # assign projection manually
# hopefully na is represented logically correct
# area_ts$DOY[area_ts$DOY < -1] = NA # assign the NA value manually
area_ts = read_ncdf("/home/pzellner@eurac.edu/s2_eodc_force_timeseries.nc")
plot(area_ts %>% slice("time", 24))

#' plot interactively
mapview(st_bbox(area_ts))
pts_ts = st_as_sf(area_ts, as_points = TRUE, merge = FALSE)
mapview(pts_ts, zcol = "DOY.V30") # this is doy 30*4 = 120






