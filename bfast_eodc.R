# bfast on eodc

# libs -------------------------------------------------------------------------
library("openeo")
library("sf")
library("stars")
library("pbapply")
library("bfast")
library("dplyr")
library("mapview")
library("ggplot2")
library("plotly")

# load reference regions -------------------------------------------------------
# area welschenofen from vaia storm
# no attributes
pth_area = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/area_32632.shp"
area = st_read(pth_area)
mapview(area)

# # global wind damage database ----
# # this shapefile is invalid, can't be used with sf 
# # has attributes
# pth_wind = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/9555008/FORWIND_v2.shp"
# wind = st_read(pth_wind) 
# #wind = wind %>% dplyr::filter(Country == "IT")
# wind = wind %>% dplyr::filter(StormName == "Vaia")
# mapview(st_bbox(wind))
# # wind
# table(wind$EventDate)
# mapview(head(wind))
# 
# wind = st_transform(wind, crs = 3035)
# wind = st_buffer(wind, 0)
# wind = st_make_valid(wind)
# #table(st_is_valid(wind))
# #wind_area = st_intersection(wind[3, ], st_transform(area, crs = st_crs(wind)))
# ----

# prep area for looping --------------------------------------------------------

# get center pixel within each polygon 
# providing polygons is only supported on vito so far
# so for first test only get a pixel
area_px = st_point_on_surface(area)

# add some buffer to avoid point error in openeo
buff_size = 20
area_px = st_buffer(area_px, buff_size, endCapStyle = "SQUARE")
mapview(area_px) + mapview(area)

# to lat/lon
area_px = st_transform(area_px, crs = 4326)


px2bbox = function(x){
  bbox = st_bbox(x)
  bbox = list(west = bbox[[1]],
              east = bbox[[3]],
              south = bbox[[2]],
              north = bbox[[4]])
}

area_px_openeo = lapply(st_geometry(area_px), px2bbox)
area_px_openeo[[1]]

# st_inscribed_circle() would get the inner circle if needed

# openeo -----------------------------------------------------------------------

# login ----
# live
host = "https://openeo.eodc.eu/"
user = "openeo-user"
password = "openeo-56XA!"

# dev 
host = "https://openeo-dev.eodc.eu/"
user = "openeo-user"
password = "imSLLCd6rGT8"

con = openeo::connect(host = host, 
                      user = user, 
                      password = password, 
                      login_type = "basic")


# metadata ----
extent_viewer = function(collection, ...){
  extent = openeo::describe_collection(collection)$extent$spatial
  extent = sf::st_bbox(obj = c(xmin = extent[1], 
                               xmax = extent[3], 
                               ymax = extent[4], 
                               ymin = extent[2]), 
                       crs = sf::st_crs(4326))
  mapview::mapview(extent, ...)
}

extent_viewer("boa_sentinel_2") +
  extent_viewer("boa_landsat_8", col.regions = "yellow") +
  extent_viewer("gamma0_sentinel_1_dv", col.regions = "green")

collection = "boa_sentinel_2"
collection_viewer(collection)
describe_collection(collection)

# defs ----
collection = "boa_sentinel_2"
time_range = list("2016-01-07T12:00:00Z", 
                  "2016-04-29T12:00:00Z")
bands = c("B04", "B08")
bbox = area_px_openeo[[1]]

# process graph ----
p = processes()

data = p$load_collection(id = collection, 
                         spatial_extent = bbox,
                         temporal_extent = time_range, 
                         bands = bands)

ndvi_calc = p$reduce_dimension(data = data, 
                               dimension = "bands", 
                               reducer = function(data, context) {
                                 red = data[1]
                                 nir = data[2]
                                 ndvi = (nir-red)/(nir+red)
                                 return(ndvi)
                               })

result = p$save_result(data = ndvi_calc, format="NetCDF")

graph_info = create_user_process(result, id = "test", submit = FALSE)
print(jsonlite::toJSON(graph_info, pretty = TRUE, auto_unbox = TRUE))

out_name =  "./openeo_tmp/eodc_ndvi_s2.nc"


# synchronous ----
a = Sys.time()
compute_result(result,
               format = "netCDF",
               output_file = out_name, 
               con = con)
b = Sys.time()-a
b #11 mins for one px and 2years

# batch ----
job = create_job(graph = result,
                 title = out_name,
                 description = out_name,
                 format = "netCDF", 
                 con = con)
job
start_job(job = job$id, 
          con = eurac) # use the id of the job (job$id) to start the job
openeo::list_jobs(con = eurac) # here you can see your jobs and their status
# get the job info 
result_obj = list_results(job = job$id, 
                          con = eurac)

# download the data sets
dwnld = download_results(job = job$id, 
                         folder = "Data/", # adjust path here
                         con = eurac)



# load and plot ----
ndvi = read_ncdf(out_name)

ndvi_ts_df = data.frame(value = ndvi %>% pull() %>% c(), 
                        dates = as.Date(st_get_dimension_values(ndvi, "t")))
# plot the timeseries
plot_ts = ggplot(ndvi_ts_df, aes(x = dates, y = value)) + 
  geom_line() + 
  geom_point()


plot_ts_plotly = plotly::ggplotly(plot_ts)
plot_ts_plotly

# bfast ------------------------------------------------------------------------


