# bfast on vito

# libs -------------------------------------------------------------------------
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
# to lat/lon
area = st_transform(area, crs = 4326)

# get center pixel within each polygon 
# providing polygons is only supported on vito so far
# so for first test only get a pixel
area_px = st_point_on_surface(area)
mapview(area_px) + mapview(area)

px2bbox = function(x){
  bbox = st_bbox(x)
  bbox = list(west = bbox[[1]],
              east = bbox[[3]],
              south = bbox[[2]],
              north = bbox[[4]])
}

area_px_openeo = lapply(st_geometry(area_px), px2bbox)
area_px_openeo[[1]]

# openeo -----------------------------------------------------------------------
host = "https://openeo.vito.be"
con = openeo::connect(host)
openeo::login(user = "peter", password = "peter123", login_type = "basic")

collection = "TERRASCOPE_S2_NDVI_V2"
collection = "SENTINEL2_L1C_SENTINELHUB"
collection_viewer(collection)

time_range = list("2017-01-01T00:00:00.000Z", 
                  "2018-12-31T00:00:00.000Z")
bands = c("B04", "B08", "CLP", "B09", "B8A", "B11",
          "sunAzimuthAngles", "sunZenithAngles", "viewAzimuthMean", "viewZenithMean")

bbox = area_px_openeo[[1]]

p = processes()
data = p$load_collection(id = collection, 
                         spatial_extent = bbox,
                         temporal_extent = time_range, 
                         bands = bands)
boa_corr = p$atmospheric_correction(data = data, method = "smac") # or use "iCor"
ndvi_calc = p$reduce_dimension(data = boa_corr, 
                               dimension = "bands", 
                               reducer = function(data, context) {
                                 red = data[1]
                                 nir = data[2]
                                 (nir-red)/(nir+red)})
# maybe do smoothing to weekly or so

result = p$save_result(data = ndvi_calc, format="NetCDF")

graph_info = create_user_process(result, id = "test", submit = FALSE)
print(jsonlite::toJSON(graph_info, pretty = TRUE, auto_unbox = TRUE))

out_name =  "./openeo_tmp/vito_ndvi_smac.nc"

a = Sys.time()
compute_result(result,
               format = "netCDF",
               output_file = out_name, 
               con = con)
b = Sys.time()-a
b #11 mins for one px and 2years

# load and plot ----------------------------------------------------------------
ndvi = read_ncdf(out_name)

ndvi_ts_df = data.frame(value = ndvi %>% pull() %>% c(), 
                        dates = as.Date(st_get_dimension_values(ndvi, "t")))
# plot the timeseries
plot_ts = ggplot(ndvi_ts_df, aes(x = dates, y = value)) + 
  geom_line() + 
  geom_point()


plot_ts_plotly = plotly::ggplotly(plot_ts)
plot_ts_plotly

# cloud contaminated
# whats the time step?

# get full history!




