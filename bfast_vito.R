# bfast on vito

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
#

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
buff_size = 1
area_px = st_buffer(area_px, buff_size, endCapStyle = "SQUARE")

# to lat/lon
area_px = st_transform(area_px, crs = 4326, desired_accuracy = 10)
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

# st_inscribed_circle() would get the inner circle if needed

# openeo -----------------------------------------------------------------------
host = "https://openeo.vito.be"
con = openeo::connect(host)
openeo::login(user = "peter", password = "peter123", login_type = "basic")

collection = "TERRASCOPE_S2_NDVI_V2"
collection = "SENTINEL2_L1C_SENTINELHUB"
collection_viewer(collection)
describe_collection(collection)

time_range = list("2016-01-01T00:00:00.000Z", 
                  "2020-12-31T00:00:00.000Z")
bands = c("B04", "B08", "CLM", "B09", "B8A", "B11",
          "sunAzimuthAngles", "sunZenithAngles", "viewAzimuthMean", "viewZenithMean")
bands = c("CLM")

bbox = area_px_openeo[[1]]

p = processes()
data = p$load_collection(id = collection, 
                         spatial_extent = bbox,
                         temporal_extent = time_range, 
                         bands = bands)
boa_corr = p$atmospheric_correction(data = data, method = "smac") # or use "iCor"

#boa_flt = p$filter_bands(data = boa_corr, bands = c("B04", "B08", "CLM"))
#max_val_comp = p$aggregate_temporal_period()
#x <- seq(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "2 weeks")
#max_val_comp = p$aggregate_temporal()

ndvi_calc = p$reduce_dimension(data = boa_corr, 
                               dimension = "bands", 
                               reducer = function(data, context) {
                                 red = data[1]
                                 nir = data[2]
                                 ndvi = (nir-red)/(nir+red)
                                 return(ndvi)
                                 })
# THIS IS MASKING EVERYTHING
# LOOK AT THE MASK!
cld_band = p$filter_bands(data = data, bands = c("CLM")) # 0 = ok, 1 = cld, 255 = NA
# ndvi_masked = p$mask(data = ndvi_calc, mask = cld_mask)
# https://docs.sentinel-hub.com/api/latest/user-guides/cloud-masks/

filter_function <- function(data, context) {
  no_cloud <- p$eq(data[1], 0) # no cloud is 0 (1 = cloud, 255 = no data)
  # we want to mask all other values, so NOT (0)
  return(p$not(no_cloud))
}

# create mask by reducing bands with our defined formula
cld_mask <- p$reduce_dimension(data = cld_band, reducer = filter_function, dimension = "bands")

# mask the NDVI data
ndvi_masked <- p$mask(ndvi_calc, cld_mask)


result = p$save_result(data = data, format="NetCDF")

graph_info = create_user_process(result, id = "test", submit = FALSE)
print(jsonlite::toJSON(graph_info, pretty = TRUE, auto_unbox = TRUE))

out_name =  "./openeo_tmp/vito_ndvi_smac1.nc"

a = Sys.time()
compute_result(result,
               format = "netCDF",
               output_file = out_name, 
               con = con)
b = Sys.time()-a
b #11 mins for one px and 2years

# load and plot ----------------------------------------------------------------
ndvi = read_ncdf(out_name)
system(paste0("gdalinfo ", out_name, " | head"))
st_crs(ndvi) = st_crs(32632)

plot(ndvi %>% slice("t", 1))

mapview(st_bbox(ndvi)) + 
  mapview(area[1, ]) + 
  mapview(area_px[1, ]) + 
  mapview(st_bbox(c(xmin = bbox$west, xmax = bbox$east, ymin = bbox$south, ymax = bbox$north)))

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




