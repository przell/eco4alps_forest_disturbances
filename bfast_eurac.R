# bfast on eurac

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
buff_size = 1
area_px = st_buffer(area_px, buff_size, endCapStyle = "SQUARE")
mapview(area_px) + mapview(area)

# to lat/lon
area_px = st_transform(area_px, crs = 4326)


px2bbox = function(x){pb
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
# login
eurac_host = "https://openeo.eurac.edu"
conf = read.csv("/home/pzellner@eurac.edu/pwd/openeo_eurac_conf.csv", 
                stringsAsFactors = FALSE)
conf = list(client_id = conf$client_id, secret = conf$secret)
eurac = connect(host = eurac_host)
prov = list_oidc_providers()
login(login_type = "oidc", 
      provider = prov$Eurac_EDP_Keycloak, 
      config = conf, 
      con = eurac)



collection = "SAO_S2_ST_BRDF_10m_L2A"
collection_mask = "s2cloudless_alps"
collection_viewer(collection)

# adapt time range to mask
# is mask on same days?
# is mask on same grid?
# check on normal files maybe first?
time_range = list("2015-07-04 00:00:00.000", 
                  "2021-01-21 00:00:00.000")
bands = c("B04", "B08", "CLM", "B09", "B8A", "B11",
          "sunAzimuthAngles", "sunZenithAngles", "viewAzimuthMean", "viewZenithMean")

bbox = area_px_openeo[[1]]

p = processes()
data = p$load_collection(id = collection, 
                         spatial_extent = bbox,
                         temporal_extent = time_range, 
                         bands = bands)

s2_cloudless = p$load_collection(id = collection, 
                                 spatial_extent = bbox,
                                 temporal_extent = time_range, 
                                 bands = bands)

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
cld_mask = p$filter_bands(data = boa_corr, bands = c("CLM")) # 0 = ok, 1 = cld, 255 = NA
ndvi_masked = p$mask(data = ndvi_calc, mask = cld_mask)

result = p$save_result(data = ndvi_masked, format="NetCDF")

graph_info = create_user_process(result, id = "test", submit = FALSE)
print(jsonlite::toJSON(graph_info, pretty = TRUE, auto_unbox = TRUE))

out_name =  "./openeo_tmp/vito_ndvi_smac_msk.nc"

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




