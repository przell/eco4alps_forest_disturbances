# bfast on eurac - modis

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
# buff_size = 10
# area_px = st_buffer(area_px, buff_size, endCapStyle = "SQUARE")
# mapview(area_px) + mapview(area)

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

# openeo login -----------------------------------------------------------------------
host = "https://openeo.eurac.edu"
conf = read.csv("/home/pzellner@eurac.edu/pwd/openeo_eurac_conf.csv", 
                stringsAsFactors = FALSE)
conf = list(client_id = conf$client_id, secret = conf$secret)
con = connect(host = host)
prov = list_oidc_providers()
login(login_type = "oidc", 
      provider = prov$Eurac_EDP_Keycloak, 
      config = conf, 
      con = con)


# get S2 NDVI ---------------------------------------------------------------
collection = "ADO_NDVI_MODIS_231m_3035"
time_range = list("2016-01-01T00:00:00Z", 
                  "2020-01-01T00:00:00Z")
bands = c("DOY", "NDVI")
bbox = area_px_openeo[[1]]

p = processes()
data = p$load_collection(id = collection, 
                         spatial_extent = bbox,
                         temporal_extent = time_range, 
                         bands = bands)

result = p$save_result(data = ndvi_calc, format="netCDF")

# graph_info = create_user_process(result, id = "test", submit = FALSE)
# print(jsonlite::toJSON(graph_info, pretty = TRUE, auto_unbox = TRUE))

out_name =  paste0("./openeo_tmp/", collection, ".nc")

a = Sys.time()
compute_result(result,
               format = "netCDF",
               output_file = out_name, 
               con = con)
b = Sys.time()-a
b #11 mins for one px and 2years

# get S2Cloudless cloudmask -----------------------------------------------
collection = "s2cloudless_alps"

data = p$load_collection(id = collection, 
                         spatial_extent = bbox,
                         temporal_extent = time_range)

result = p$save_result(data = data, format="netCDF")

out_name =  paste0("./openeo_tmp/", collection, ".nc")

a = Sys.time()
compute_result(result,
               format = "netCDF",
               output_file = out_name, 
               con = con)
b = Sys.time()-a
b #11 mins for one px and 2years


# # function for downloading multiple cubes -------------------------------------
# openeo_downloader = function(collection, bands, bbox, time_range){
#   # load the collection for the selected aoi.
#   data = p$load_collection(id = collection, 
#                            spatial_extent = bbox,
#                            temporal_extent = time_range)
#   
#   # for collections with temporal and band dimension netCDF is suitable.
#   result = p$save_result(data = data, format = "netCDF")
#   
#   # compute result
#   outname = paste0(collection, ".nc")
#   
#   a = Sys.time()
#   compute_result(result,
#                  format = "netCDF",
#                  output_file = outname, 
#                  con = eurac)
#   b = Sys.time() - a
#   message(paste0("Time elapsed for: ", outname, " - ", round(b, 3)))
#   return(outname)
# }
# 



# batch with log ---------------------------------------------------------------
# job = create_job(graph = result,
#                  title = out_name,
#                  description = out_name,
#                  format = "netCDF", 
#                  con = con)
# job
# start_job(job = job$id, 
#           con = con) # use the id of the job (job$id) to start the job
# openeo::list_jobs(con = con) # here you can see your jobs and their status
# openeo::log_job(job$id)
# 
# # get the job info 
# result_obj = list_results(job = job$id, 
#                           con = con)
# 
# # download the data sets
# dwnld = download_results(job = job$id, 
#                          folder = "./openeo_tmp/", # adjust path here
#                          con = con)



# load and plot ----------------------------------------------------------------

# S2 NDVI -----
# load
ndvi = read_ncdf("./openeo_tmp/SAO_S2_ST_BRDF_10m_L2A.nc")

# assign crs
st_crs(ndvi) = st_crs(32632)
mapview(st_bbox(ndvi)) + mapview(area_px[1, ])

# time from unix to date
ndvi_t = st_get_dimension_values(ndvi, "DATE") %>% 
  as.POSIXct(origin = "1970-01-01")
ndvi = ndvi %>%  st_set_dimensions(which = "DATE", values = ndvi_t)

# spatially aggregate
ndvi[[1]][ndvi[[1]] <= 0] =  NA
ndvi_agg = st_apply(X = ndvi, MARGIN = c("DATE"), FUN = median, na.rm = TRUE)

# create ts
ndvi_ts = data.frame(value = ndvi_agg %>% pull() %>% c(), 
                     dates = as.Date(st_get_dimension_values(ndvi_agg, "DATE")))



# plot the timeseries
plot_ts = ggplot(ndvi_ts, aes(x = dates, y = value)) + 
  geom_line() + 
  geom_point()

# S2Cloudless ----
# 0 no cloud, 1 cloud
clouds = read_ncdf("./openeo_tmp/s2cloudless_alps.nc")
st_crs(clouds) = st_crs(32632)
mapview(st_bbox(ndvi)) + mapview(st_bbox(clouds)) + mapview(area_px[1, ])

clouds_agg = st_apply(X = clouds, MARGIN = c("t"), FUN = max)
clouds_t = st_get_dimension_values(clouds, "t")

# create ts
clouds_ts = data.frame(value = clouds_agg %>% pull() %>% c(), 
                       dates = as.Date(st_get_dimension_values(clouds_agg, "t")))


# combine
ndvi_clouds_ts = dplyr::inner_join(ndvi_ts, clouds_ts)
ndvi_clouds_ts = merge(x = ndvi_ts, y = clouds_ts, by = "dates", 
                       all.x = FALSE, all.y = FALSE, suffixes = c("_ndvi", "_clouds"))
ndvi_clouds_ts = ndvi_clouds_ts %>% mutate(ndvi_masked = ifelse(value_clouds == 1, NA, value_ndvi))

ggplot(ndvi_clouds_ts, aes(x = dates, y = ndvi_masked)) + 
  geom_line() + 
  geom_point()

