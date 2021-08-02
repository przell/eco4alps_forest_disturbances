# wrappers for appeears api


# login_appeears fun ----
# def
# it gives you an token to use for furhter requests of the api

login_appeears = function(user, 
                          password, 
                          api_url = 'https://lpdaacsvc.cr.usgs.gov/appeears/api/'){
  
  # generate secret, encoded user and password
  secret = jsonlite::base64_enc(paste(user, password, sep = ":"))
  
  # log in
  response <- httr::POST(paste0(api_url, "login"), 
                         add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                                     "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                         body = "grant_type=client_credentials")
  
  response_content <- content(response)
  token_response <- toJSON(response_content, auto_unbox = TRUE)
  token <- paste(fromJSON(token_response)$token_type, fromJSON(token_response)$token)
  message(paste0("this is your token: ", token))
  
  return(token)
}

# # test
# token = login_appeears(user = user, password = password)





# request_job fun ----
# def
# it creates a job on app eears
# it gives you task_id and token -> can be used for status querry and download

# comments
# - you have to pass the token from the function login_appeears
# - only one product supported by this function currently
# - layers and product names have to be found in the api
# - date format:
# - aoi has to be a valid sf object
# - projections and file format can be found in the api

request_appeears = function(task_name, 
                            product, 
                            layers, 
                            start_date, 
                            end_date, 
                            aoi, 
                            projection = "native", 
                            file_format = "geotiff", 
                            task_type = "area", 
                            token, 
                            api_url = 'https://lpdaacsvc.cr.usgs.gov/appeears/api/'){
  
  # convert aoi to json ----
  aoi = st_transform(aoi, crs = st_crs(4326))
  aoi = geojsonsf::sf_geojson(aoi, simplify = FALSE) # to geojson
  aoi = geojsonR::FROM_GeoJson(aoi) # to list for request
  aoi$features[[1]]$geometry$coordinates <- list(aoi$features[[1]]$geometry$coordinates) # change for request struct
  
  # define the api call ----
  # description
  task_desc = list(task_type = task_type, 
                   task_name = task_name)
  # dates
  dates = data.frame(startDate = start_date, 
                     endDate = end_date)
  task_dates = list(dates = dates)
  # product and layers
  product = rep(x = product, length(layers))
  layers = data.frame(product = product, layer = layers)
  task_layers = list(layers = layers)
  # output
  output = list(projection = projection)
  output$format$type <- file_format
  task_output = list(output = output)
  # aoi
  task_geo = list(geo = aoi)
  
  # combine to api call ----
  # combine parameters
  task_params = list(params = c(task_dates, task_layers, task_output, task_geo))
  # combine complete task
  task = c(task_desc, task_params)
  # convert task to json
  task_json <- jsonlite::toJSON(task, auto_unbox = TRUE, digits = 10)
  
  # Post the request to the API task service ----
  response <- POST(paste0(api_url, "task"), 
                   body = task_json, 
                   encode = "json", 
                   add_headers(Authorization = token, "Content-Type" = "application/json"))
  
  task_content = content(response)
  task_response = toJSON(task_content, auto_unbox = TRUE)
  task_id = fromJSON(task_response)$task_id
  message(paste0("this is your id: ", task_id))
  
  # return ----
  return(list(task_name = task_name, 
              task_id = task_id, 
              token = token))
  
}

# # test use fun
# user = getPass(msg = "Enter NASA Earthdata Login Username: ") # eurac classic
# password = getPass(msg = "Enter NASA Earthdata Login Password: ") # first big sport 
# 
# task_name = "mod13q1_ndvi_eusalp_2017"
# product = "MOD13Q1.006"
# layers = c("_250m_16_days_NDVI", "_250m_16_days_VI_Quality", "_250m_16_days_pixel_reliability")
# start_date = "01-01-2017"
# end_date = "31-12-2017"
# projection = "native"
# file_format = "geotiff"
# 
# pth_aoi = "/mnt/CEPH_PROJECTS/ADO/VHI/01_input_data/target_grid/eusalp_laea_231m.tif"
# aoi = read_stars(pth_aoi)
# aoi = st_transform(aoi, crs = st_crs(4326))
# aoi = st_as_sf(st_as_sfc(st_bbox(aoi)))
# 
# request = request_appeears(task_name = task_name, 
#                            product = product, 
#                            layers = layers, 
#                            start_date = start_date, 
#                            end_date = end_date, 
#                            aoi = aoi, 
#                            projection = projection, 
#                            file_format = file_format, 
#                            #task_type = "area", 
#                            user = user, 
#                            password = password, 
#                            #api_url = 'https://lpdaacsvc.cr.usgs.gov/appeears/api/'
# )
# 


# download_job ----
# def
# it downloads a request you have created via request_appeears -> use the task_id
# it saves to outdir
# you need the token created by login_appeears
# it returns the filepath

download_appeears = function(token, 
                             task_id, 
                             out_dir, 
                             api_url = 'https://lpdaacsvc.cr.usgs.gov/appeears/api/'){
  # create bundle (file list)
  bundle <- content(GET(paste0(api_url, "bundle/", task_id), 
                        add_headers(Authorization = token)))
  bundle <- prettify(toJSON(bundle, auto_unbox = TRUE))
  bundle <- fromJSON(bundle)$files
  message(paste0("Downloading n files: ", nrow(bundle)))
  
  # download bundle
  for (i in 1:nrow(bundle)){
    id = bundle$file_id[i]
    filename = bundle$file_name[i]
    message(paste0(Sys.time(), " | downloading: ", 
                   filename, ": ", i, " of ", length(bundle$file_id)))
    
    # create a destination directory to store the file in
    filepath <- paste(out_dir,filename, sep = "/")
    suppressWarnings(dir.create(dirname(filepath)))
    # write the file to disk using the destination directory and file name 
    response <- GET(paste0(api_url, "bundle/", task_id, "/", id), 
                    write_disk(filepath, overwrite = TRUE), progress(),
                    add_headers(Authorization = token))
  }
  
  return(filepath)
}

# # test
# out_dir = file.path("/mnt/CEPH_PROJECTS/ADO/VHI/01_input_data/MOD13Q1/")
# 
# files_download = download_appeears(token = token, 
#                                    task_id = request$task_id, 
#                                    out_dir = out_dir)


