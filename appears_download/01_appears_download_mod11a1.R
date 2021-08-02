# download mod11a1 2000 - 2020 via appears

# resources ----
# API Documentation
# https://lpdaacsvc.cr.usgs.gov/appeears/api/?_ga=2.267648142.1035835492.1616403797-413231647.1616403797
# R code examples to use API
# https://git.earthdata.nasa.gov/projects/LPDUR/repos/appeears-api-getting-started_r/browse

# libs -------------------------------------------------------------------------
library(getPass)           # A micro-package for reading passwords
library(httr)              # To send a request to the server/receive a response from the server
library(jsonlite)          # Implements a bidirectional mapping between JSON data and the most important R data types
library(geojsonio)         # Convert data from various R classes to 'GeoJSON' 
library(geojsonR)          # Functions for processing GeoJSON objects
library(sf)
library(stars)

# login ------------------------------------------------------------------------
api_url = 'https://lpdaacsvc.cr.usgs.gov/appeears/api/'
user = getPass(msg = "Enter NASA Earthdata Login Username: ") # eurac classic
password = getPass(msg = "Enter NASA Earthdata Login Password: ") # first big sport 
secret = jsonlite::base64_enc(paste(user, password, sep = ":")) # Encode the string of username and password

# Insert API URL, call login service, set the component of HTTP header, and post the request to the server
response <- httr::POST(paste0(api_url, "login"), 
                       add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                                   "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                       body = "grant_type=client_credentials")

response_content <- content(response)                          # Retrieve the content of the request
token_response <- toJSON(response_content, auto_unbox = TRUE)  # Convert the response to the JSON object
remove(user, password, secret, response)                       # Remove the variables that are not needed anymore 
prettify(token_response)                                       # Print the prettified response

# ---------------------------------------------------------------------------- #
# Define API call ----
# ---------------------------------------------------------------------------- #

# define your aoi as geojson --------------------------------------------------
# get the bbox of the grid defined here: 00_define_grid.R
pth_refgrid = "/mnt/CEPH_PROJECTS/ADO/VHI/01_input_data/target_grid/eusalp_laea_231m.tif"
refgrid = read_stars(pth_refgrid)
refgrid = st_transform(refgrid, crs = st_crs(4326))
aoi = st_as_sf(st_as_sfc(st_bbox(refgrid)))

aoi <- geojsonsf::sf_geojson(aoi, simplify = FALSE) # to geojson
# prettify(aoi)
# class(aoi)
aoi = geojsonR::FROM_GeoJson(aoi) # to list for request
aoi$features[[1]]$geometry$coordinates <- list(aoi$features[[1]]$geometry$coordinates) # change for request struct

# setup request to api ---------------------------------------------------------
# example request: 
# https://lpdaacsvc.cr.usgs.gov/appeears/api/examples/sample_area_request.json

# here you have to set some items:

# task description ----
task_type = "area"
task_name = "mod11a1_lst_eusalp_2000_2010" # param
task_desc = list(task_type = task_type, 
                 task_name = task_name)

# parameter dates ----
start_date = "01-01-2000" # param
end_date = "31-12-2010" # param
dates = data.frame(startDate = start_date, 
                   endDate = end_date
)
task_dates = list(dates = dates)

# parameter layers ----
layers <- c("LST_Day_1km", "QC_Day")     
prods <- c("MOD11A1.006", "MOD11A1.006") 
layers = data.frame(product = prods, layer = layers)
task_layers = list(layers = layers)

# parameter output ----
projection = "native"
file_format = "geotiff"
output = list(projection = projection)
output$format$type <- file_format
task_output = list(output = output)

# parameter geo (has to be valid geojson obj) ----
task_geo = list(geo = aoi)

# combine parameters ----
task_params = list(params = c(task_dates, task_layers, task_output, task_geo))

# combine complete task ----
task = c(task_desc, task_params)

# convert task to json ----
task_json <- jsonlite::toJSON(task, auto_unbox = TRUE, digits = 10)
prettify(task_json)

# outdir ------------------------------------------------------------------------
out_dir = file.path("/mnt/CEPH_PROJECTS/ADO/VHI/01_input_data/MOD11A1/")

# ---------------------------------------------------------------------------- #
# Submit task ----
# ---------------------------------------------------------------------------- #

# submit task ------------------------------------------------------------------
token <- paste("Bearer", fromJSON(token_response)$token)     # Save login token to a variable

# Post the request to the API task service
response <- POST(paste0(api_url, "task"), 
                 body = task_json, 
                 encode = "json", 
                 add_headers(Authorization = token, "Content-Type" = "application/json"))

task_content <- content(response)                           # Retrieve content of the request 
task_response <- toJSON(task_content, auto_unbox = TRUE)    # Convert the content to JSON and prettify it
prettify(task_response)                                     # Print the task response
task_id = fromJSON(task_response)$task_id

# task status ------------------------------------------------------------------
params <- list(limit = 2, pretty = TRUE)                    # Set up query parameters
status_req <- GET(paste0(api_url,"task/", task_id), 
                  add_headers(Authorization = token))
status_content <- content(status_req)                       # Retrieve content of the request
statusResponse <-toJSON(status_content, auto_unbox = TRUE)  # Convert the content to JSON object
remove(status_req, status_content)                          # Remove the variables that are not needed
prettify(statusResponse)                                    # Print the prettified response

# get only status (repeat status request until status is done)
fromJSON(statusResponse)$status


# ---------------------------------------------------------------------------- #
# Download results ----
# ---------------------------------------------------------------------------- #

# check files that are in request ----------------------------------------------
task_id
response <- GET(paste0(api_url, "bundle/", task_id), add_headers(Authorization = token))
bundle_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
bundle_response

# download ---------------------------------------------------------------------
bundle <- fromJSON(bundle_response)$files
for (id in bundle$file_id){
  message(paste0("- downloading id: ", id, " of ", length(bundle$file_id)))
  message(Sys.time())
  # retrieve the filename from the file_id
  filename <- bundle[bundle$file_id == id,]$file_name           
  # create a destination directory to store the file in
  filepath <- paste(out_dir,filename, sep = "/")
  suppressWarnings(dir.create(dirname(filepath)))
  # write the file to disk using the destination directory and file name 
  response <- GET(paste0(api_url, "bundle/", task_id, "/", id), 
                  write_disk(filepath, overwrite = TRUE), progress(),
                  add_headers(Authorization = token))
}





