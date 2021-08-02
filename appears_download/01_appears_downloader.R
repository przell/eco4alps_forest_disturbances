library("httr")
library("jsonlite")

# # set your params here ---------------------------------------------------------
# out_dir = file.path("/mnt/CEPH_PROJECTS/ADO/VHI/01_input_data/MOD13Q1/")
# api_url = 'https://lpdaacsvc.cr.usgs.gov/appeears/api/'
# task_id = "4ee1959f-b7f8-4855-b43e-6025cddbdffa"
# token = "Bearer FeBOy0MBhrt4xD3HzkIHW-KVCdPM0zrTXFmZFdmsZTjue9N9_sKV-Wn2OP455EGUeSpX7_C4cdp1fIxDksWbcA"
# 
# # get file list from api -------------------------------------------------------
# bundle <- content(GET(paste0(api_url, "bundle/", task_id), 
#                         add_headers(Authorization = token)))
# bundle <- prettify(toJSON(bundle, auto_unbox = TRUE))
# bundle <- fromJSON(bundle)$files
# table(dirname(bundle$file_name))
# 
# #bundle = bundle[750:nrow(bundle), ]
# 
# # download ---------------------------------------------------------------------
# 
# for (id in bundle$file_id){
#   message(paste0("- downloading id: ", id, " of ", length(bundle$file_id)))
#   message(Sys.time())
#   # retrieve the filename from the file_id
#   filename <- bundle[bundle$file_id == id,]$file_name           
#   # create a destination directory to store the file in
#   filepath <- paste(out_dir,filename, sep = "/")
#   suppressWarnings(dir.create(dirname(filepath)))
#   # write the file to disk using the destination directory and file name 
#   response <- GET(paste0(api_url, "bundle/", task_id, "/", id), 
#                   write_disk(filepath, overwrite = TRUE), progress(),
#                   add_headers(Authorization = token))
# }



# test new fun -------

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

# test
out_dir = file.path("/mnt/CEPH_PROJECTS/ADO/VHI/01_input_data/MOD13Q1/")
user = "pzellner"
password = "Soccer88"
task_id = "cc7de6dc-b10d-4ce8-8ecc-f3e951e16bf5"

token = login_appeears(user = user, password = password)

files_download = download_appeears(token = token, 
                                   task_id = task_id, # request$task_id, 
                                   out_dir = out_dir)




















