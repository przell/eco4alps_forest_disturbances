# Refine bfast results with Hansen Forest change layer
#
# - mask pixels with hansen forest change layer after bfast run
# - evaluate if the accuracy gets better
# - if yes: incorporate in workflow
#

# libs -------------------------------------------------------------------------



# hansen forest change layer ---------------------------------------------------
# based on forest cover 2000. Then loss for every year. 1 = 2001 etc.
# https://data.globalforestwatch.org/documents/134f92e59f344549947a3eade9d80783/explore
path_fc = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/Hansen_GFC-2020-v1.8_lossyear_50N_010E_ndvi_mask.tif"
fc = read_stars(path_fc)

# bfats result layer -------------------------------------------------------------
pth_res = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast"
fls_brks = list.files(pth_res, pattern = "brks_", full.names = TRUE)
fls_magn= list.files(pth_res, pattern = "magn_", full.names = TRUE)
fls_val_obs = list.files(pth_res, pattern = "val_obs_", full.names = TRUE)

fls_in = list(brks = fls_brks, magn = fls_magn, val_obs = fls_val_obs)


# add start history and start monitor from filename ----------------------------
fls_in = lapply(fls_in, function(x){
  # get this form expand_filelist()
  start_history = gsub(pattern = ".*_([2][0][1-9][1-9])_20[1-9][1-9]_.*", replacement = "//1", x = x)
  #start_monitor
})



# mask bfast results according to start history and start monitoring -----------


# save result ------------------------------------------------------------------
pth_out = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast/refine_hansen/"


# run reporting!

# offtopic...
# compare magnitude in reports



