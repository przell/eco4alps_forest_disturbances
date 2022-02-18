# Refine bfast results with Hansen Forest change layer
#
# - mask pixels with hansen forest change layer after bfast run
# - evaluate if the accuracy gets better
# - if yes: incorporate in workflow
#

# libs -------------------------------------------------------------------------
library(stars)
library(sf)
library(mapview)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# hansen forest change layer ---------------------------------------------------
# based on forest cover 2000. 
# prepared in conv_foresttype_to_binary.R:
# no change 0 -> 1
# changes 2001-2017 -> 1
# changes 2018-2020 -< 2018-2020
# https://data.globalforestwatch.org/documents/134f92e59f344549947a3eade9d80783/explore
pth_fc = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/Hansen_GFC-2020-v1.8_lossyear_50N_010E_ndvi_mask.tif"
fc = read_stars(pth_fc)

# bfats result layer -------------------------------------------------------------
pth_res = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast"
fls_brks = list.files(pth_res, pattern = "brks_", full.names = TRUE)
fls_magn= list.files(pth_res, pattern = "magn_", full.names = TRUE)
fls_val_obs = list.files(pth_res, pattern = "val_obs_", full.names = TRUE)

fls_in = list(brks = fls_brks, magn = fls_magn, val_obs = fls_val_obs)


# add start history and start monitor from filename ----------------------------
fls_in = lapply(fls_in, function(x){
  # get this form expand_filelist()
  start_history = gsub(pattern = ".*_([2][0][0-9][0-9])_20[0-9][0-9]_start.*", replacement = "\\1", x = basename(x))
  start_monitor = gsub(pattern = ".*start_([2][0][0-9][0-9])_level.*", replacement = "\\1", x = basename(x))
  df = data.frame(pth_in = x, start_history = start_history, start_monitor = start_monitor)
})

# mask bfast results according to start history and start monitoring -----------
# read bfast brks
x = fls_in[[1]][1, ]
brks = read_stars(x$pth_in)

# read bfast magnitude
y = fls_in[[2]][1, ]
magn = read_stars(y$pth_in)

# convert forest change to binary mask according to start_monitor --------------
# only pixels where forest change occured
fc_msk_change = fc
fc_msk_change[[1]][fc_msk_change[[1]] <= x$start_monitor] = NA
fc_msk_change[[1]][fc_msk_change[[1]] > x$start_monitor] = 1

# only pixels where no forest change occured
fc_msk_nochange = fc
fc_msk_nochange[[1]][fc_msk_nochange[[1]] <= x$start_monitor] = 1
fc_msk_nochange[[1]][fc_msk_nochange[[1]] > x$start_monitor] = NA

# hansen forest change from 2018
fc_2018 = fc
fc_2018[[1]][fc_2018[[1]] < x$start_monitor] = NA


# ---------------------------------------------------------------------------- #
# Evaluate magnitude based on change no change by HANSEN to extract threshold ----
# ---------------------------------------------------------------------------- #

# split magn into areas where change happened and where not according to hansen ----
magn_change = magn*fc_msk_change
magn_nochange = magn*fc_msk_nochange

# plot distribution of magnitude in change and no change areas -----------------
magn_df = data.frame(
  rbind(data.frame(category = "change", magn = c(magn_change[[1]])),
        data.frame(category = "no_change", magn = c(magn_nochange[[1]]))
  )
)

ggplot(data = magn_df, mapping = aes(x = magn, fill = category)) +
  geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") + 
  geom_density(alpha=0.7) + 
  geom_vline(aes(xintercept=mean(magn)), color="black", linetype="dashed", size=1) +
  ggtitle("Histogram of bfast magnitude", 
          subtitle =  "change/no-change areas according to HANSEN")

ggplot(data = magn_df, mapping = aes(x = magn, y = category, fill = category)) +
  geom_boxplot() + 
  coord_flip() + 
  ggtitle("Boxplot of bfast magnitude", 
          subtitle =  "change/no-change areas according to HANSEN")


# ---------------------------------------------------------------------------- #
# Evaluate magnitude based on change/no-change by BFAST BREAKS to extract threshold ----
# ---------------------------------------------------------------------------- #
# check if this would also work without hansen... of course not so good
# ... since we are trying to find a threshold to seperate the detections in 
# correct and false positives

# create a change/no change mask from the brks retrieved from bfast ------------
# only pixels where forest change occurred
brks_msk_change = brks
brks_msk_change[[1]][brks_msk_change[[1]] == 0] = NA
brks_msk_change[[1]][!is.na(brks_msk_change[[1]])] = 1

# only pixels where no forest change occurred
brks_msk_nochange = brks_msk_change
brks_msk_nochange[[1]][is.na(brks_msk_nochange[[1]])] = 0
brks_msk_nochange[[1]][brks_msk_nochange[[1]] == 1] = NA
brks_msk_nochange[[1]][brks_msk_nochange[[1]] == 0] = 1

magn_change_bfast = magn*brks_msk_change
magn_nochange_bfast = magn*brks_msk_nochange

mapview(magn_change_bfast, at = c(-1 , -0.5, -0.375, -0.25, -0.125, 0, 1))

mapview(fc_msk_change) + 
  mapview(magn_change_bfast, at = c(-1 , -0.5, -0.375, -0.25, -0.125, 0, 1)) +
  mapview(magn_change)

magn_df_bfast = data.frame(
  rbind(data.frame(category = "change", magn = c(magn_change_bfast[[1]])),
        data.frame(category = "no_change", magn = c(magn_nochange_bfast[[1]]))
  )
)

ggplot(data = magn_df_bfast, mapping = aes(x = magn, fill = category)) +
  geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") + 
  geom_density(alpha=0.7) + 
  geom_vline(aes(xintercept=mean(magn)), color="black", linetype="dashed", size=1) +
  ggtitle("Histogram of bfast magnitude", 
          subtitle =  "change/no-change areas according to bfast brks")

ggplot(data = magn_df_bfast, mapping = aes(x = magn, y = category, fill = category)) +
  geom_boxplot() + coord_flip() + ggtitle("Boxplot of bfast magnitude", 
                                          subtitle =  "change/no-change areas according to bfast brks")

# filter breaks according to magnitude -----------------------------------------
magn_msk_change = magn
magn_msk_change[[1]][magn_msk_change[[1]] >= -0.2] = NA
magn_msk_change[[1]][magn_msk_change[[1]] < -0.2] = 1
plot(magn_msk_change)

brks_change_magn = brks * magn_msk_change
brks_change_magn[[1]][brks_change_magn[[1]] < 2000] = NA
plot(brks_change_magn)

mapview(brks_change_magn) + mapview(fc_2018)


# save result ------------------------------------------------------------------
# pth_out = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast/refine_hansen/"


# run reporting!

# offtopic...
# compare magnitude in reports



