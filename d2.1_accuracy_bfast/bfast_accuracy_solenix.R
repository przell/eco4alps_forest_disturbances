# compare local results to slx results

# libs
library(stars)
library(sf)
library(mapview)
library(dplyr)
library(stringr)

# slx
pth_slx = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances_SLX/20220419-forest-disturbance-validation1/"
fls_slx = list.files(pth_slx, pattern = "brks_", full.names = TRUE)
brks_slx = stars::read_stars(fls_slx[1])

# local
pth_loc = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast"
fls_loc = list.files(pth_loc, pattern = "brks_", full.names = TRUE)
brks_loc = stars::read_stars(fls_loc[5]) # choose manually what fits to what comes from solenix

# compare
brks_slx 
brks_loc

plot(brks_slx) # masking different?
plot(brks_loc)  

hist(brks_slx)
hist(brks_loc)

mapview(st_bbox(brks_slx), col.reg = "red") + mapview(st_bbox(brks_loc))

brks_slx[st_bbox(brks_loc)]-brks_loc

st_crs(brks_loc)
st_crs(brks_slx)
