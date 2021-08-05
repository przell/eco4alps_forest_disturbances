# bfast accuracy reporting

# the brks from bfast come like this:
# - detected_break = decimal year (2018.5)
# - no_break = 0
# - no_forest = NA

# bfast reporter

# input 
# bfast results (brks, val_obs, magn)
# forest mask
# vaja storm damage shps
# control shps

# workflow 
# loop through damage shps 
# (loop through control shps)

# indicators
# count total pixels = forest pixels in aoi
# count total observation dates
# count valid observations (per year, per season) -> boxplot
# count snow, cloud pixels (from mask)
# get ndvi ts for aoi mean
# get bfast breakpoints for aoi
# % before
# % break_lag: 1, 2, 3, 6 , >6 months
# % no_break
# get magnitude 

# later in project checks:
# winter
# walddichte 2-3 klassen
# datendichte anzahl der punkte
# spatial resolution 3x3 vs pixel
# newer events then 2018
# parameters (h, magnitude) to not identify false positives
# select 10 clean disturbance events and 10 clean non-disturbance patches
# -> add aggregated patch stats 
# -> analyse on patch basis also with ndvi time series and breakpoint

# libs ------------------------------------------------------------------------
library(stars)
library(sf)
library(mapview)
library(dplyr)
library(stringr)
library(parallel)
library(bfast)
library(pbapply)
library(lubridate)
library(xts)
library(zoo)
library(forecast)
library(ggplot2)
library("rmarkdown")


# data ------------------------------------------------------------------------
pth_res = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/bfast"
fls_brks = list.files(pth_res, pattern = "brks_", full.names = TRUE)

# magnitude and valid obs not analysed so far
# pth_magn = paste0(pth_res, "magn.tif")
# pth_val_obs = paste0(pth_res, "val_obs.tif")

pth_wind = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/01_data/01_reference_data/area_32632.shp"
wind = st_read(pth_wind)


# function analyser ------------------------------------------------------------
# function to analyse breakpoint of bfastmonitor output stars object

analyse_brks = function(brks, event_date = NULL){
  # get total count and no_brk count
  cnt = list(
    cnt_total = length(brks[[1]]),
    cnt_forest = sum(!is.na(brks[[1]])),
    cnt_noforest = sum(is.na(brks[[1]])),
    cnt_brk = sum(brks[[1]] > 0, na.rm = T),
    cnt_no_brk = sum(brks[[1]] == 0, na.rm = T)
    )
  
  # test
  stopifnot(cnt$cnt_forest + cnt$cnt_noforest == cnt$cnt_total)
  stopifnot(cnt$cnt_brk + cnt$cnt_no_brk == cnt$cnt_forest)
  
  # make break no break table for analysing areas without disturbance
  brk_table = data.frame(cls = c("brk", "no_brk"), 
                         freq = c(cnt$cnt_brk, cnt$cnt_no_brk))
  brk_table$freq_perc = brk_table$freq / sum(brk_table$freq) * 100
  
  plt_brk_pie = ggplot(brk_table, aes(x="", y=freq_perc, fill=cls)) +
    geom_bar(width = 1, stat = "identity", color = "black", size = 0.25) +
    coord_polar("y", start=0) 
  
  # scale_fill_brewer(palette="RdYlGn", direction = -1) 
  # scale_fill_manual(values = c("#d73027", "#1a9850"))
  
  # set no_break to na to not interfere with further analysis
  brks[[1]][brks[[1]] == 0] = NA 
  
  # pull data
  brks_v = as.vector(brks[[1]])
  
  # quantiles
  quantiles = date_decimal(quantile(brks_v, na.rm = T)) %>% as.Date()
  
  # hist(brks_v)
  yrs = sort(unique(round(na.omit(brks_v),0)))
  n_yrs = length(yrs)
  plt_hist = ggplot(data.frame(brks = brks_v), aes(x=brks)) + 
    geom_histogram(color="black", fill="white", 
                   breaks=seq(from = min(yrs), to = max(yrs), by= 0.25))
  
  # boxplot(brks_v)
  plt_boxplot = ggplot(data.frame(brks = brks_v), aes(x = "1", y = brks)) + 
    geom_boxplot() + 
    stat_summary(geom="text", fun=quantile,
                 aes(label=sprintf("%1.1f", ..y..)),
                 position=position_nudge(x=0.5), size=3.5) 
  
  # assemble results in list
  res = list(
    "cnt" = cnt, 
    "brk_table" = brk_table, 
    "plt_brk_pie" = plt_brk_pie,
    "quantiles" = quantiles, 
    "plt_hist" = plt_hist, 
    "plt_boxplot" = plt_boxplot
  )
  
  # event part only if event is specified
  if(!is.null(event_date)){
    event_date = as.Date(event_date)
    min_brk = min(brks_v, na.rm = T) %>% date_decimal() %>% as.Date()
    max_brk = max(brks_v, na.rm = T) %>% date_decimal() %>% as.Date()
    event_date = as.Date(event_date)
    date_cls = c(min_brk-1,
                 event_date, 
                 event_date+31, 
                 event_date+31*2, 
                 event_date+31*3, 
                 event_date+31*6, 
                 event_date+31*12, 
                 max_brk+1)
    date_cls_dec = decimal_date(date_cls)
    date_labels = c("before", "1mnth", "2mnth", "3mnth", "6mnth", "12mnth", ">12mnth")
    
    cls = cut(brks_v, breaks = date_cls_dec, labels = c(date_labels))
    acc_table = as.data.frame(table(cls), stringsAsFactors = FALSE)
    acc_table$date_cls = date_cls[-1]
    acc_table$date_cls_dec = date_cls_dec[-1]
    acc_table = rbind(acc_table, 
                      data.frame(cls = "no_break", Freq = cnt$cnt_no_brk, 
                                 date_cls = NA, date_cls_dec = NA))
    
    stopifnot(sum(acc_table$Freq) == cnt$cnt_forest)
    
    acc_table = acc_table %>% 
      mutate(freq_perc = Freq/sum(Freq)*100) %>% 
      rename(freq = Freq)
    
    acc_table$cls = factor(acc_table$cls, 
                           levels = c("1mnth", "2mnth", "3mnth", "6mnth", 
                                      "12mnth", ">12mnth", "no_break", "before"), 
                           ordered = TRUE)
    
    plt_acc_pie = ggplot(acc_table, aes(x="", y=freq_perc, fill=cls)) +
      geom_bar(width = 1, stat = "identity", color = "black", size = 0.25) +
      coord_polar("y", start=0) + 
      scale_fill_brewer(palette="RdYlGn", direction = -1)
      
    
    # add vline to hist where event is
    event_date_dec = decimal_date(event_date)
    plt_hist = plt_hist + geom_vline(xintercept = event_date_dec, color = "red")
    
    # make special hist with brakes as defined in acc_table
    plt_hist_event = ggplot(data.frame(brks = brks_v), aes(x=brks)) + 
      geom_histogram(color="black", fill="white", 
                     breaks=date_cls_dec) +
      geom_vline(xintercept = event_date_dec, color = "red")
    
    # assemble in list
    res$plt_hist = plt_hist # overwrite old plot
    res = c(res, 
            list("event_table" = acc_table, 
                 "plt_pie_event" = plt_acc_pie,
                 "plt_hist_event" = plt_hist_event))
  }
  return(res)
}


# analysis wind event ----------------------------------------------------------
# pick bfast runs that have the vaja event in it: monitoring start in 2018
fls_brks_wind = fls_brks[grepl(pattern = "_2018_", x = fls_brks)]

# analyze areas where an event has been reported
lst_anal_wind = lapply(fls_brks_wind, function(x){
  message(paste0("at: ", x))
  brks = read_stars(x)
  # only keep pixels that are inside of an event site
  wind = st_transform(wind, crs = st_crs(brks))
  brks = brks[wind]
  # analyze
  anal_wind = analyse_brks(brks = brks, event_date = "2018-10-27")
  
})
names(lst_anal_wind) = basename(fls_brks_wind)

# add meta info from file name to list
meta = lapply(fls_brks_wind, function(x){
  x = basename(x)
  list(meta = list(input_file = x, 
                   start_period = substr(x, 6, 9),
                   stop_period = substr(x, 11,14), 
                   start_monitoring = substr(x, 22, 25), 
                   level = gsub(pattern = ".*level_(.+)\\.tif$", 
                                replacement = "\\1", x = x)))
})
names(meta) = names(lst_anal_wind)
lst_anal_wind = Map(c, lst_anal_wind, meta)

# save the list elements to rds that can be loaded by markdown
pth_report = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/acc_reports/"
lapply(names(lst_anal_wind), function(x){
  saveRDS(object = lst_anal_wind[[x]], 
          file = paste0(pth_report, "acc_event_", tools::file_path_sans_ext(x), ".RDS"))
})


# event report 
# template event.rmd
lapply(names(lst_anal_wind), function(x){
  rds_file = paste0(pth_report, "acc_event_", tools::file_path_sans_ext(x), ".RDS")
  render(input = "d2.1_accuracy_bfast/template_event.Rmd", 
         output_file = paste0(pth_report, "acc_event_", tools::file_path_sans_ext(x), ".html"),
         params = list(title = paste0("Accuracy Report - ", x), 
                       pth_data = rds_file))
})


# analysis no disturbance -----------------------------------------------------
# get values in no wind areas
fls_brks 

# analyze areas where no event was recorded (that we know of)
lst_anal = lapply(fls_brks, function(x){
  message(paste0("at: ", x))
  brks = read_stars(x)
  # cut out wind areas from bounding box, so that they are not analyzed
  wind = st_transform(wind, crs = st_crs(brks))
  no_wind = st_difference(x = st_as_sfc(st_bbox(wind)), y = st_union(wind))
  brks = brks[no_wind]
  # analyze
  anal = analyse_brks(brks = brks, event_date = NULL)
  
})
names(lst_anal) = basename(fls_brks)

# add meta info from file name to list
meta = lapply(fls_brks, function(x){
  x = basename(x)
  list(meta = list(input_file = x, 
                   start_period = substr(x, 6, 9),
                   stop_period = substr(x, 11,14), 
                   start_monitoring = substr(x, 22, 25), 
                   level = gsub(pattern = ".*level_(.+)\\.tif$", 
                                replacement = "\\1", x = x)))
})
names(meta) = names(lst_anal)
lst_anal = Map(c, lst_anal, meta)

# save the list elements to rds that can be loaded by markdown
pth_report = "/mnt/CEPH_PROJECTS/ECO4Alps/Forest_Disturbances/03_results/acc_reports/"
lapply(names(lst_anal), function(x){
  saveRDS(object = lst_anal[[x]], 
          file = paste0(pth_report, "acc_noevent_", tools::file_path_sans_ext(x), ".RDS"))
})


# non event report 
lapply(names(lst_anal), function(x){
  rds_file = paste0(pth_report, "acc_noevent_", tools::file_path_sans_ext(x), ".RDS")
  render(input = "d2.1_accuracy_bfast/template_noevent.Rmd", 
         output_file = paste0(pth_report, "acc_noevent_", tools::file_path_sans_ext(x), ".html"),
         params = list(title = paste0("Accuracy Report - ", x), 
                       pth_data = rds_file))
})



# to reduce the detections
# check: 
#  magnitude
#  level
#  influence of more years as history
#  run bfast mulitple times, if break is detected, run again and remove detection point?!

# repeat this yearly and actually only estimate for a year


