library(walking)
library(tidyverse)
library(readr)
options(digits.secs = 3)

source(here::here("code/R/utils.R"))

clemson_files = list.files(here::here("data", "reorganized",
                                      "clemson"),
                           recursive = TRUE,
                           full.names = TRUE)

oxwalk_files = list.files(here::here("data", "reorganized",
                                                     "oxwalk"),
                                          recursive = TRUE,
                                          full.names = TRUE)

marea_files = list.files(here::here("data", "reorganized",
                                     "marea"),
                          recursive = TRUE,
                          full.names = TRUE)
# just get the raw files
clemson_files = clemson_files[grepl("step_estimates", clemson_files) == FALSE]
oxwalk_files = oxwalk_files[grepl("step_estimates", oxwalk_files) == FALSE]
marea_files = marea_files[grepl("step_estimates", marea_files) == FALSE]


map(c(clemson_files, oxwalk_files, marea_files),
    .f = function(x){
      df = readr::read_csv(x)
      if(grepl("clemson", x) == TRUE){
        study = "clemson"
        id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", x)
        fname_root = sub(".*\\/(.+).csv.gz.*", "\\1", x)
        fname_root_new = paste0(fname_root, "-steps_")
      }
      if(grepl("oxwalk", x) == TRUE){
        study = "oxwalk"
        id = sub(".*oxwalk\\-(.+)-r.*", "\\1", x)
        fname_root =   sub(".*\\/(.+).csv.gz.*", "\\1", x)
        fname_root_new = paste0(fname_root, "-steps_")
      }
      if(grepl("marea", x) == TRUE){
        study = "marea"
        id =  regmatches(x, gregexpr("(?<=marea\\-)[a-zA-Z0-9]{3}", x, perl = TRUE))[[1]][1]
        fname_root =   sub(".*\\/(.+).csv.gz.*", "\\1", x)
        fname_root_new = paste0(fname_root, "-steps_")
      }
      if (!"HEADER_TIME_STAMP" %in% colnames(df)) {
        df = df %>%
          rename(HEADER_TIME_STAMP = tm_dttm)
      }
      srate = df$sample_rate[1]

      # create directory if doesn't already exist
      if (!file.exists(here::here("data", "reorganized", study, id, "step_estimates"))) {
        dir.create(here::here("data", "reorganized", study, id, "step_estimates"))
      }
      # check if files exist, then fit algorithms if they don't
      if(!file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                 paste0(fname_root_new, "oak.csv")))){
        oak = fit_oak(df)
        readr::write_csv(oak, here::here("data", "reorganized", study, id, "step_estimates",
                                         paste0(fname_root_new, "oak.csv")))
      }
      if(!file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                 paste0(fname_root_new, "adept.csv")))){
        adept = fit_adept(df, sample_rate = srate)
        readr::write_csv(adept, here::here("data", "reorganized", study, id, "step_estimates",
                                         paste0(fname_root_new, "adept.csv")))
      }
      if(!file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                 paste0(fname_root_new, "sdt.csv")))){
        sdt = fit_sdt(df, sample_rate = srate)
        readr::write_csv(sdt, here::here("data", "reorganized", study, id, "step_estimates",
                                         paste0(fname_root_new, "sdt.csv")))
      }
      if(!file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                 paste0(fname_root_new, "vs.csv")))){
        vs = fit_vs(df, sample_rate = srate)
        readr::write_csv(vs, here::here("data", "reorganized", study, id, "step_estimates",
                                         paste0(fname_root_new, "vs.csv")))
      }
      if(grepl("resampled", x) == FALSE &
         !file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                       paste0(fname_root_new, "truth.csv")))){
        truth = get_truth(df)
        readr::write_csv(truth, here::here("data", "reorganized", study, id, "step_estimates",
                                           paste0(fname_root_new, "truth.csv")))
      }
      message(paste0(fname_root, " finished"))
})
