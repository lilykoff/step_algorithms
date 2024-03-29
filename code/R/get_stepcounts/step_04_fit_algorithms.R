# fit all walking algorithms and return steps at the second level for all algorithms except for
# stepcount (bash script) and actilife (need to use actilife software)

library(walking)
library(tidyverse)
library(readr)
options(digits.secs = 3)

source(here::here("code/R/utils.R"))

# system("conda activate stepcount")
# system(here::here("code", "bash", "step_4_fit_stepcount.sh"))
## COME BACK TO THIS

# get raw/resampled files
clemson_files = list.files(here::here("data", "reorganized",
                                      "clemson"),
                           recursive = TRUE,
                           full.names = TRUE,
                           pattern = ".*.csv.gz")

marea_files = list.files(here::here("data", "reorganized",
                                     "marea"),
                          recursive = TRUE,
                          full.names = TRUE,
                         pattern = ".*.csv.gz")

oxwalk_files = list.files(here::here("data", "reorganized",
                                     "oxwalk"),
                          recursive = TRUE,
                          full.names = TRUE,
                          pattern = ".*.csv.gz")

if(length(marea_files) > 0){
  map(c(clemson_files, oxwalk_files, marea_files),
      .f = function(x){
        df = readr::read_csv(x)
        # determine which study data come from
        if(grepl("clemson", x)){
          study = "clemson"
          id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", x)
          fname_root = sub(".*\\/(.+).csv.gz.*", "\\1", x)
          fname_root_new = paste0(fname_root, "-steps_")
        }
        if(grepl("oxwalk", x)){
          study = "oxwalk"
          id = sub(".*oxwalk\\-(.+)-r.*", "\\1", x)
          fname_root =   sub(".*\\/(.+).csv.gz.*", "\\1", x)
          fname_root_new = paste0(fname_root, "-steps_")
        }
        if(grepl("marea", x)){
          study = "marea"
          id =  regmatches(x, gregexpr("(?<=marea\\-)[a-zA-Z0-9]{3}", x, perl = TRUE))[[1]][1]
          fname_root =   sub(".*\\/(.+).csv.gz.*", "\\1", x)
          fname_root_new = paste0(fname_root, "-steps_")
        }
        # rename `tm_dttm` column to be compatible with algorithms
        if (!"HEADER_TIMESTAMP" %in% colnames(df)) {
          df = df %>%
            rename(HEADER_TIMESTAMP = tm_dttm)
        }
        # get sample rate
        srate = df$sample_rate[1]

        # create directory to store step estimates if doesn't already exist
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
        if(file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                   paste0(fname_root_new, "vs.csv")))){
          vso_raw = fit_vs(df, sample_rate = srate, method_type = "original", resample = FALSE) %>%
             rename(steps_vsoraw = steps_vs)
          vso_res = fit_vs(df, sample_rate = srate, method_type = "original", resample = TRUE) %>%
            rename(steps_vsores = steps_vs)
          vsr_raw = fit_vs(df, sample_rate = srate, method_type = "revised", resample = FALSE) %>%
            rename(steps_vsrraw = steps_vs)
          vsr_res = fit_vs(df, method_type = "revised", sample_rate = srate, resample = TRUE) %>%
            rename(steps_vsrres = steps_vs)
          vs = left_join(vso_raw, vsr_raw, by = "time") %>%
            left_join(vso_res, by = "time") %>%
            left_join(vsr_res, by = "time")
          readr::write_csv(vs, here::here("data", "reorganized", study, id, "step_estimates",
                                          paste0(fname_root_new, "vs.csv")))
        }

        # if(!file.exists(here::here("data", "reorganized", study, id, "step_estimates",
        #                            paste0(fname_root_new, "vs.csv")))){
        #   vs = fit_vs(df, sample_rate = srate)
        #   readr::write_csv(vs, here::here("data", "reorganized", study, id, "step_estimates",
        #                                   paste0(fname_root_new, "vs.csv")))
        # }

        # if not resampled data, write truth
        if(!grepl("resampled", x) &
           !file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                   paste0(fname_root_new, "truth.csv")))){
          truth = get_truth(df)
          readr::write_csv(truth, here::here("data", "reorganized", study, id, "step_estimates",
                                             paste0(fname_root_new, "truth.csv")))
        }
        message(paste0(fname_root, " finished"))
      })
} else {
  map(c(clemson_files, oxwalk_files),
        .f = function(x){
          df = readr::read_csv(x)
          # determine which study data come from
          if(grepl("clemson", x)){
            study = "clemson"
            id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", x)
            fname_root = sub(".*\\/(.+).csv.gz.*", "\\1", x)
            fname_root_new = paste0(fname_root, "-steps_")
          }
          if(grepl("oxwalk", x)){
            study = "oxwalk"
            id = sub(".*oxwalk\\-(.+)-r.*", "\\1", x)
            fname_root =   sub(".*\\/(.+).csv.gz.*", "\\1", x)
            fname_root_new = paste0(fname_root, "-steps_")
          }
          if(grepl("marea", x)){
            study = "marea"
            id =  regmatches(x, gregexpr("(?<=marea\\-)[a-zA-Z0-9]{3}", x, perl = TRUE))[[1]][1]
            fname_root =   sub(".*\\/(.+).csv.gz.*", "\\1", x)
            fname_root_new = paste0(fname_root, "-steps_")
          }
          # rename `tm_dttm` column to be compatible with algorithms
          if (!"HEADER_TIME_STAMP" %in% colnames(df)) {
            df = df %>%
              rename(HEADER_TIME_STAMP = tm_dttm)
          }
          # get sample rate
          srate = df$sample_rate[1]

          # create directory to store step estimates if doesn't already exist
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
          if(file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                    paste0(fname_root_new, "vs.csv")))){
            vso_raw = fit_vs(df, sample_rate = srate, method_type = "original", resample = FALSE) %>%
              rename(steps_vsoraw = steps_vs)
            vso_res = fit_vs(df, sample_rate = srate, method_type = "original", resample = TRUE) %>%
              rename(steps_vsores = steps_vs)
            vsr_raw = fit_vs(df, sample_rate = srate, method_type = "revised", resample = FALSE) %>%
              rename(steps_vsrraw = steps_vs)
            vsr_res = fit_vs(df, method_type = "revised", sample_rate = srate, resample = TRUE) %>%
              rename(steps_vsrres = steps_vs)
            vs = left_join(vso_raw, vsr_raw, by = "time") %>%
              left_join(vso_res, by = "time") %>%
              left_join(vsr_res, by = "time")
            readr::write_csv(vs, here::here("data", "reorganized", study, id, "step_estimates",
                                            paste0(fname_root_new, "vs.csv")))
          }
          # if(!file.exists(here::here("data", "reorganized", study, id, "step_estimates",
          #                            paste0(fname_root_new, "vs.csv")))){
          #   vs = fit_vs(df, sample_rate = srate)
          #   readr::write_csv(vs, here::here("data", "reorganized", study, id, "step_estimates",
          #                                   paste0(fname_root_new, "vs.csv")))
          # }
          # if not resampled data, write truth
          if(!grepl("resampled", x) &
             !file.exists(here::here("data", "reorganized", study, id, "step_estimates",
                                     paste0(fname_root_new, "truth.csv")))){
            truth = get_truth(df)
            readr::write_csv(truth, here::here("data", "reorganized", study, id, "step_estimates",
                                               paste0(fname_root_new, "truth.csv")))
          }
          message(paste0(fname_root, " finished"))
        })
}

