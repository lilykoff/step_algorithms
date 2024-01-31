library(tidyverse)
# HERE

# step six join and nest
# read in raw files, nest datetime, then join with steps
all_clem_files =  list.files(here::here("data", "reorganized", "clemson"), full.names = TRUE,
                             recursive = TRUE)
ids = sub(".*clemson\\-(.+)\\-walk.*", "\\1", all_clem_files) %>% unique()

all_clem =
  map(.x = ids,
    .f = function(id){
      if(!file.exists(here::here("data", "processed", "clemson", id))){
        dir.create(here::here("data", "processed", "clemson", id))
      }
      # get all files for that subject
      subj_files = list.files(here::here("data", "reorganized", "clemson", id),
                              full.names = TRUE,
                            recursive = TRUE)
      # get activities from that subject
      activities = paste0("walk_", sub(".*walk\\_(.+)\\-r.*", "\\1", subj_files)) %>% unique()
      # for ea activity read in step estimates and join with raw data; save
      map(.x = activities,
          .f = function(activity){
            sa_files = subj_files[grepl(activity, subj_files)] # get all files
            raw_file =  sa_files[grepl(".csv.gz", sa_files) & grepl("raw", sa_files)] # get raw file
            resamp_file = sa_files[grepl(".csv.gz", sa_files) & grepl("resampled", sa_files)]

            # read in raw data; nest
            raw_df =
              readr::read_csv(raw_file)  %>%
              mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
              tidyr::nest(raw_data = c(tm_dttm, X, Y, Z, ind_step, cat_step_type)) %>%
              select(-c(sample_rate))
            # read in resampled raw data; nest
            resampled_df =
              readr::read_csv(resamp_file) %>%
              mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
              tidyr::nest(resampled_data = c(tm_dttm, X, Y, Z)) %>%
              select(-c(sample_rate))

            # join together
            both =
              left_join(raw_df, resampled_df, by = c("time", "id_subject", "id_study", "cat_activity"))

            step_files = sa_files[grepl("steps", sa_files)]
            step_files_raw = step_files[grepl("raw", step_files)]
            step_files_resamp = step_files[grepl("resampled", step_files)]


            ad = readr::read_csv(step_files_raw[grepl("adept", step_files_raw)])
            oak = readr::read_csv(step_files_raw[grepl("oak", step_files_raw)])
            sdt = readr::read_csv(step_files_raw[grepl("sdt", step_files_raw)])
            scssl = readr::read_csv(step_files_raw[grepl("stepcountssl", step_files_raw)])
            scrf = readr::read_csv(step_files_raw[grepl("stepcountrf", step_files_raw)])
            truth = readr::read_csv(step_files_raw[grepl("truth", step_files_raw)])
            vs = readr::read_csv(step_files_raw[grepl("vs", step_files_raw)])
            acti = readr::read_csv(step_files_raw[grepl("acti", step_files_raw)])

            ad_r = readr::read_csv(step_files_resamp[grepl("adept", step_files_resamp)])
            oak_r = readr::read_csv(step_files_resamp[grepl("oak", step_files_resamp)])
            sdt_r = readr::read_csv(step_files_resamp[grepl("sdt", step_files_resamp)])
            scssl_r = readr::read_csv(step_files_resamp[grepl("stepcountssl", step_files_resamp)])
            scrf_r = readr::read_csv(step_files_resamp[grepl("stepcountrf", step_files_resamp)])
            vs_r = readr::read_csv(step_files_resamp[grepl("vs", step_files_resamp)])
            acti_r = readr::read_csv(step_files_resamp[grepl("acti", step_files_resamp)])


            df =
              raw_df %>% select(time) %>%
              left_join(truth, by = "time") %>%
              left_join(ad, by = "time") %>%
              left_join(oak, by = "time") %>%
              left_join(sdt, by = "time") %>%
              left_join(vs, by = "time")

            if(nrow(scssl) > 0){
              df = df %>%
                left_join(scssl, by = "time")
            }
            if(nrow(scrf) > 0){
              df = df %>%
                left_join(scrf, by = "time")
            }
            if(nrow(acti) > 0){
              df = df %>%
                left_join(acti, by = "time")
            }


            df_resampled = raw_df %>% select(time) %>%
              left_join(ad_r, by = "time") %>%
              left_join(oak_r, by = "time") %>%
              left_join(sdt_r, by = "time") %>%
              left_join(vs_r, by = "time")

            if(nrow(scssl_r) > 0){
                df_resampled = df_resampled %>%
                  left_join(scssl_r, by = "time")
            }
            if(nrow(scrf_r) > 0){
              df_resampled = df_resampled %>%
                left_join(scrf_r, by = "time")
            }
            if(nrow(acti_r) > 0){
              df_resampled = df_resampled %>%
                left_join(acti_r, by = "time")
            }
            df_resampled =
              df_resampled %>%
              rename_with(~str_c(., "_30"), .cols = starts_with("steps"))

            step_df = left_join(df, df_resampled, by = "time")
            both_w_steps = left_join(both, step_df, by = "time")
            fname = paste0("clemson-", id, "-", activity, "-nested.rds")
            saveRDS(both_w_steps, here::here("data", "processed", "clemson", id,
                                             fname))
            rm(df)
            rm(df_resampled)
            rm(both)
            both_w_steps
          })
    }) %>%
  bind_rows()
saveRDS(all_clem, here::here("data/processed", "clemson_nested_all.rds"))
rm(all_clem)

all_oxwalk_files =  list.files(here::here("data", "reorganized", "oxwalk"), full.names = TRUE,
                             recursive = TRUE)

ids = sub(".*oxwalk-(.+)\\-r.*", "\\1", all_oxwalk_files) %>% unique()
all_ox =
  map(.x = ids,
      .f = function(id){
        if(!file.exists(here::here("data", "processed", "oxwalk", id))){
          dir.create(here::here("data", "processed", "oxwalk", id))
        }
        subj_files = list.files(here::here("data", "reorganized", "oxwalk", id),
                                full.names = TRUE,
                                recursive = TRUE)
        sample_rates = c(25,100)
        map(.x = sample_rates,
            .f = function(sample_rate){
              sa_files = subj_files[grepl(paste0(sample_rate, "to"), subj_files)|
                                      grepl(paste0("raw",sample_rate), subj_files)]
              raw_file =  sa_files[grepl(".csv.gz", sa_files) & grepl("raw", sa_files)]
              resamp_file = sa_files[grepl(".csv.gz", sa_files) & grepl("resampled", sa_files)]
              raw_df =
                readr::read_csv(raw_file)  %>%
                mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
                tidyr::nest(raw_data = c(tm_dttm, X, Y, Z, ind_step)) %>%
                select(-c(sample_rate))

              resampled_df =
                readr::read_csv(resamp_file) %>%
                mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
                tidyr::nest(resampled_data = c(tm_dttm, X, Y, Z)) %>%
                select(-c(sample_rate))

              both =
                left_join(raw_df, resampled_df, by = c("time", "id_subject", "id_study")) %>%
                mutate(sample_rate = sample_rate)

              step_files = sa_files[grepl("steps", sa_files)]
              step_files_raw = step_files[grepl("raw", step_files)]
              step_files_resamp = step_files[grepl("resampled", step_files)]


              ad = readr::read_csv(step_files_raw[grepl("adept", step_files_raw)])
              oak = readr::read_csv(step_files_raw[grepl("oak", step_files_raw)])
              sdt = readr::read_csv(step_files_raw[grepl("sdt", step_files_raw)])
              scssl = readr::read_csv(step_files_raw[grepl("stepcountssl", step_files_raw)])
              scrf = readr::read_csv(step_files_raw[grepl("stepcountrf", step_files_raw)])
              truth = readr::read_csv(step_files_raw[grepl("truth", step_files_raw)])
              vs = readr::read_csv(step_files_raw[grepl("vs", step_files_raw)])
              acti = readr::read_csv(step_files_raw[grepl("acti", step_files_raw)])

              ad_r = readr::read_csv(step_files_resamp[grepl("adept", step_files_resamp)])
              oak_r = readr::read_csv(step_files_resamp[grepl("oak", step_files_resamp)])
              sdt_r = readr::read_csv(step_files_resamp[grepl("sdt", step_files_resamp)])
              scssl_r = readr::read_csv(step_files_resamp[grepl("stepcountssl", step_files_resamp)])
              scrf_r = readr::read_csv(step_files_resamp[grepl("stepcountrf", step_files_resamp)])
              vs_r = readr::read_csv(step_files_resamp[grepl("vs", step_files_resamp)])
              acti_r = readr::read_csv(step_files_resamp[grepl("acti", step_files_resamp)])


              df =
                raw_df %>% select(time) %>%
                left_join(truth, by = "time") %>%
                left_join(ad, by = "time") %>%
                left_join(oak, by = "time") %>%
                left_join(sdt, by = "time") %>%
                left_join(vs, by = "time")

              if(nrow(scssl) > 0){
                df = df %>%
                  left_join(scssl, by = "time")
              }
              if(nrow(scrf) > 0){
                df = df %>%
                  left_join(scrf, by = "time")
              }
              if(nrow(acti) > 0){
                df = df %>%
                  left_join(acti, by = "time")
              }


              df_resampled = raw_df %>% select(time) %>%
                left_join(ad_r, by = "time") %>%
                left_join(oak_r, by = "time") %>%
                left_join(sdt_r, by = "time") %>%
                left_join(vs_r, by = "time")

              if(nrow(scssl_r) > 0){
                df_resampled = df_resampled %>%
                  left_join(scssl_r, by = "time")
              }
              if(nrow(scrf_r) > 0){
                df_resampled = df_resampled %>%
                  left_join(scrf_r, by = "time")
              }
              if(nrow(acti_r) > 0){
                df_resampled = df_resampled %>%
                  left_join(acti_r, by = "time")
              }
              df_resampled =
                df_resampled %>%
                rename_with(~str_c(., "_30"), .cols = starts_with("steps"))

              step_df = left_join(df, df_resampled, by = "time")
              both_w_steps = left_join(both, step_df, by = "time")
              fname = paste0("oxwalk-", id, "-", sample_rate, "Hz-nested.rds")
              saveRDS(both_w_steps, here::here("data", "processed", "oxwalk", id,
                                               fname))
              rm(df)
              rm(df_resampled)
              rm(both)
              both_w_steps
            })
      }) %>%
  bind_rows()

saveRDS(all_ox, here::here("data/processed", "oxwalk_nested_all.rds"))
rm(all_ox)

all_marea_files =  list.files(here::here("data", "reorganized", "marea"), full.names = TRUE,
                             recursive = TRUE)
ids = substr(sub(".*marea\\/(.+)\\/.*", "\\1", all_marea_files), 1, 3) %>% unique()


all_marea =
  map(.x = ids,
      .f = function(id){
        if(!file.exists(here::here("data", "processed", "marea", id))){
          dir.create(here::here("data", "processed", "marea", id))
        }
        subj_files = list.files(here::here("data", "reorganized", "marea", id),
                                full.names = TRUE,
                                recursive = TRUE)
        activities = sub(".*\\-(.+)-r.*", "\\1", subj_files) %>% unique()
        map(.x = activities,
            .f = function(activity){
              sa_files = subj_files[grepl(activity, subj_files)]
              raw_file =  sa_files[grepl(".csv.gz", sa_files) & grepl("raw", sa_files)]
              resamp_file = sa_files[grepl(".csv.gz", sa_files) & grepl("resampled", sa_files)]

              raw_df =
                readr::read_csv(raw_file)  %>%
                mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
                tidyr::nest(raw_data = c(tm_dttm, X, Y, Z, ind_step, cat_step_type)) %>%
                select(-c(sample_rate))

              resampled_df =
                readr::read_csv(resamp_file) %>%
                mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
                tidyr::nest(resampled_data = c(tm_dttm, X, Y, Z)) %>%
                select(-c(sample_rate))

              both =
                left_join(raw_df, resampled_df, by = c("time", "id_subject", "id_study", "cat_activity", "cat_activity_large"))

              step_files = sa_files[grepl("steps", sa_files)]
              step_files_raw = step_files[grepl("raw", step_files)]
              step_files_resamp = step_files[grepl("resampled", step_files)]


              ad = readr::read_csv(step_files_raw[grepl("adept", step_files_raw)])
              oak = readr::read_csv(step_files_raw[grepl("oak", step_files_raw)])
              sdt = readr::read_csv(step_files_raw[grepl("sdt", step_files_raw)])
              scssl = readr::read_csv(step_files_raw[grepl("stepcountssl", step_files_raw)])
              scrf = readr::read_csv(step_files_raw[grepl("stepcountrf", step_files_raw)])
              truth = readr::read_csv(step_files_raw[grepl("truth", step_files_raw)])
              vs = readr::read_csv(step_files_raw[grepl("vs", step_files_raw)])
              acti = readr::read_csv(step_files_raw[grepl("acti", step_files_raw)])

              ad_r = readr::read_csv(step_files_resamp[grepl("adept", step_files_resamp)])
              oak_r = readr::read_csv(step_files_resamp[grepl("oak", step_files_resamp)])
              sdt_r = readr::read_csv(step_files_resamp[grepl("sdt", step_files_resamp)])
              scssl_r = readr::read_csv(step_files_resamp[grepl("stepcountssl", step_files_resamp)])
              scrf_r = readr::read_csv(step_files_resamp[grepl("stepcountrf", step_files_resamp)])
              vs_r = readr::read_csv(step_files_resamp[grepl("vs", step_files_resamp)])
              acti_r = readr::read_csv(step_files_resamp[grepl("acti", step_files_resamp)])


              df =
                raw_df %>% select(time) %>% distinct() %>%
                left_join(truth, by = "time") %>%
                left_join(ad, by = "time") %>%
                left_join(oak, by = "time") %>%
                left_join(sdt, by = "time") %>%
                left_join(vs, by = "time")

              if(nrow(scssl) > 0){
                df = df %>%
                  left_join(scssl, by = "time")
              }
              if(nrow(scrf) > 0){
                df = df %>%
                  left_join(scrf, by = "time")
              }
              if(nrow(acti) > 0){
                df = df %>%
                  left_join(acti, by = "time")
              }


              df_resampled = raw_df %>% select(time) %>%
                distinct() %>% # to acct for multiple seconds spanning diff activities
                left_join(ad_r, by = "time") %>%
                left_join(oak_r, by = "time") %>%
                left_join(sdt_r, by = "time") %>%
                left_join(vs_r, by = "time")

              if(nrow(scssl_r) > 0){
                df_resampled = df_resampled %>%
                  left_join(scssl_r, by = "time")
              }
              if(nrow(scrf_r) > 0){
                df_resampled = df_resampled %>%
                  left_join(scrf_r, by = "time")
              }
              if(nrow(acti_r) > 0){
                df_resampled = df_resampled %>%
                  left_join(acti_r, by = "time")
              }
              df_resampled =
                df_resampled %>%
                rename_with(~str_c(., "_30"), .cols = starts_with("steps"))

              step_df = left_join(df, df_resampled, by = "time")
              both_w_steps = left_join(both, step_df, by = "time")
              fname = paste0("marea-", id, "-", activity, "-nested.rds")
              saveRDS(both_w_steps, here::here("data", "processed", "marea", id,
                                               fname))
              rm(df)
              rm(df_resampled)
              rm(both)
              both_w_steps
            })
      }) %>%
  bind_rows()
saveRDS(all_marea, here::here("data/processed", "marea_nested_all.rds"))
rm(all_marea)


# make datasets without nested data
# start here

clem = readRDS(here::here("data/processed/clemson_nested_all.rds")) %>%
  select(-c(raw_data, resampled_data)) %>%
  mutate(across(contains("steps_sc"), ~ifelse(is.na(.x), 0, .x)))

readr::write_csv(clem, here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))

ox = readRDS(here::here("data/processed/oxwalk_nested_all.rds")) %>%
  select(-c(raw_data, resampled_data)) %>%
  mutate(across(contains("steps_sc"), ~ifelse(is.na(.x), 0, .x)))


readr::write_csv(ox, here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))

marea = readRDS(here::here("data/processed/marea_nested_all.rds")) %>%
  select(-c(raw_data, resampled_data)) %>%
  mutate(across(contains("steps_sc"), ~ifelse(is.na(.x), 0, .x)))

readr::write_csv(marea, here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz"))

