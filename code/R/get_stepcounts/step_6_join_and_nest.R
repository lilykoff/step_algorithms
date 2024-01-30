library(tidyverse)
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
      subj_files = list.files(here::here("data", "reorganized", "clemson", id),
                              full.names = TRUE,
                            recursive = TRUE)
      activities = paste0("walk_", sub(".*walk\\_(.+)\\-r.*", "\\1", subj_files)) %>% unique()
      map(.x = activities,
          .f = function(activity){
            sa_files = subj_files[grepl(activity, subj_files)]
            raw_file =  sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("raw", sa_files) == TRUE]
            resamp_file = sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("resampled", sa_files) == TRUE]

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
              left_join(raw_df, resampled_df, by = c("time", "id_subject", "id_study", "cat_activity"))

            step_files = sa_files[grepl("steps", sa_files)]
            step_files_raw = step_files[grepl("raw", step_files)]
            step_files_resamp = step_files[grepl("resampled", step_files)]
            # get actilife - only resampled data for now - need to make same format as others
            acti_file = step_files_resamp[grepl("acti", step_files_resamp)]
            acti = readr::read_csv(acti_file)
            start_time = floor_date(min(raw_df$time), unit = "seconds")

            acti = acti %>%
              mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
              select(time, steps_acti)

            ad = readr::read_csv(step_files_raw[grepl("adept", step_files_raw)])
            oak = readr::read_csv(step_files_raw[grepl("oak", step_files_raw)])
            sdt = readr::read_csv(step_files_raw[grepl("sdt", step_files_raw)])
            sc = readr::read_csv(step_files_raw[grepl("stepcount.csv", step_files_raw)]) %>%
              mutate(time = floor_date(time, unit = "seconds"))
            scrf = readr::read_csv(step_files_raw[grepl("stepcountrf", step_files_raw)])
            truth = readr::read_csv(step_files_raw[grepl("truth", step_files_raw)])
            vs = readr::read_csv(step_files_raw[grepl("vs", step_files_raw)])

            ad_r = readr::read_csv(step_files_resamp[grepl("adept", step_files_resamp)])
            oak_r = readr::read_csv(step_files_resamp[grepl("oak", step_files_resamp)])
            sdt_r = readr::read_csv(step_files_resamp[grepl("sdt", step_files_resamp)])
            sc_r = readr::read_csv(step_files_resamp[grepl("stepcount.csv", step_files_resamp)]) %>%
              mutate(time = floor_date(time, unit = "seconds"))
            scrf_r = readr::read_csv(step_files_resamp[grepl("stepcountrf", step_files_resamp)])
            vs_r = readr::read_csv(step_files_resamp[grepl("vs", step_files_resamp)])

            # to account for errors in stepcount
            if(nrow(scrf) ==0){
              scrf = tibble(time = raw_df$time, steps_scrf = 0)
            }
            if(nrow(scrf_r)==0){
              scrf = tibble(time = raw_df$time, steps_scrf = 0)
            }

            df =
              raw_df %>% select(time) %>% distinct() %>%
              left_join(truth, by = "time") %>%
              left_join(ad, by = "time") %>%
              left_join(oak, by = "time") %>%
              left_join(sdt, by = "time") %>%
              left_join(scrf, by = "time") %>%
              left_join(vs, by = "time") %>%
              left_join(sc, by = "time")


            df_resampled = raw_df %>% select(time) %>% distinct() %>%
              left_join(ad_r, by = "time") %>%
              left_join(oak_r, by = "time") %>%
              left_join(sdt_r, by = "time") %>%
              left_join(scrf_r, by = "time") %>%
              left_join(vs_r, by = "time") %>%
              left_join(sc_r, by = "time") %>%
              left_join(acti, by = "time") %>%
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
              raw_file =  sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("raw", sa_files) == TRUE]
              resamp_file = sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("resampled", sa_files) == TRUE]
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
              # get actilife - only resampled data for now - need to make same format as others
              acti_file = step_files_resamp[grepl("acti", step_files_resamp)]
              acti = readr::read_csv(acti_file)
              start_time = floor_date(min(raw_df$time), unit = "seconds")

              acti = acti %>%
                mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
                select(time, steps_acti)

              ad = readr::read_csv(step_files_raw[grepl("adept", step_files_raw)])
              oak = readr::read_csv(step_files_raw[grepl("oak", step_files_raw)])
              sdt = readr::read_csv(step_files_raw[grepl("sdt", step_files_raw)])
              sc = readr::read_csv(step_files_raw[grepl("stepcount.csv", step_files_raw)]) %>%
                mutate(time = floor_date(time, unit = "seconds"))
              scrf = readr::read_csv(step_files_raw[grepl("stepcountrf", step_files_raw)])
              truth = readr::read_csv(step_files_raw[grepl("truth", step_files_raw)])
              vs = readr::read_csv(step_files_raw[grepl("vs", step_files_raw)])

              ad_r = readr::read_csv(step_files_resamp[grepl("adept", step_files_resamp)])
              oak_r = readr::read_csv(step_files_resamp[grepl("oak", step_files_resamp)])
              sdt_r = readr::read_csv(step_files_resamp[grepl("sdt", step_files_resamp)])
              sc_r = readr::read_csv(step_files_resamp[grepl("stepcount.csv", step_files_resamp)]) %>%
                mutate(time = floor_date(time, unit = "seconds"))

              scrf_r = readr::read_csv(step_files_resamp[grepl("stepcountrf", step_files_resamp)])
              vs_r = readr::read_csv(step_files_resamp[grepl("vs", step_files_resamp)])

              # temp fix for bugs w stepcount
              if(nrow(scrf) == 0){
                scrf = tibble(time = raw_df$time, steps_scrf = 0)
              }
              if(nrow(scrf_r)==0){
                scrf = tibble(time = raw_df$time, steps_scrf = 0)
              }

              df =
                raw_df %>% select(time) %>% distinct() %>%
                left_join(truth, by = "time") %>%
                left_join(ad, by = "time") %>%
                left_join(oak, by = "time") %>%
                left_join(sdt, by = "time") %>%
                left_join(scrf, by = "time") %>%
                left_join(vs, by = "time") %>%
                left_join(sc, by = "time")


              df_resampled = raw_df %>% select(time) %>% distinct() %>%
                left_join(ad_r, by = "time") %>%
                left_join(oak_r, by = "time") %>%
                left_join(sdt_r, by = "time") %>%
                left_join(scrf_r, by = "time") %>%
                left_join(vs_r, by = "time") %>%
                left_join(sc_r, by = "time") %>%
                left_join(acti, by = "time") %>%
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
# find ./ -name "*indoor_run*" | xargs rm -r
# start here

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
              raw_file =  sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("raw", sa_files) == TRUE]
              resamp_file = sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("resampled", sa_files) == TRUE][1]
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

              # get actilife - only resampled data for now - need to make same format as others
              acti_file = step_files_resamp[grepl("acti", step_files_resamp)]
              acti = readr::read_csv(acti_file)
              start_time = floor_date(min(raw_df$time), unit = "seconds")

              acti = acti %>%
                mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
                select(time, steps_acti)

              ad = readr::read_csv(step_files_raw[grepl("adept", step_files_raw)])
              oak = readr::read_csv(step_files_raw[grepl("oak", step_files_raw)])
              sdt = readr::read_csv(step_files_raw[grepl("sdt", step_files_raw)])
              sc = readr::read_csv(step_files_raw[grepl("stepcount.csv", step_files_raw)]) %>%
                mutate(time = floor_date(time, unit = "seconds"))
              scrf = readr::read_csv(step_files_raw[grepl("stepcountrf", step_files_raw)])
              truth = readr::read_csv(step_files_raw[grepl("truth", step_files_raw)])
              vs = readr::read_csv(step_files_raw[grepl("vs", step_files_raw)])

              ad_r = readr::read_csv(step_files_resamp[grepl("adept", step_files_resamp)])
              oak_r = readr::read_csv(step_files_resamp[grepl("oak", step_files_resamp)])
              sdt_r = readr::read_csv(step_files_resamp[grepl("sdt", step_files_resamp)])
              sc_r = readr::read_csv(step_files_resamp[grepl("stepcount.csv", step_files_resamp)]) %>%
                mutate(time = floor_date(time, unit = "seconds"))
              scrf_r = readr::read_csv(step_files_resamp[grepl("stepcountrf", step_files_resamp)])
              vs_r = readr::read_csv(step_files_resamp[grepl("vs", step_files_resamp)])

              if(nrow(scrf) ==0){
                scrf = tibble(time = raw_df$time, steps_scrf = 0)
              }
              if(nrow(scrf_r)==0){
                scrf = tibble(time = raw_df$time, steps_scrf = 0)
              }

              df =
                raw_df %>% select(time) %>% distinct() %>%
                left_join(truth, by = "time") %>%
                left_join(ad, by = "time") %>%
                left_join(oak, by = "time") %>%
                left_join(sdt, by = "time") %>%
                left_join(scrf, by = "time") %>%
                left_join(vs, by = "time") %>%
                left_join(sc, by = "time")


              df_resampled = raw_df %>% select(time) %>% distinct() %>%
                left_join(ad_r, by = "time") %>%
                left_join(oak_r, by = "time") %>%
                left_join(sdt_r, by = "time") %>%
                left_join(scrf_r, by = "time") %>%
                left_join(vs_r, by = "time") %>%
                left_join(sc_r, by = "time") %>%
                left_join(acti, by = "time") %>%
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
  mutate(across(contains("rf"), ~ifelse(is.na(.x), 0, .x)))

readr::write_csv(clem, here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
clem10 = clem %>%
  mutate(time_10 = floor_date(time, unit = "10 seconds")) %>%
  group_by(time_10, id_subject, cat_activity) %>%
  mutate(n_seconds = n()) %>%
  group_by(id_subject, cat_activity, time_10, n_seconds) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE)))

readr::write_csv(clem10, here::here("results/all_algorithms/clemson_step_estimates_10sec.csv.gz"))


ox = readRDS(here::here("data/processed/oxwalk_nested_all.rds")) %>%
  select(-c(raw_data, resampled_data)) %>%
  mutate(across(contains("rf"), ~ifelse(is.na(.x), 0, .x)))

readr::write_csv(ox, here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))
ox10 = ox %>%
  mutate(time_10 = floor_date(time, unit = "10 seconds")) %>%
  group_by(time_10, id_subject, sample_rate) %>%
  mutate(n_seconds = n()) %>%
  group_by(id_subject, sample_rate, time_10, n_seconds) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE)))

readr::write_csv(ox10, here::here("results/all_algorithms/oxwalk_step_estimates_10sec.csv.gz"))

marea = readRDS(here::here("data/processed/marea_nested_all.rds")) %>%
  select(-c(raw_data, resampled_data)) %>%
  mutate(across(contains("rf"), ~ifelse(is.na(.x), 0, .x)))

readr::write_csv(marea, here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz"))
marea10 = marea %>%
  mutate(time_10 = floor_date(time, unit = "10 seconds")) %>%
  group_by(time_10, id_subject, cat_activity, cat_activity_large, slope, speed) %>%
  mutate(n_seconds = n()) %>%
  group_by(id_subject, cat_activity, cat_activity_large, slope, speed, time_10, n_seconds) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE)))

readr::write_csv(marea10, here::here("results/all_algorithms/marea_step_estimates_10sec.csv.gz"))

