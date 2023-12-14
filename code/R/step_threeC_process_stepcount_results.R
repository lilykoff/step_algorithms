
options(digits.secs = 3)
source(here::here("code/R/utils.R"))
files = list.files(here::here("results/stepcount"), full.names = TRUE,
                     recursive = TRUE)

target_csvs = files[grep("\\-Steps.csv", files)]

clemson_csvs = target_csvs[grep("clemson", target_csvs)]
ox_csvs = target_csvs[grep("ox", target_csvs)]

processed_stepcount_clemson =
  map(.x = clemson_csvs,
      .f = process_sc_results) %>%
  bind_rows()

processed_stepcount_ox =
  map(.x = ox_csvs,
      .f = process_sc_results) %>%
  bind_rows()

write_csv(processed_stepcount_clemson, here::here("results", "stepcount_all_clemson.csv"))
write_csv(processed_stepcount_ox, here::here("results", "stepcount_all_ox.csv"))
