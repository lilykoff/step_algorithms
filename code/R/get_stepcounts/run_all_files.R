source(here::here("code", "R", "get_stepcounts", "step_1_download_process_data.R"))
source(here::here("code", "R", "get_stepcounts", "step_2_resample_data.R"))
source(here::here("code", "R", "get_stepcounts", "step_1_download_process_data.R"))
source(here::here("code", "R", "get_stepcounts", "step_3_reformat_data_for_actilife_stepcount.R"))
source(here::here("code", "R", "get_stepcounts", "step_4_fit_algorithms.R"))
system()
source(here::here("code", "R", "get_stepcounts", "step_5_process_actilife_stepcount_results.R"))
source(here::here("code", "R", "get_stepcounts", "step_6_join_and_nest.R"))

