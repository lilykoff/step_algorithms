# devtools::install_github("https://github.com/muschellij2/walking")
# devtools::install_github("https://github.com/jhuwit/stepcount")
library(adept)
library(adeptdata)
library(walking)
library(stepcount)
library(tidyverse)
options(digits.secs = 3)

# data is data frame with columns X, Y, Z for acceleration and HEADER_TIMESTAMP or time for time
# sample_rate_of_raw_data  = sample_rate of original data
# data_file is path to the file



# adept
all_wrist_templates = adeptdata::stride_template$left_wrist
template_list = do.call(rbind, all_wrist_templates)
template_list = apply(template_list, 1, identity, simplify = FALSE)
template = template_list

step_result = adept::segmentWalking(
  xyz = data[, c("X", "Y", "Z")],
  xyz.fs = sample_rate_of_raw_data,
  template = template_list,
  compute.template.idx = FALSE,
  run.parallel = TRUE,
  run.parallel.cores = 8,
  sim_MIN = 0.6,
  dur_MIN = 0.8,
  dur_MAX = 1.4,
  ptp_r_MIN = 0.5,
  ptp_r_MAX = 2,
  vmc_r_MIN = 0.05,
  vmc_r_MAX = 0.5,
  mean_abs_diff_med_p_MAX = 0.7,
  mean_abs_diff_med_t_MAX = 0.2,
  mean_abs_diff_dur_MAX = 0.3
) %>%
  filter(is_walking_i == 1) %>%
  mutate(steps = 2 / (T_i / sample_rate_of_raw_data))
# the steps per second
data %>%
  mutate(row_ind = row_number()) %>%
  left_join(., step_result, by = c("row_ind" = "tau_i")) %>%
  mutate(
    steps = ifelse(is.na(steps), 0, steps),
    time = lubridate::floor_date(HEADER_TIMESTAMP, unit = "seconds")
  ) %>%
  group_by(time) %>%
  summarize(steps_adept = sum(steps)) %>%
  select(time, steps_adept)

# oak
walking::estimate_steps_forest(data)

# verisense original
walking::estimate_steps_verisense(
  data,
  method = "original",
  resample_to_15hz = TRUE,
  sample_rate = sample_rate_of_raw_data
)

# verisense revised
walking::estimate_steps_verisense(
  data,
  method = "revised",
  resample_to_15hz = TRUE,
  sample_rate = sample_rate_of_raw_data
)

# SDT
walking::estimate_steps_sdt(
  data,
  sample_rate = sample_rate_of_raw_data
)

# stepcount

# ssl version
stepcount::stepcount(file = data_file,
                     model_type = "ssl",
                     sample_rate = sample_rate_of_raw_data)
# random forest
stepcount::stepcount(file = data_file,
                     model_type = "rf",
                     sample_rate = sample_rate_of_raw_data)
