library(tidyverse)
clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))
# remove running from MAREA
marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz")) %>%
  filter(grepl("run", cat_activity)==FALSE)


clemson = clemson %>%
  mutate(cat_activity = paste0("clemson_", cat_activity))
oxwalk = oxwalk %>%
  mutate(cat_activity = ifelse(sample_rate == 100, "oxwalk100", "oxwalk25"))

marea = marea %>%
  mutate(cat_activity = "marea")

# create data frame with f1 score, precision, and recall by algorithm and individual
f1_pre_recall_acc =
  oxwalk %>%
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  ungroup() %>% # just make sure not grouped
  # classify each second as true positive/neg or false positive/neg
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_subject, cat_activity) %>%
  # summarize tp, fp, tn, fn by id and activity
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, algorithm, cat_activity)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))),
         acc = (tp + tn) / (tp + tn + fp + fn)) %>%
  mutate(prec = ifelse(tp == 0 & fp == 0 & fn > 0, 0, prec)) %>%
  ungroup() %>%
  mutate(id_study = case_when(cat_activity == "marea" ~ "marea",
                              cat_activity %in% c("oxwalk100", "oxwalk25") ~ "oxwalk",
                              TRUE ~ "clemson"))

# same thing but with clemson all together
f1_pre_recall_acc_clem =
  clemson %>%
  ungroup() %>% # just make sure not grouped
  # classify each second as true positive/neg or false positive/neg
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_subject) %>%
  # summarize tp, fp, tn, fn by id
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, algorithm)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))),
         acc = (tp + tn) / (tp + tn + fp + fn)) %>%
  mutate(prec = ifelse(tp == 0 & fp == 0 & fn > 0, 0, prec)) %>%
  ungroup() %>%
  mutate(id_study = "clemson", cat_activity = "clemson_overall")

f1_prec_recall_all =
  f1_pre_recall_acc %>%
  bind_rows(f1_pre_recall_acc_clem)

saveRDS(f1_prec_recall_all, here::here("results", "all_algorithms",
                                       "accuracy_stats_bysubject.rds"))

rm(list = ls())

# do the same thing for step counts

library(tidyverse)
clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))
# remove running from MAREA
marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz")) %>%
  filter(grepl("run", cat_activity)==FALSE)

clemson = clemson %>%
  mutate(cat_activity = paste0("clemson_", cat_activity))
oxwalk = oxwalk %>%
  mutate(cat_activity = ifelse(sample_rate == 100, "oxwalk100", "oxwalk25"))

marea = marea %>%
  mutate(cat_activity = "marea")


bias_ape =
  oxwalk %>%
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  ungroup() %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x))) %>% # sum steps across subjects
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth*100), # absolute pct error
                     bias = ~ .x - steps_truth))) %>% # bias (predicted - truth)
  select(id_subject, cat_activity, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_.*", "\\1", name)) %>%
  select(-name) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(algorithm = paste0("steps_", algorithm)) %>%  # just so in same format as accuracy
  mutate(id_study = case_when(cat_activity == "marea" ~ "marea",
                              cat_activity %in% c("oxwalk100", "oxwalk25") ~ "oxwalk",
                              TRUE ~ "clemson"))

bias_ape_clem =
  clemson %>%
  ungroup() %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x))) %>% # sum steps across subjects
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth*100), # absolute pct error
                     bias = ~ .x - steps_truth))) %>% # bias (predicted - truth)
  select(id_subject, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_.*", "\\1", name)) %>%
  select(-name) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(algorithm = paste0("steps_", algorithm)) %>%  # just so in same format as accuracy
  mutate(id_study = "clemson", cat_activity = "clemson_overall")

bias_ape_all =
  bias_ape %>%
  bind_rows(bias_ape_clem)

saveRDS(bias_ape_all, here::here("results", "all_algorithms",
                                       "step_stats_bysubject.rds"))

rm(list = ls())

# finally do df that is total steps by subject, df
clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))
# remove running from MAREA
marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz")) %>%
  filter(grepl("run", cat_activity)==FALSE)

clemson = clemson %>%
  mutate(cat_activity = paste0("clemson_", cat_activity))
oxwalk = oxwalk %>%
  mutate(cat_activity = ifelse(sample_rate == 100, "oxwalk100", "oxwalk25"))

# marea = marea %>%
#   mutate(cat_activity = "marea")


total_steps =
  oxwalk %>%
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  ungroup() %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x))) %>% # sum steps across subjects
  ungroup() %>%
  pivot_longer(cols = starts_with("steps"), names_to = "algorithm",
               values_to = "total_steps")
total_steps_clem =
  clemson %>%
  ungroup() %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x))) %>% # sum steps across subjects
  ungroup() %>%
  pivot_longer(cols = starts_with("steps"), names_to = "algorithm",
               values_to = "total_steps") %>%
  mutate(id_study = "clemson", cat_activity = "clemson_overall")

total_steps_all =
  total_steps %>%
  bind_rows(total_steps_clem)

saveRDS(total_steps_all, here::here("results", "all_algorithms",
                                 "total_steps_bysubject.rds"))

rm(list = ls())

