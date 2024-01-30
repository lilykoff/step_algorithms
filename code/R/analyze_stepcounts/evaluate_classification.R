####
# accuracy/f1 stuff
# accuracy = (tp + tn)/(tp + tn + fp + fn)
# recall = tp / tp+ fn
# prec = tp / tp + fp
library(tidyverse)

# values = c("#990F0FFF","#CC5151FF",
#            "#99540FFF","#CC8E51FF",
#            "#6B990FFF", "#A3CC51FF",
#            "#0F6B99FF", "#51A3CCFF",
#            "#260F99FF", "#6551CCFF")

# load all results data - one second

# HERE


clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
# for accuracy, we can't use stepcount
clemson = clemson %>%
  select(-c("steps_sc", "steps_sc_30"))

# check to make sure NAs are just happening at end
clemson %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  filter(position != 1)

# now get rid of last row
clemson = clemson %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position)

oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))

# for now just use 100 hz
oxwalk = oxwalk %>%
  select(-c("steps_sc", "steps_sc_30")) %>%
  filter(sample_rate == 100)


# check to make sure NAs are just happening at end
oxwalk %>%
  group_by(id_subject) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  filter(position != 1)

# now get rid of last row
oxwalk = oxwalk %>%
  group_by(id_subject) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position)

marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz")) %>%
  select(-c("steps_sc", "steps_sc_30"))



# check to make sure NAs are just happening at end
marea %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  select(time, steps_truth, steps_oak, steps_oak_30, steps_acti_30, position) %>%
  filter(position != 1)

# now get rid of last row
marea = marea %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position)

# generate a table 1 with study descriptions
marea %>%
  group_by(cat_activity_large) %>%
  summarize(subs = length(unique(id_subject)))

marea %>%
  group_by(cat_activity_large, id_subject) %>%
  summarize(n = n()/60) %>%
  group_by(cat_activity_large) %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))
marea %>%
  group_by(cat_activity_large, id_subject) %>%
  summarize(n = sum(steps_truth)) %>%
  group_by(cat_activity_large) %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))

clemson %>%
  group_by(cat_activity, id_subject) %>%
  summarize(n = n()/60) %>%
  group_by(cat_activity) %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))


clemson %>%
  group_by(cat_activity, id_subject) %>%
  summarize(n = sum(steps_truth)) %>%
  group_by(cat_activity) %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))
oxwalk %>%
  group_by( id_subject) %>%
  summarize(n = n()/60) %>%
  ungroup() %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))
oxwalk %>%
  group_by(id_subject) %>%
  summarize(n = sum(steps_truth)) %>%
  ungroup() %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))

# for classification analysis, just use on resampled data
clemson = clemson %>%
  select(time, id_study, id_subject, cat_activity, ends_with("30"),
         steps_truth) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular", "walk_semiregular", "walk_irregular")))

oxwalk = oxwalk %>%
  select(time, id_study, id_subject,  ends_with("30"),
         steps_truth)

marea = marea %>%
  select(time, id_study, id_subject,  ends_with("30"),
         steps_truth, cat_activity)
# and get rid of running
marea = marea %>%
  filter(grepl("run", cat_activity)==FALSE)

# walking recognition table

# supplemental classification table
oxwalk %>%
  mutate(cat_activity = "ox") %>%
  bind_rows(clemson) %>%
  bind_rows(marea %>% mutate(cat_activity = "marea")) %>%
  mutate(cat_activity = factor(cat_activity,
                               levels = c("walk_regular", "walk_semiregular",
                                          "walk_irregular", "marea", "ox"))) %>%
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
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, algorithm, cat_activity)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  # get means
  group_by(algorithm, cat_activity) %>%
  summarize(across(c(recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(round(recall_mean, 3), " (", round(recall_sd, 3), ")"),
         precision = paste0(round(prec_mean, 3), " (", round(prec_sd, 3), ")"),
         f1 = paste0(round(f1_mean, 3), " (", round(f1_sd, 3), ")")) %>%
  select(algorithm, cat_activity, recall, precision, f1) %>%
  pivot_wider(names_from = cat_activity, values_from = recall:f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll", booktabs = TRUE, format = "latex", col.names =
                      c("Algorithm", rep(c("Regular", "Semiregular", "Irregular", "Regular", "Free-Living"), 3))) %>%
  kableExtra::add_header_above(c(" " = 1, "Clemson" = 3,
                                 "MAREA" = 1, "OxWalk" = 1, "Clemson" = 3,
                                 "MAREA" = 1, "OxWalk" = 1, "Clemson" = 3,
                                 "MAREA" = 1, "OxWalk" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "Recall" = 5,
                                 "Precision" = 5, "F1 Score" = 5)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# longer table format
oxwalk %>%
  mutate(cat_activity = "ox") %>%
  bind_rows(clemson) %>%
  bind_rows(marea %>% mutate(cat_activity = "marea")) %>%
  mutate(cat_activity = factor(cat_activity,
                               levels = c("walk_regular", "walk_semiregular",
                                          "walk_irregular", "marea", "ox"))) %>%
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
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, algorithm, cat_activity)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  # get means
  group_by(algorithm, cat_activity) %>%
  summarize(across(c(recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(round(recall_mean, 3), " (", round(recall_sd, 3), ")"),
         precision = paste0(round(prec_mean, 3), " (", round(prec_sd, 3), ")"),
         f1 = paste0(round(f1_mean, 3), " (", round(f1_sd, 3), ")")) %>%
  select(algorithm, cat_activity, recall, precision, f1) %>%
  pivot_longer(recall:f1) %>%
  pivot_wider(names_from = cat_activity, values_from = value) %>%
  ungroup() %>%
  select(name, algorithm, walk_regular, walk_semiregular, walk_irregular, marea, ox) %>%
  arrange(name) %>%
  kableExtra::kable(align = "llll", booktabs = TRUE,  format = "latex", col.names =
                      c("Metric", "Algorithm", rep(c("Regular", "Semiregular", "Irregular", "Regular", "Free-Living"), 1)))  %>%
  kableExtra::collapse_rows(columns = 1:2, valign = "top") %>%
  kableExtra::add_header_above(c(" " = 2, "Clemson" = 3,
                                 "MAREA" = 1, "OxWalk" = 1))  %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# main manuscript classification table
oxwalk %>%
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  mutate(id_study = factor(id_study,
                               levels = c("clemson", "marea", "oxwalk"))) %>%
  # classify each second as true positive/neg or false positive/neg
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_subject, id_study) %>%
  # summarize tp, fp, tn, fn by id and activity
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, algorithm, id_study)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  # get means
  group_by(algorithm, id_study) %>%
  summarize(across(c(recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(round(recall_mean, 3), " (", round(recall_sd, 3), ")"),
         precision = paste0(round(prec_mean, 3), " (", round(prec_sd, 3), ")"),
         f1 = paste0(round(f1_mean, 3), " (", round(f1_sd, 3), ")")) %>%
  select(algorithm, id_study, recall, precision, f1) %>%
  pivot_wider(names_from = id_study, values_from = recall:f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll", booktabs = TRUE, format = "latex", col.names =
                      c("Algorithm", rep(c("Clemson", "MAREA", "Oxwalk"), 3))) %>%
  kableExtra::add_header_above(c(" " = 1, "Recall" = 3,
                                 "Precision" = 3, "F1 Score" = 3)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# main manuscript boxplots of metrics

recog_mean_sd =
  oxwalk %>%
  mutate(cat_activity = "ox") %>%
  bind_rows(clemson) %>%
  bind_rows(marea %>% mutate(cat_activity = "marea")) %>%
  mutate(cat_activity = factor(cat_activity,
                               levels = c("walk_regular", "walk_semiregular",
                                          "walk_irregular", "marea", "ox"))) %>%
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
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name))  %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, algorithm, cat_activity)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  # get means
  group_by(algorithm, cat_activity) %>%
  summarize(across(c(recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE))))

recog_mean =
  oxwalk %>%
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  mutate(id_study = factor(id_study,
                           levels = c("clemson", "marea", "oxwalk"))) %>%
  # classify each second as true positive/neg or false positive/neg
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_subject, id_study) %>%
  # summarize tp, fp, tn, fn by id and activity
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name))  %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, algorithm, id_study)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  # get means
  group_by(algorithm, id_study) %>%
  summarize(across(c(recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = ends_with("mean")) %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  ungroup()


# each row is subject, activity
recog_stats =
  oxwalk %>%
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  mutate(id_study = factor(id_study,
                           levels = c("clemson", "marea", "oxwalk"))) %>%
  # classify each second as true positive/neg or false positive/neg
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_subject, id_study) %>%
  # summarize tp, fp, tn, fn by id and activity
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name))  %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, algorithm, id_study)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup()

labs = c("F1 Score", "Precision", "Recall")
names(labs) = c("f1", "prec", "recall")


labs2 = c("Clemson", "MAREA", "Oxwalk")
names(labs2) = c("clemson", "marea", "oxwalk")
level_order_ox = c(recog_mean %>%
                  filter(id_study == "oxwalk" & name == "f1_mean") %>%
                  arrange(value) %>%
                  pull(algorithm))

level_order_marea = c(recog_mean %>%
                  filter(id_study == "marea" & name == "f1_mean") %>%
                  arrange(value) %>%
                  pull(algorithm))
level_order_clem = c(recog_mean %>%
                       filter(id_study == "clemson" & name == "f1_mean") %>%
                       arrange(value) %>%
                       pull(algorithm))
# current figure
# factor(algorithm, level = level_order)
recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  mutate(algorithm2 =
           case_when(id_study == "clemson" & algorithm=="adept" ~ "1adept",
                     id_study == "clemson" & algorithm=="vs" ~ "2vs",
                     id_study == "clemson" & algorithm=="sdt" ~ "3sdt",
                     id_study == "clemson" & algorithm=="oak" ~ "4oak",
                     id_study == "clemson" & algorithm=="acti" ~ "5acti",
                     id_study == "clemson" ~ "6scrf",
                     id_study == "marea" & algorithm=="oak" ~ "1oak",
                     id_study == "marea" & algorithm=="adept" ~ "2adept",
                     id_study == "marea" & algorithm=="vs" ~ "3vs",
                     id_study == "marea" & algorithm=="acti" ~ "4acti",
                     id_study == "marea" & algorithm=="scrf" ~ "5scrf",
                     id_study == "marea" ~ "6sdt",
                     id_study == "oxwalk" & algorithm=="adept" ~ "1adept",
                     id_study == "oxwalk" & algorithm=="sdt" ~ "2sdt",
                     id_study == "oxwalk" & algorithm=="acti" ~ "3acti",
                     id_study == "oxwalk" & algorithm=="vs" ~ "4vs",
                     id_study == "oxwalk" & algorithm=="oak" ~ "5oak",
                     id_study == "oxwalk" ~ "6scrf")) %>%
  ggplot(aes(x = algorithm2, y = value, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.2)+
  facet_grid(name ~ id_study, scales = "free_x",
             labeller = labeller(name = labs,
                                 id_study = labs2))+
  scale_color_brewer(palette  = "Dark2",name = "",
                     labels = c("ActiLife", "ADEPT", "Oak","Stepcount", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank())+
  labs(x = "", y = "")+
  guides(colour = guide_legend(nrow = 1))

  # to add text to plots
  # geom_text(data = recog_mean %>%
  #             mutate(name = sub("\\_.*", "", name)),
  #           aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)
  #



# old stuff
my_comparisons = list(c("acti", "adept"), c("acti", "oak"),c("acti", "sdt"), c("acti", "vs"),
                      c("adept", "oak"),c("adept", "sdt"), c("adept", "vs"),
                      c("oak", "sdt"), c("oak", "vs"),
                     c("vs", "sdt"))

# plt2 +
#   ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE,
#                              label= "p.signif")
# plt3 +
#   ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE,
#                              label= "p.signif")

plt3 +
  ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE,
                             label= "p.format")

# full latex table, way # 1 (averaging across indiv)

