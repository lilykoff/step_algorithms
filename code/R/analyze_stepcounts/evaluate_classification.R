####
# accuracy/f1 stuff
# accuracy = (tp + tn)/(tp + tn + fp + fn)
# recall = tp / tp+ fn
# prec = tp / tp + fp
library(tidyverse)
clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
# just use 100 hz data from oxwalk for this part
oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz")) %>%
  filter(sample_rate == 100)
# remove running from MAREA
marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz")) %>%
  filter(grepl("run", cat_activity)==FALSE)


# just use steps estimated from 30hz data for all algorithms
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


# supplemental classification table, wide
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
  ungroup() %>%
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
  # get means by algorithm and activity
  group_by(algorithm, cat_activity) %>%
  summarize(across(c(recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
         precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
         f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
  select(algorithm, cat_activity, recall, precision, f1) %>%
  pivot_wider(names_from = cat_activity, values_from = recall:f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")) %>%
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

# supplemental classification table, long
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
  mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
         precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
         f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
  select(algorithm, cat_activity, recall, precision, f1) %>%
  pivot_longer(recall:f1) %>%
  pivot_wider(names_from = cat_activity, values_from = value) %>%
  ungroup() %>%
  select(name, algorithm, marea,  walk_regular, walk_semiregular, walk_irregular, ox) %>%
  arrange(name) %>%
  kableExtra::kable(align = "llll", booktabs = TRUE,  format = "latex", col.names =
                      c("Metric", "Algorithm", rep(c("Regular", "Regular", "Semiregular", "Irregular", "Free-Living"), 1)))  %>%
  kableExtra::collapse_rows(columns = 1:2, valign = "top") %>%
  kableExtra::add_header_above(c(" " = 2, "MAREA" = 1,
                                 "Clemson" = 3, "OxWalk" = 1))  %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# main manuscript classification table
tab_individual =
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
  mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
         precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
         f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
  select(algorithm, id_study, recall, precision, f1) %>%
  pivot_wider(names_from = id_study, values_from = recall:f1) %>%
  select(-precision_marea)

tab_overall =
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
  group_by(algorithm) %>%
  summarize(across(c(recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
         precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
         f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
  select(algorithm, recall, precision, f1) %>%
  rename_with(~str_c(., "_overall"), .cols = -algorithm)

tab_individual %>%
  left_join(tab_overall) %>%
  ungroup() %>%
  select(algorithm, starts_with("recall"), starts_with("precision"), starts_with("f1")) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll", booktabs = TRUE,  format = "latex", col.names =
                      c("Algorithm", "Clemson", "MAREA", "Oxwalk", "Overall",
                        "Clemson", "Oxwalk", "Overall",
                        "Clemson", "MAREA", "Oxwalk", "Overall")) %>%
  kableExtra::add_header_above(c(" " = 1, "Recall" = 4,
                                 "Precision" = 3, "F1 Score" = 4)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")


# main manuscript boxplots of metrics
recog_median =
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
                   list(median = ~median(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = ends_with("median")) %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  ungroup()

level_order =
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
  group_by(algorithm) %>%
  summarize(median = median(f1)) %>%
  ungroup() %>%
  arrange(median) %>%
  pull(algorithm)


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

# overall panel
overall =
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
  pivot_longer(cols = recall:f1) %>%
  mutate(id_study = "overall")
labs = c("F1 Score", "Precision", "Recall")
names(labs) = c("f1", "prec", "recall")


labs2 = c("Clemson", "MAREA", "OxWalk", "Overall")
names(labs2) = c("clemson", "marea", "oxwalk", "overall")

# current figure
recog_stats %>%
  # mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  bind_rows(overall) %>%
  mutate(value= ifelse(name == "prec" & id_study == "marea", NA, value)) %>%
  mutate(id_study = factor(id_study, levels = c("clemson", "marea", "oxwalk", "overall"))) %>%
  ggplot(aes(x = factor(algorithm, levels = level_order), y = value, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.5)+
  facet_grid(name ~ id_study,
             labeller = labeller(name = labs,
                                 id_study = labs2)) +
  scale_color_brewer(palette  = "Dark2",name = "",
                     labels = c("ActiLife", "ADEPT", "Oak","Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = c(.49, .6),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10))+
  labs(x = "", y = "")+
  guides(colour = guide_legend(nrow = 4))


recog_stats_tmp =
  recog_stats %>%
  select(recall, prec, f1, id_subject, id_study, algorithm)
# create overall df
recog_stats_test =
  recog_stats_tmp %>% mutate(id_study = "overall") %>%
  bind_rows(recog_stats %>%
              select(recall, prec, f1, id_subject, id_study, algorithm))

expand.grid.unique <- function(x, y, include.equals=FALSE)
{
  x <- unique(x)

  y <- unique(y)

  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])

    if(length(z)) cbind(x[i], z, deparse.level=0)
  }

  do.call(rbind, lapply(seq_along(x), g))
}
# significance testing
pairs = expand.grid.unique(x = c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vs"),
                           y =  c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vs")) %>%
  as_tibble() %>%
  magrittr::set_colnames(c("var1", "var2")) %>%
  mutate(var1 = paste0("steps_", var1, "_30"),
         var2 = paste0("steps_", var2, "_30")) %>%
  mutate(z = paste(var1, var2))

key = recog_stats_test %>%
  select(algorithm, id_study, id_subject, f1) %>%
  group_by(algorithm, id_study) %>%
  nest(data = c(id_subject, f1))

expanded = expand_grid(pair = pairs$z,id_study= unique(key$id_study)) %>%
  rowwise() %>%
  mutate(pair1 = str_split(pair, " ")[[1]][1],
         pair2 = str_split(pair, " ")[[1]][2])

t_test_res =
  expanded %>%
  select(-pair) %>%
  left_join(key, by = c("pair1" ="algorithm", "id_study" = "id_study")) %>%
  rename(f1_pair1 = data) %>%
  left_join(key, by = c("pair2" ="algorithm", "id_study" = "id_study")) %>%
  rename(f1_pair2 = data) %>%
  unnest() %>%
  group_by(id_study, pair1, pair2) %>%
  nest(data = c(id_subject, id_subject1, f1, f11)) %>%
  mutate(pval = map(data, ~ t.test(.x$f1, .x$f11, data = .x, paired = TRUE)$p.value)) %>%
  unnest(pval) %>%
  mutate(pval_char =as.character(signif(pval,digits=2)))


# p value plot
f1 = t_test_res %>%
  ggplot(aes(x = pair1, y = pair2, fill = pval, label = pval_char))+
  geom_tile()+
  geom_text(parse = TRUE)+
  facet_wrap(.~id_study)+
  theme_bw()+
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn",
               palette = function(x) c("lightgreen", "lightblue", "white", "red"),
               breaks = c(0,0.001, 0.01, 0.05, 1),
               limits = c(0, 1),
               show.limits = TRUE,
               guide = "colorsteps"
  ) +
  scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt")) +
  scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vs"))+
  labs(title = "p values for f1 score")


key = recog_stats_test %>%
  select(algorithm, id_study, id_subject, recall) %>%
  group_by(algorithm, id_study) %>%
  nest(data = c(id_subject, recall))

expanded = expand_grid(pair = pairs$z,id_study= unique(key$id_study)) %>%
  rowwise() %>%
  mutate(pair1 = str_split(pair, " ")[[1]][1],
         pair2 = str_split(pair, " ")[[1]][2])

t_test_res =
  expanded %>%
  select(-pair) %>%
  left_join(key, by = c("pair1" ="algorithm", "id_study" = "id_study")) %>%
  rename(recall_pair1 = data) %>%
  left_join(key, by = c("pair2" ="algorithm", "id_study" = "id_study")) %>%
  rename(recall_pair2 = data) %>%
  unnest() %>%
  group_by(id_study, pair1, pair2) %>%
  nest(data = c(id_subject, id_subject1, recall, recall1)) %>%
  mutate(pval = map(data, ~ t.test(.x$recall, .x$recall1, data = .x, paired = TRUE)$p.value)) %>%
  unnest(pval) %>%
  mutate(pval_char =as.character(signif(pval,digits=2)))


# p value plot
recall = t_test_res %>%
  ggplot(aes(x = pair1, y = pair2, fill = pval, label = pval_char))+
  geom_tile()+
  geom_text(parse = TRUE)+
  facet_wrap(.~id_study)+
  theme_bw()+
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn",
               palette = function(x) c("lightgreen", "lightblue", "white", "red"),
               breaks = c(0, 0.001, 0.01, 0.05, 1),
               limits = c(0, 1),
               show.limits = TRUE,
               guide = "colorsteps"
  ) +
  scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt")) +
  scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vs"))+
  labs(title = "p values for recall score")



key = recog_stats_test %>%
  select(algorithm, id_study, id_subject, prec) %>%
  group_by(algorithm, id_study) %>%
  nest(data = c(id_subject, prec))

expanded = expand_grid(pair = pairs$z,id_study= unique(key$id_study)) %>%
  rowwise() %>%
  mutate(pair1 = str_split(pair, " ")[[1]][1],
         pair2 = str_split(pair, " ")[[1]][2])

t_test_res =
  expanded %>%
  select(-pair) %>%
  left_join(key, by = c("pair1" ="algorithm", "id_study" = "id_study")) %>%
  rename(prec_pair1 = data) %>%
  left_join(key, by = c("pair2" ="algorithm", "id_study" = "id_study")) %>%
  rename(prec_pair2 = data) %>%
  unnest() %>%
  group_by(id_study, pair1, pair2) %>%
  nest(data = c(id_subject, id_subject1, prec, prec1)) %>%
  mutate(pval = map(data, ~ t.test(.x$prec, .x$prec1, data = .x, paired = TRUE)$p.value)) %>%
  unnest(pval) %>%
  mutate(pval_char =as.character(signif(pval,digits=2)))


# p value plot
prec = t_test_res %>%
  ggplot(aes(x = pair1, y = pair2, fill = pval, label = pval_char))+
  geom_tile()+
  geom_text(parse = TRUE)+
  facet_wrap(.~id_study)+
  theme_bw()+
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn",
               palette = function(x) c("lightgreen", "lightblue", "white", "red"),
               breaks = c(0, 0.001, .01, 0.05, 1),
               limits = c(0, 1),
               show.limits = TRUE,
               guide = "colorsteps"
  ) +
  scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt")) +
  scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vs"))+
  labs(title = "p values for prec score")

cowplot::plot_grid(f1, prec, recall)
