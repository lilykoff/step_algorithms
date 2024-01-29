# evaluate accuracy w/ stepcount
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

clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_10sec.csv.gz"))

# check to make sure NAs are just happening at end
clemson %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time_10)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  filter(position != 1)

# get rid of periods w/o 10 full seconds
clemson = clemson %>%
  filter(n_seconds == 10)

oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_10sec.csv.gz"))
# for accuracy, we can't use stepcount
# for now just use 100 hz
oxwalk = oxwalk %>%
  filter(sample_rate == 100)

# check to make sure NAs are just happening at end
oxwalk %>%
  group_by(id_subject) %>%
  arrange(desc(time_10)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  filter(position != 1)


oxwalk = oxwalk %>%
  filter(n_seconds == 10)

marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_10sec.csv.gz"))


# check to make sure NAs are just happening at end
marea %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time_10)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  select(time_10, steps_truth, steps_oak, steps_oak_30, steps_acti_30, position) %>%
  filter(position != 1)

# now get rid of last row
marea = marea %>% filter(n_seconds==10)

# generate a table 1 with study descriptions

clemson = clemson %>%
  select(time_10, id_subject, cat_activity, ends_with("30"),
         steps_truth) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular", "walk_semiregular", "walk_irregular")))

oxwalk = oxwalk %>%
  select(time_10, id_subject,  ends_with("30"),
         steps_truth)

marea = marea %>%
  select(time_10, id_subject,  ends_with("30"),
         steps_truth, cat_activity) # and get rid of running
marea = marea %>%
  filter(grepl("run", cat_activity)==FALSE)

# walking recognition table

# first do accuracy table where counts if any steps in second
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
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(round(recall_mean, 2), " (", round(recall_sd, 2), ")"),
         precision = paste0(round(prec_mean, 2), " (", round(prec_sd, 2), ")"),
         f1 = paste0(round(f1_mean, 2), " (", round(f1_sd, 2), ")")) %>%
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

df2 =
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
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(round(recall_mean, 2), " (", round(recall_sd, 2), ")"),
         precision = paste0(round(prec_mean, 2), " (", round(prec_sd, 2), ")"),
         f1 = paste0(round(f1_mean, 2), " (", round(f1_sd, 2), ")")) %>%
  select(algorithm, cat_activity, recall, precision, f1) %>%
  pivot_wider(names_from = cat_activity, values_from = recall:f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  select(algorithm, starts_with("f1"))


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
                  (.x >= 5 & steps_truth >= 5) ~ "tp",
                  (.x >= 5 & steps_truth < 5) ~ "fp",
                  (.x < 5 & steps_truth < 5) ~ "tn",
                  (.x < 5 & steps_truth >= 5) ~ "fn"
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
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(round(recall_mean, 2), " (", round(recall_sd, 2), ")"),
         precision = paste0(round(prec_mean, 2), " (", round(prec_sd, 2), ")"),
         f1 = paste0(round(f1_mean, 2), " (", round(f1_sd, 2), ")")) %>%
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

# just f1 scores
df1 = oxwalk %>%
  mutate(cat_activity = "ox") %>%
  bind_rows(clemson) %>%
  bind_rows(marea %>% mutate(cat_activity = "marea")) %>%
  mutate(cat_activity = factor(cat_activity,
                               levels = c("walk_regular", "walk_semiregular",
                                          "walk_irregular", "marea", "ox"))) %>%
  # classify each second as true positive/neg or false positive/neg
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x >= 5 & steps_truth >= 5) ~ "tp",
                  (.x >= 5 & steps_truth < 5) ~ "fp",
                  (.x < 5 & steps_truth < 5) ~ "tn",
                  (.x < 5 & steps_truth >= 5) ~ "fn"
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
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  # formatting
  mutate(recall = paste0(round(recall_mean, 2), " (", round(recall_sd, 2), ")"),
         precision = paste0(round(prec_mean, 2), " (", round(prec_sd, 2), ")"),
         f1 = paste0(round(f1_mean, 2), " (", round(f1_sd, 2), ")")) %>%
  select(algorithm, cat_activity, recall, precision, f1) %>%
  pivot_wider(names_from = cat_activity, values_from = recall:f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  select(algorithm, starts_with("f1"))

left_join(df1, df2, by = "algorithm") %>%
  kableExtra::kable(align = "llll", booktabs = TRUE, format = "latex", col.names =
                      c("Algorithm", rep(c("Regular", "Semiregular", "Irregular", "Regular", "Free-Living"), 2))) %>%
  kableExtra::add_header_above(c(" " = 1, "Clemson" = 3,
                                 "MAREA" = 1, "OxWalk" = 1, "Clemson" = 3,
                                 "MAREA" = 1, "OxWalk" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "F1 Score: Threshold: >0 Steps" = 5,
                                 "F1 Score: Threshold: >=5 Steps" = 5)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")


recog_stats =
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
  ungroup()

recog_mean =
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
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = ends_with("mean")) %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm))


labs2 = c("Clemson - Regular", "Clemson - Semiregular", "Clemson - Irregular", "MAREA", "Oxwalk")
names(labs2) = c("walk_regular", "walk_semiregular", "walk_irregular", "marea", "ox")

p1 = recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  ggplot(aes(x = algorithm, y = f1, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.5, size = .7)+
  facet_grid(. ~ cat_activity,
             labeller = labeller(cat_activity = labs2))+
  # scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
  #                    labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  scale_color_brewer(palette = "Dark2")+
  labs(x = "", y = "Value")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) +
  geom_text(data = recog_mean %>%
              filter(name == "f1_mean"),
            aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)

recog_stats5 =
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
                  (.x >= 5 & steps_truth >= 5) ~ "tp",
                  (.x >= 5 & steps_truth < 5) ~ "fp",
                  (.x < 5 & steps_truth < 5) ~ "tn",
                  (.x < 5 & steps_truth >= 5) ~ "fn"
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
  ungroup()

recog_mean5 =
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
                  (.x >= 5 & steps_truth >= 5) ~ "tp",
                  (.x >= 5 & steps_truth < 5) ~ "fp",
                  (.x < 5 & steps_truth < 5) ~ "tn",
                  (.x < 5 & steps_truth >= 5) ~ "fn"
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
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = ends_with("mean")) %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm))


labs2 = c("Clemson - Regular", "Clemson - Semiregular", "Clemson - Irregular", "MAREA", "Oxwalk")
names(labs2) = c("walk_regular", "walk_semiregular", "walk_irregular", "marea", "ox")

p2 = recog_stats5 %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  ggplot(aes(x = algorithm, y = f1, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.5, size = .7)+
  facet_grid(. ~ cat_activity,
             labeller = labeller(cat_activity = labs2))+
  # scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
  #                    labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  scale_color_brewer(palette = "Dark2")+
  labs(x = "", y = "Value")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) +
  geom_text(data = recog_mean5 %>%
              filter(name == "f1_mean"),
            aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)

cowplot::plot_grid(p1, p2)
df <- as.data.frame(rbind(c(1,  30, 40, 6,  8,  12, 10),
                          c(2,  15, 12, 9,  13, 7,  7),
                          c(3,  20, 22, 11, 12, 9,  10)))

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

pairs = expand.grid.unique(x = c("acti", "adept", "oak", "sc", "sdt", "vs"),
                           y =  c("acti", "adept", "oak", "sc", "sdt", "vs")) %>%
  as_tibble() %>%
  magrittr::set_colnames(c("var1", "var2")) %>%
  mutate(var1 = paste0("steps_", var1, "_30"),
         var2 = paste0("steps_", var2, "_30")) %>%
  mutate(z = paste(var1, var2))
# expand.grid.unique(
#   x = c("acti", "adept", "oak", "sc", "sdt", "vs"),
#   y =  c("acti", "adept", "oak", "sc", "sdt", "vs")
# ) %>%
#   as_tibble() %>%
#   magrittr::set_colnames(c("var1", "var2")) %>%
#   mutate(var1 = paste0("steps_", var1, "_30"),
#          var2 = paste0("steps_", var2, "_30")) %>%
#   left_join(
#     recog_stats %>% select(algorithm, f1, id_subject),
#     by = c("var1" = "algorithm"),
#     relationship = "many-to-many"
#   ) %>%
#   rename(f1_var1 = f1) %>%
#   left_join(
#     recog_stats %>% select(algorithm, f1, id_subject),
#     by = c("var2" = "algorithm", "id_subject" = "id_subject"),
#     relationship = "many-to-many"
#   ) %>%
#   nest(data = c(id_subject, f1_var1, f1)) %>%
#   mutate(t_tests = map(data, ~ t.test(
#     .x$f1, .x$f1_var1, data = .x, paired = TRUE
#   )$p.value)) %>%
#   unnest(t_tests)

# t.test(recog_stats %>% filter(algorithm == "steps_acti_30" & cat_activity == "walk_regular") %>% pull(f1),
#        recog_stats %>% filter(algorithm == "steps_oak_30"& cat_activity == "walk_regular") %>% pull(f1),
#        paired = TRUE)


key = recog_stats %>%
  select(algorithm, cat_activity, id_subject, f1) %>%
  group_by(algorithm, cat_activity) %>%
  nest(data = c(id_subject, f1))

expanded = expand_grid(pair = pairs$z,cat_activity= unique(key$cat_activity)) %>%
  rowwise() %>%
  mutate(pair1 = str_split(pair, " ")[[1]][1],
         pair2 = str_split(pair, " ")[[1]][2])

t_test_res =
  expanded %>%
  select(-pair) %>%
  left_join(key, by = c("pair1" ="algorithm", "cat_activity" = "cat_activity")) %>%
  rename(f1_pair1 = data) %>%
  left_join(key, by = c("pair2" ="algorithm", "cat_activity" = "cat_activity")) %>%
  rename(f1_pair2 = data) %>%
  unnest() %>%
  group_by(cat_activity, pair1, pair2) %>%
  nest(data = c(id_subject, id_subject1, f1, f11)) %>%
  mutate(pval = map(data, ~ t.test(.x$f1, .x$f11, data = .x, paired = TRUE)$p.value)) %>%
  unnest(pval) %>%
  mutate(pval_char =as.character(signif(pval,digits=2)))

t_test_res %>%
  ggplot(aes(x = pair1, y = pair2, fill = pval, label = pval_char))+
  geom_tile()+
  geom_text(parse = TRUE)+
  facet_wrap(.~cat_activity)+
  theme_bw()+
  scale_fill_stepsn(colors = c("blue", "white", "red"),
                    breaks = c(0, 0.01, 0.05, 1))

# p value plot
t_test_res %>%
  ggplot(aes(x = pair1, y = pair2, fill = pval, label = pval_char))+
  geom_tile()+
  geom_text(parse = TRUE)+
  facet_wrap(.~cat_activity)+
  theme_bw()+
  binned_scale(aesthetics = "fill",
             scale_name = "stepsn",
             palette = function(x) c("blue", "white", "red"),
             breaks = c(0, 0.01, 0.05, 1),
             limits = c(0, 1),
             show.limits = TRUE,
             guide = "colorsteps"
)+
  scale_x_discrete(labels = c("actilife", "adept", "oak", "sc", "sdt", "vs")) +
  scale_y_discrete(labels = c( "adept", "oak", "sc", "sdt", "vs"))

