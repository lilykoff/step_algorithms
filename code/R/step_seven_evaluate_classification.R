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

# load all results data

clemson = readr::read_csv(here::here("results/clemson_steps_1sec.csv")) %>%
  select(-"steps_truth_30")
ox = readr::read_csv(here::here("results/ox_steps_1sec.csv")) %>%
  select(-"steps_truth_30")

ox10 = readr::read_csv(here::here("results/ox_steps_10sec.csv")) %>%
  select(-"steps_truth_30")


marea10 = readr::read_csv(here::here("results/marea_steps_1sec.csv")) %>%
  filter(grepl("run", cat_activity) == FALSE & cat_activity != "treadmill_slopewalk")



paletteer::paletteer_d("ggthemes::Hue_Circle")
col1 = "#1BA3C6FF"; col2 = "#F06719FF"; col3 = "#33A65CFF"
col4  = "#A26DC2FF"; col5 = "#FC719EFF"; col6 = "#F8B620FF"

# for primary analysis, just use clemson at 10 sec level, 30hz
# ox at 10 sec level, 100 hz original, resampled to 30 hz

clemson = clemson %>%
  select(time, id_study, id_subject, cat_activity, ends_with("30"),
         steps_truth) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular", "walk_semiregular", "walk_irregular")))

ox = ox %>%
  filter(sample_rate == 100) %>%
  select(time, id_study, id_subject,  ends_with("30"),
         steps_truth)

marea10 = marea10 %>%
  select(time, id_study, id_subject,  ends_with("30"),
         steps_truth, cat_activity)

# full latex table, way # 1 (averaging across individuals), w all 4 scenarios
ox %>%
  mutate(cat_activity = "Oxwalk") %>%
  bind_rows(marea10 %>% mutate(cat_activity = "MAREA", id_subject = as.character(id_subject))) %>%
  bind_rows(clemson) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("MAREA", "Oxwalk", "walk_regular",
                                                        "walk_semiregular", "walk_irregular"))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = -c(id_subject, cat_activity))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, cat_activity, algorithm)) %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn)))) %>%
  ungroup() %>%
  group_by(algorithm, cat_activity) %>%
  summarize(across(c(acc, recall, prec, f1), ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_wider(names_from = cat_activity, values_from = c(acc, recall, prec, f1)) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  magrittr::set_colnames(c("Algorithm", rep(c("MAREA", "Oxwalk", "Regular", "Semiregular", "Irregular"), 4))) %>%
  kableExtra::kable(digits = 2, align = "llll", booktabs = TRUE) %>%
  kableExtra::add_header_above(c(" " = 1, "Accuracy" = 5, "Recall" = 5, "Precision" = 5, "F1" = 5)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")


# latex table, separated by study type, with mean and SD
ox %>%
  bind_rows(clemson) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_study, id_subject) %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name))  %>%
 pivot_wider(names_from = metric, values_from = value,
            id_cols = c(id_subject, id_study, algorithm)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  group_by(algorithm, id_study) %>%
  summarize(across(c(acc, recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  mutate(acc = paste0(round(acc_mean, 2), " (", round(acc_sd, 2), ")"),
         recall = paste0(round(recall_mean, 2), " (", round(recall_sd, 2), ")"),
         precision = paste0(round(prec_mean, 2), " (", round(prec_sd, 2), ")"),
         f1 = paste0(round(f1_mean, 2), " (", round(f1_sd, 2), ")")) %>%
  select(algorithm, id_study, acc, recall, precision, f1) %>%
  pivot_wider(names_from = id_study, values_from = acc:f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll",booktabs = TRUE, col.names =
                      c("Algorithm", rep(c("Clemson", "Oxwalk"), 4))) %>%
  kableExtra::add_header_above(c(" " = 1, "Accuracy" = 2, "Recall" = 2,
                                 "Precision" = 2, "F1 Score" = 2)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# latex table, separated by study type, with mean and SD - means by trial type
# currently what I have in paper
ox %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_study, id_subject, cat_activity) %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name))  %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, id_study, algorithm, cat_activity)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  group_by(algorithm, id_study) %>%
  summarize(across(c(acc, recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  mutate(acc = paste0(round(acc_mean, 2), " (", round(acc_sd, 2), ")"),
         recall = paste0(round(recall_mean, 2), " (", round(recall_sd, 2), ")"),
         precision = paste0(round(prec_mean, 2), " (", round(prec_sd, 2), ")"),
         f1 = paste0(round(f1_mean, 2), " (", round(f1_sd, 2), ")")) %>%
  select(algorithm, id_study, acc, recall, precision, f1) %>%
  pivot_wider(names_from = id_study, values_from = acc:f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll",booktabs = TRUE, col.names =
                      c("Algorithm", rep(c("Clemson", "Oxwalk"), 4))) %>%
  kableExtra::add_header_above(c(" " = 1, "Accuracy" = 2, "Recall" = 2,
                                 "Precision" = 2, "F1 Score" = 2)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# n (%) with F1 > 0.75
ox %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_study, id_subject, cat_activity) %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name))  %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, id_study, algorithm, cat_activity)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  group_by(algorithm, id_study) %>%
  summarize(across(f1,
                   list(n = ~sum(.x > 0.75),
                        pct = ~sum(.x > 0.75)/n()))) %>%
  mutate(f1 = paste0(f1_n, " (", round(f1_pct*100, 1), ")")) %>%
  select(algorithm, id_study, f1) %>%
  pivot_wider(names_from = id_study, values_from = f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll", format = "latex", booktabs = TRUE, col.names =
                      c("Algorithm", rep(c("Clemson", "Oxwalk"), 1))) %>%
  kableExtra::add_header_above(c(" " = 1, "n (%) with F1 Score> 0.75" = 2)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# above info as figure
labs = c("Accuracy", "F1", "Precision", "Recall")
names(labs) = c("acc", "f1", "prec", "recall")
ox %>%
  bind_rows(clemson) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(id_study, id_subject) %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name))  %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, id_study, algorithm)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  group_by(algorithm, id_study) %>%
  summarize(across(c(acc, recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = -c(algorithm, id_study)) %>%
  mutate(measure = sub(".*\\_", "", name),
         metric = sub("\\_.*", "", name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(lb = mean - 1.96*sd, ub = mean + 1.96*sd) %>%
  ggplot() +
  geom_point(aes(x = algorithm,y = mean, color = id_study), position = position_dodge(0.5))+
  geom_errorbar(aes(x = algorithm, ymin = lb, ymax = ub, color = id_study),
                width = .2, position = position_dodge(0.5)) +
  facet_wrap(.~metric, labeller=labeller(metric = labs)) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "", labels = c("Clemson", "OxWalk"))+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme(legend.position = "bottom",
        axis.title = element_blank())

labs = c("Accuracy", "F1", "Precision", "Recall")
names(labs) = c("acc", "f1", "prec", "recall")
ox %>%
  mutate(cat_activity = "Oxwalk") %>%
  bind_rows(clemson) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("Oxwalk", "walk_regular", "walk_semiregular", "walk_irregular"))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(cat_activity, id_subject) %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name))  %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(id_subject, cat_activity, algorithm)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn))))  %>%
  ungroup() %>%
  group_by(algorithm, cat_activity) %>%
  summarize(across(c(acc, recall, prec, f1),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = -c(algorithm, cat_activity)) %>%
  mutate(measure = sub(".*\\_", "", name),
         metric = sub("\\_.*", "", name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(lb = mean - 1.96*sd, ub = mean + 1.96*sd) %>%
  ggplot() +
  geom_point(aes(x = algorithm,y = mean, color = cat_activity), position = position_dodge(0.5))+
  geom_errorbar(aes(x = algorithm, ymin = lb, ymax = ub, color = cat_activity),
                width = .2, position = position_dodge(0.5)) +
  facet_wrap(.~metric, labeller=labeller(metric = labs)) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "", labels = c("Oxwalk", "Clemson Regular", "Clemson Semiregular",
                                                              "Clemson Irregular"))+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme(legend.position = "bottom",
        axis.title = element_blank())






# latex table, pooling all walking types to get one metric
ox %>%
  mutate(cat_activity = "Oxwalk") %>%
  bind_rows(clemson) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("Oxwalk", "walk_regular",
                                                        "walk_semiregular", "walk_irregular"))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  ungroup() %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols =  algorithm) %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn)))) %>%
  select(-c(tp, tn, fp, fn)) %>%
  ungroup() %>%
  mutate(algorithm = c("Oak", "Verisense", "ADEPT", "SDT", "ActiLife", "Stepcount")) %>%
  magrittr::set_colnames(c("Algorithm", "Accuracy", "Recall", "Precision", "F1 Score")) %>%
  kableExtra::kable(digits = 2, align = "llll", booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# averages, way 2
ox %>%
  mutate(cat_activity = "Oxwalk") %>%
  bind_rows(clemson) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("Oxwalk", "walk_regular",
                                                        "walk_semiregular", "walk_irregular"))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  ungroup() %>%
  group_by(cat_activity) %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = starts_with("steps"))  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols =  c(cat_activity, algorithm)) %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn)))) %>%
  select(-c(tp, tn, fp, fn)) %>%
  ungroup() %>%
  group_by(algorithm) %>%
  summarize(across(c(acc, recall, prec, f1),
                   ~mean(.x))) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  magrittr::set_colnames(c("Algorithm", "Accuracy", "Recall", "Precision", "F1 Score")) %>%
  kableExtra::kable(digits = 2, align = "llll", booktabs = TRUE, format = "latex") %>%
  kableExtra::kable_styling(latex_options = "scale_down")


# old way
# full latex table, way # 2 (pooling then averaging)
ox %>%
  mutate(cat_activity = "Oxwalk") %>%
  bind_rows(clemson) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("Oxwalk", "walk_regular",
                                                        "walk_semiregular", "walk_irregular"))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(.x > 0, 1, 0))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(type = ~ case_when(
                  (.x > 0 & steps_truth > 0) ~ "tp",
                  (.x > 0 & steps_truth == 0) ~ "fp",
                  (.x == 0 & steps_truth == 0) ~ "tn",
                  (.x == 0 & steps_truth > 0) ~ "fn"
                )))) %>%
  group_by(cat_activity) %>%
  summarize(across(ends_with("type"),
                   list(tp = ~sum(.x == "tp"),
                        tn = ~sum(.x == "tn"),
                        fp = ~sum(.x == "fp"),
                        fn = ~sum(.x == "fn")))) %>%
  pivot_longer(cols  = -cat_activity)  %>%
  mutate(metric = sub(".*type\\_", "", name),
         algorithm = sub("\\_type\\_.*", "", name)) %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(cat_activity, algorithm)) %>%
  rowwise() %>%
  mutate(acc = (tp + tn)/(tp + tn + fp + fn),
         recall = tp /(tp + fn),
         prec = tp / (tp + fp),
         f1 = tp/(tp + (0.5*(fp + fn)))) %>%
  select(-c(tp, tn, fp, fn)) %>%
  pivot_wider(names_from = cat_activity, values_from = c(acc, recall, prec, f1)) %>%
  mutate(algorithm = c("Oak", "Verisense", "ADEPT", "SDT", "ActiLife", "Stepcount")) %>%
  arrange(algorithm) %>%
  magrittr::set_colnames(c("Algorithm", rep(c("Oxwalk", "Regular", "Semiregular", "Irregular"), 4))) %>%
  kableExtra::kable(digits = 2, align = "llll", booktabs = TRUE, format = "latex") %>%
  kableExtra::add_header_above(c(" " = 1, "Accuracy" = 4, "Recall" = 4,
                                 "Precision" = 4, "F1 Score" = 4)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

