library(tidyverse)
options(digits.secs =3)

clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, cat_activity, starts_with("steps"), time_10) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular", "walk_semiregular", "walk_irregular")))


oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, sample_rate,  starts_with("steps"), steps_truth, time_10)


# check to make sure NAs are just happening at end

marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, cat_activity,  starts_with("steps"), steps_truth, time_10) %>%
  filter(grepl("run", cat_activity)==FALSE)


# paletteer::paletteer_d("RColorBrewer::Dark2")

# PLOTS truth vs predicted
labs = c("ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")
names(labs) = c("adept", "oak", "scrf", "sc", "sdt", "vs")

# current figure
dat.labs = c("Clemson (15 Hz)", "MAREA (128 Hz)", "OxWalk (100 Hz)", "OxWalk (25 Hz)")
names(dat.labs) = c("clemson", "marea", "oxwalk100", "oxwalk25")
oxwalk %>%
  filter(sample_rate == 25) %>%
  mutate(id_study = "oxwalk25") %>%
  bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
              mutate(id_study = "oxwalk100")) %>%
  bind_rows(marea %>% mutate(id_study = "marea")) %>%
  bind_rows(clemson %>% mutate(id_study = "clemson")) %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = starts_with("steps") & !contains("truth") & !contains("acti")) %>%
  mutate(sample_rate = ifelse(grepl("30", name), "resampled", "original"),
         method = ifelse(sample_rate == "resampled",
                         sub(".*steps\\_(.+)\\_.*", "\\1", name),
                         sub(".*\\_", "", name))) %>%
  select(-name) %>%
  pivot_wider(names_from = sample_rate, values_from = value) %>%
  ggplot(aes(x = original, y = resampled, col = method))+
  geom_point() +
  facet_grid(id_study ~ method,labeller = labeller(method = labs, id_study = dat.labs))+
  scale_color_manual(values = c("#D95F02FF", "#7570B3FF","#A6761DFF", "#E7298AFF", "#66A61EFF",  "#E6AB02FF"))+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 6200))+
  scale_y_continuous(limits=c(0,6200))+
  theme(legend.position = "none")+
  labs(x = "Total Steps Estimated from Raw Data", y = "Total Steps Estimated from Data Resampled to 30 Hz")+
  coord_equal()+
  geom_text(data = cor_df %>%
              pivot_longer(cols = -method) %>% rename(id_study = name),
            aes(x = 2500, y = 5000,
            label = paste("\u03c1 =", formatC(signif(value, digits=2), digits=2, format="fg", flag="#"))),
            inherit.aes = FALSE)

# corr between totals
cor_df =
  oxwalk %>%
  filter(sample_rate == 25) %>%
  mutate(id_study = "oxwalk25", cat_activity2 = "ox") %>%
  bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
              mutate(id_study = "oxwalk100")) %>%
  bind_rows(marea %>% mutate(id_study = "marea")) %>%
  bind_rows(clemson %>% mutate(id_study = "clemson"))  %>%
  group_by(id_subject, id_study) %>%
  summarize(across(starts_with("steps"), ~sum(.x))) %>%
  pivot_longer(cols = starts_with("steps") & !contains("truth") & !contains("acti")) %>%
  select(id_subject, id_study, name, value) %>%
  mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
         method = ifelse(type == "resampled",
                         sub(".*steps\\_(.+)\\_.*", "\\1", name),
                         sub(".*\\_", "", name))) %>%
  ungroup() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value,
              id_cols = c(id_subject, id_study, method))  %>%
  group_by(id_study, method) %>%
  summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
  ungroup() %>%
  pivot_wider(names_from = id_study, values_from = pearson)

# calc across 10s time periods, ignoring IDs
oxwalk %>%
  filter(sample_rate == 25) %>%
  mutate(id_study = "oxwalk25", cat_activity = "ox") %>%
  bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
              mutate(id_study = "oxwalk100", cat_activity = "ox")) %>%
  bind_rows(marea %>% mutate(id_study = "marea")) %>%
  bind_rows(clemson %>% mutate(id_study = "clemson"))  %>%
  pivot_longer(cols = starts_with("steps") & !contains("truth") & !contains("acti")) %>%
  select(id_subject, id_study, cat_activity, name, value, time_10) %>%
  mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
         method = ifelse(type == "resampled",
                         sub(".*steps\\_(.+)\\_.*", "\\1", name),
                         sub(".*\\_", "", name))) %>%
  ungroup() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value,
              id_cols = c(id_subject, id_study, cat_activity, method, time_10))  %>%
  group_by(id_study, method) %>%
  summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
  ungroup() %>%
  pivot_wider(names_from = id_study, values_from = pearson)

# corr by 10s period by id, then take mean
oxwalk %>%
  filter(sample_rate == 25) %>%
  mutate(id_study = "oxwalk25", cat_activity = "ox") %>%
  bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
              mutate(id_study = "oxwalk100", cat_activity = "ox")) %>%
  bind_rows(marea %>% mutate(id_study = "marea")) %>%
  bind_rows(clemson %>% mutate(id_study = "clemson"))  %>%
  pivot_longer(cols = starts_with("steps") & !contains("truth") & !contains("acti")) %>%
  select(id_subject, id_study, cat_activity, name, value, time_10) %>%
  mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
         method = ifelse(type == "resampled",
                         sub(".*steps\\_(.+)\\_.*", "\\1", name),
                         sub(".*\\_", "", name))) %>%
  ungroup() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value,
              id_cols = c(id_subject, id_study, cat_activity, method, time_10))  %>%
  group_by(id_study, method, id_subject) %>%
  summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
  ungroup() %>%
  group_by(id_study, method) %>%
  summarize(pearson = mean(pearson, na.rm = TRUE)) %>%
  pivot_wider(names_from = id_study, values_from = pearson)


# correlation table, between total stepsestimated for ea person
oxwalk %>%
  filter(sample_rate == 25) %>%
  mutate(id_study = "oxwalk25", cat_activity2 = "ox") %>%
  bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
              mutate(id_study = "oxwalk100", cat_activity2 = "ox")) %>%
  bind_rows(marea %>% mutate(id_study = "marea", cat_activity2 = "marea")) %>%
  bind_rows(clemson %>% mutate(id_study = "clemson", cat_activity2= cat_activity))  %>%
  group_by(id_subject, cat_activity2, id_study) %>%
  summarize(across(starts_with("steps"), ~sum(.x))) %>%
  pivot_longer(cols = starts_with("steps") & !contains("truth") & !contains("acti")) %>%
  select(id_subject, id_study, cat_activity2, name, value) %>%
  mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
         method = ifelse(type == "resampled",
                         sub(".*steps\\_(.+)\\_.*", "\\1", name),
                         sub(".*\\_", "", name))) %>%
  ungroup() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value,
              id_cols = c(id_subject, cat_activity2, id_study, method))  %>%
  group_by(cat_activity2, id_study, method) %>%
  summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
  ungroup() %>%
  mutate(id = paste0(cat_activity2, id_study)) %>%
  select(-cat_activity2, -id_study) %>%
  pivot_wider(names_from = id, values_from = pearson) %>%
  select(method, reg = walk_regularclemson, semi = walk_semiregularclemson,
         irr = walk_irregularclemson, marea = mareamarea, ox25 = oxoxwalk25, ox100 = oxoxwalk100) %>%
  mutate(method = c("ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  arrange(method) %>%
  magrittr::set_colnames(c("Algorithm", "Clemson Regular", "Clemson Semiregular", "Clemson Irregular", "MAREA",
                           "OxWalk 25Hz", "OxWalk 100Hz")) %>%
  kableExtra::kable(digits = 2, align = "llll",format = "latex", booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "scale_down")




