library(tidyverse)
options(digits.secs =3)
# values = c("#990F0FFF","#CC5151FF",
#            "#99540FFF","#CC8E51FF",
#            "#6B990FFF", "#A3CC51FF",
#            "#0F6B99FF", "#51A3CCFF",
#            "#260F99FF", "#6551CCFF")

# load all results data
clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, cat_activity, starts_with("steps"), time_10) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular", "walk_semiregular", "walk_irregular")))


oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, sample_rate,  starts_with("steps"), steps_truth, time_10)


# check to make sure NAs are just happening at end

marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, cat_activity,  starts_with("steps"), steps_truth, time_10) %>%
  filter(grepl("run", cat_activity)==FALSE)


paletteer::paletteer_d("ggthemes::Hue_Circle")
col1 = "#1BA3C6FF"; col2 = "#F06719FF"; col3 = "#33A65CFF"
col4  = "#A26DC2FF"; col5 = "#FC719EFF"; col6 = "#F8B620FF"

paletteer::paletteer_d("RColorBrewer::Dark2")

# PLOTS truth vs predicted
labs = c("ADEPT", "Oak", "Stepcount", "SDT", "Verisense")
names(labs) = c("adept", "oak", "sc", "sdt", "vs")
# clemson %>%
#   filter(cat_activity == "walk_regular") %>%
#   group_by(id_subject) %>%
#   summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
#   pivot_longer(cols = starts_with("steps") & !contains("truth") & !contains("acti")) %>%
#   mutate(sample_rate = ifelse(grepl("30", name), "resampled", "original"),
#          method = ifelse(sample_rate == "resampled",
#                             sub(".*steps\\_(.+)\\_.*", "\\1", name),
#                             sub(".*\\_", "", name))) %>%
#   select(-name) %>%
#   pivot_wider(names_from = sample_rate, values_from = value) %>%
#   ggplot(aes(x = original, y = resampled, col = method))+
#   geom_point() +
#   facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
#   scale_color_brewer(palette = "Dark2")+
#   theme_bw()+
#   geom_abline()+
#   scale_x_continuous(limits = c(0, 1250))+
#   scale_y_continuous(limits=c(0,1250))+
#   theme(legend.position = "none")+
#   labs(x = "Predicted Steps: Raw Data (15 Hz)", y = "Predicted Steps: Resampled Data (30 Hz)", title = "Clemson: Regular Walking")+
#   coord_equal()

clem = clemson %>%
  group_by(id_subject, cat_activity) %>%
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
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1250))+
  scale_y_continuous(limits=c(0,1250))+
  theme(legend.position = "none")+
  labs(x = " Raw Data (15 Hz)", y = " Resampled Data (30 Hz)", title = "Clemson")+
  coord_equal()

mar = marea %>%
  group_by(id_subject, cat_activity) %>%
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
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1600))+
  scale_y_continuous(limits=c(0,1600))+
  theme(legend.position = "none")+
  labs(x = " Raw Data (128 Hz)", y = " Resampled Data (30 Hz)", title = "MAREA")+
  coord_equal()

ox100 = oxwalk %>%
  filter(sample_rate == 100) %>%
  group_by(id_subject) %>%
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
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 6000))+
  scale_y_continuous(limits=c(0,6000))+
  theme(legend.position = "none")+
  labs(x = " Raw Data (100 Hz)", y = " Resampled Data (30 Hz)", title = "Oxwalk 100 Hz")+
  coord_equal()

ox25 = oxwalk %>%
  filter(sample_rate == 25) %>%
  group_by(id_subject) %>%
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
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 6200))+
  scale_y_continuous(limits=c(0,6200))+
  theme(legend.position = "none")+
  labs(x = " Raw Data (25 Hz)", y = " Resampled Data (30 Hz)", title = "Oxwalk 25 Hz")+
  coord_equal()

cowplot::plot_grid(clem, mar, ox100, ox25)
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
  facet_grid(method~id_study,labeller = labeller(method = labs, id_study = dat.labs))+
  scale_color_manual(values = c("#D95F02FF", "#7570B3FF", "#E7298AFF","#66A61EFF",  "#E6AB02FF"))+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 6200))+
  scale_y_continuous(limits=c(0,6200))+
  theme(legend.position = "none")+
  labs(x = "Raw Data", y = " Resampled Data (30 Hz)")+
  coord_equal()


# calculate ICCs - I don't think this way is correct actually
tmp = oxwalk %>%
  filter(sample_rate == 25) %>%
  mutate(id_study = "oxwalk25", cat_activity2 = "ox") %>%
  bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
              mutate(id_study = "oxwalk100", cat_activity2 = "ox")) %>%
  bind_rows(marea %>% mutate(id_study = "marea", cat_activity2 = "marea")) %>%
  bind_rows(clemson %>% mutate(id_study = "clemson", cat_activity2= cat_activity))  %>%
  pivot_longer(cols = starts_with("steps") & !contains("truth") & !contains("acti")) %>%
  select(id_subject, id_study, cat_activity,cat_activity2, name, value, time_10) %>%
  mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
         method = ifelse(type == "resampled",
                         sub(".*steps\\_(.+)\\_.*", "\\1", name),
                         sub(".*\\_", "", name))) %>%
  ungroup() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value,
              id_cols = c(time_10, id_subject, cat_activity, cat_activity2, id_study, method)) %>%
  select(-id_subject, -time_10, -cat_activity) %>%
  nest(df = c(raw, resampled)) %>%
  mutate(df = map_dbl(df, ~irr::icc(.x)$value))

tmp %>%
  pivot_wider(names_from = c(id_study, cat_activity2), values_from = df) %>%
  select(method, clemson_walk_regular, clemson_walk_semiregular, clemson_walk_irregular, marea_marea, oxwalk25_ox,
         oxwalk100_ox) %>%
  mutate(method = c("ADEPT", "Oak", "SDT", "Verisense", "Stepcount")) %>%
  arrange(method) %>%
  magrittr::set_colnames(c("Algorithm", "Clemson Regular", "Clemson Semiregular", "Clemson Irregular", "MAREA",
                           "OxWalk 25Hz", "OxWalk 100Hz")) %>%
  kableExtra::kable(digits = 2, align = "llll",format = "latex", booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "scale_down")


# first sum steps then get ICC
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
  select(-id_subject) %>%
  nest(df = c(raw, resampled)) %>%
  mutate(df = map_dbl(df, ~irr::icc(.x)$value)) %>%
  pivot_wider(names_from = c(id_study, cat_activity2), values_from = df) %>%
  select(method, clemson_walk_regular, clemson_walk_semiregular, clemson_walk_irregular, marea_marea, oxwalk25_ox,
         oxwalk100_ox) %>%
  mutate(method = c("ADEPT", "Oak", "SDT", "Verisense", "Stepcount")) %>%
  arrange(method) %>%
  magrittr::set_colnames(c("Algorithm", "Clemson Regular", "Clemson Semiregular", "Clemson Irregular", "MAREA",
                           "OxWalk 25Hz", "OxWalk 100Hz")) %>%
  kableExtra::kable(digits = 2, align = "llll",format = "latex", booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "scale_down")


# report correlation instead of ICC
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


clemson %>%
  filter(cat_activity == "walk_irregular") %>%
  select(steps_sc, steps_sc_30) %>%
  irr::icc(type = "agreement", model = "twoway")

oxwalk %>%
  filter(sample_rate == 25) %>%
  group_by(id_subject) %>%
  summarize(across(c(steps_vs, steps_vs_30), ~sum(.x))) %>%
  select(steps_vs, steps_vs_30) %>%
  irr::icc()

# DescTools::ICC(tmp[[4]][[1]])

