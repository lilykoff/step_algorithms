library(tidyverse)
options(digits.secs =3)

clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))

# now get rid of last row
clemson = clemson %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position)

oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))


oxwalk = oxwalk %>%
  group_by(id_subject, sample_rate) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position)

marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz"))


# now get rid of last row
marea = marea %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position) %>%
  filter(grepl("run", cat_activity)==FALSE)




# paletteer::paletteer_d("RColorBrewer::Dark2")

# PLOTS truth vs predicted
labs = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")
names(labs) = c("acti", "adept", "oak", "scrf", "sc", "sdt", "vs")

# current figure
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
  pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
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
  pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
  mutate(sample_rate = ifelse(grepl("30", name), "resampled", "original"),
         method = ifelse(sample_rate == "resampled",
                         sub(".*steps\\_(.+)\\_.*", "\\1", name),
                         sub(".*\\_", "", name))) %>%
  select(-name) %>%
  pivot_wider(names_from = sample_rate, values_from = value) %>%
  mutate(resampled =
           ifelse(method == "acti" & (id_study == "oxwalk25" | id_study == "clemson"), NA, resampled)) %>%
  ggplot(aes(x = original, y = resampled, col = method))+
  geom_point() +
  facet_grid(id_study ~ method,labeller = labeller(method = labs, id_study = dat.labs))+
  scale_color_brewer(palette = "Dark2")+
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
            label = paste("\u03c1 =", formatC(signif(value, digits=3), digits=2, format="fg", flag="#"))),
            inherit.aes = FALSE)
# formatC(signif(value, digits=3), digits=2, format="fg", flag="#"

# correlation table, between total steps estimated for ea person
oxwalk %>%
  filter(sample_rate == 25) %>%
  mutate(id_study = "oxwalk25", cat_activity2 = "ox") %>%
  bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
              mutate(id_study = "oxwalk100", cat_activity2 = "ox")) %>%
  bind_rows(marea %>% mutate(id_study = "marea", cat_activity2 = "marea")) %>%
  bind_rows(clemson %>% mutate(id_study = "clemson", cat_activity2= cat_activity))  %>%
  group_by(id_subject, cat_activity2, id_study) %>%
  summarize(across(starts_with("steps"), ~sum(.x))) %>%
  pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
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



# corr by 10s period by id, then take mean
oxwalk %>%
  filter(sample_rate == 25) %>%
  mutate(id_study = "oxwalk25", cat_activity = "ox") %>%
  bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
              mutate(id_study = "oxwalk100", cat_activity = "ox")) %>%
  bind_rows(marea %>% mutate(id_study = "marea")) %>%
  bind_rows(clemson %>% mutate(id_study = "clemson"))  %>%
  pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
  select(id_subject, id_study, cat_activity, name, value, time) %>%
  mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
         method = ifelse(type == "resampled",
                         sub(".*steps\\_(.+)\\_.*", "\\1", name),
                         sub(".*\\_", "", name))) %>%
  ungroup() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value,
              id_cols = c(id_subject, id_study, cat_activity, method, time))  %>%
  group_by(id_study, method, id_subject) %>%
  summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
  ungroup() %>%
  group_by(id_study, method) %>%
  summarize(pearson = mean(pearson, na.rm = TRUE)) %>%
  pivot_wider(names_from = id_study, values_from = pearson)



# bland altman

summary_df =
  clemson %>%
  group_by(id_subject) %>%
  select(id_subject, contains("truth"), contains("sc")) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(difference = ~steps_truth - .x,
                     truth = ~steps_truth))) %>%
  select(id_subject, steps_truth, ends_with("difference")) %>%
  pivot_longer(cols = ends_with("difference")) %>%
  rowwise() %>%
  mutate(type = ifelse(strsplit(name, "_")[[1]][3]=="30", "resampled", "raw"),
         algorithm = strsplit(name, "_")[[1]][2]) %>%
  select(-name)
labs = c("Stepcount (RF)", "Stepcount (SSL)")
names(labs) = c("scrf", "scssl")

means = summary_df  %>%
  group_by(algorithm) %>%
  summarize(mean = mean(value),
            sd  = sd(value),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

ggplot(summary_df) +
  geom_point(aes(x = steps_truth, y = value, col = type))+
  facet_wrap(.~algorithm, labeller = labeller(algorithm = labs)) +
  # geom_hline(data = means, aes(yintercept = mean), col = "#4F7CBAFF")+
  # geom_hline(data = means, aes(yintercept = ub), col = "#2CB5C0FF")+
  # geom_hline(data = means, aes(yintercept = lb), col = "#2CB5C0FF")+
  theme_bw()+
  geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
  labs(x = "True Steps ",
       y = "Difference (True Steps - Predicted Steps)",
       title = "")+
  scale_color_manual(values = c("#5773CC", "#FFB900"),
                     name = "", labels = c("Raw Data (15 Hz)",
                                           "Resampled Data (30 Hz)")) +
  theme(legend.position = "bottom")


