library(tidyverse)

# values = c("#990F0FFF","#CC5151FF",
#            "#99540FFF","#CC8E51FF",
#            "#6B990FFF", "#A3CC51FF",
#            "#0F6B99FF", "#51A3CCFF",
#            "#260F99FF", "#6551CCFF")

# load all results data
clemson = readr::read_csv(here::here("results/clemson_steps_1sec.csv")) %>%
  select(-"steps_truth_30")
clemson10 = readr::read_csv(here::here("results/clemson_steps_10sec.csv")) %>%
  select(-"steps_truth_30")
ox = readr::read_csv(here::here("results/ox_steps_1sec.csv")) %>%
  select(-"steps_truth_30")
ox10 = readr::read_csv(here::here("results/ox_steps_10sec.csv")) %>%
  select(-"steps_truth_30")

paletteer::paletteer_d("ggthemes::Hue_Circle")
col1 = "#1BA3C6FF"; col2 = "#F06719FF"; col3 = "#33A65CFF"
col4  = "#A26DC2FF"; col5 = "#FC719EFF"; col6 = "#F8B620FF"

# for primary analysis, just use clemson at 10 sec level, 30hz
# ox at 10 sec level, 100 hz original, resampled to 30 hz

clemson10 = clemson10 %>%
  select(time, id_study, id_subject, cat_activity, ends_with("30"),
         steps_truth) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular", "walk_semiregular", "walk_irregular")))

ox10 = ox10 %>%
  filter(sample_rate == 100) %>%
  select(time, id_study, id_subject,  ends_with("30"),
         steps_truth)

# tables for paper

ox10 %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson10) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("oxwalk", "walk_regular", "walk_semiregular", "walk_irregular"))) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, cat_activity, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*30\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_30.*", "\\1", name))  %>%
  select(-name) %>%
  group_by(metric, algorithm, cat_activity) %>%
  summarize(across(value,
            list(mean = ~ mean(.x, na.rm = TRUE),
                 sd = ~ sd(.x, na.rm = TRUE),
                 n = ~ sum(.x < 0.1),
                 pct = ~sum(.x < 0.1)/n()))) %>%
  rowwise() %>%
  mutate(mean = case_when(
    metric == "bias" ~ paste0(round(value_mean, 0), " (", round(value_sd, 0), ")"),
    metric == "ape" ~ paste0(round(value_mean*100, 1), " (", round(value_sd*100, 1), ")")),
    npct = paste0(value_n, " (", round(value_pct*100, 1), ")")
  ) %>%
  select(algorithm, cat_activity, mean, npct, metric) %>%
  pivot_wider(names_from = c(cat_activity,metric), values_from = mean:npct) %>%
  select(-c(npct_oxwalk_bias,  npct_walk_semiregular_bias, npct_walk_regular_bias,
            npct_walk_irregular_bias)) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll",booktabs = TRUE, format = "latex", col.names =
                      c("Algorithm", rep(c("Oxwalk", "Regular", "Semiregular", "Irregular"), 3))) %>%
  kableExtra::add_header_above(c(" " = 1, "APE" = 4, "Bias" = 4,
                                 "n (%) with APE <10%" = 4)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")


# just n (%) APE < 10
# from step 7
temp = ox %>%
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
  group_by(algorithm, id_study, cat_activity) %>%
  summarize(across(f1,
                   list(n = ~sum(.x > 0.75),
                        pct = ~sum(.x > 0.75)/n()))) %>%
  mutate(f1 = paste0(f1_n, " (", round(f1_pct*100, 1), ")")) %>%
  ungroup() %>%
  select(algorithm, cat_activity, f1) %>%
  pivot_wider(names_from = cat_activity, values_from = f1) %>%
  ungroup() %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) %>%
  bind_rows(df = tibble(algorithm = "Stepcount", clemson_ped = NA, oxwalk = NA)) %>%
  arrange(algorithm)

ox10 %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson10) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("oxwalk", "walk_regular", "walk_semiregular", "walk_irregular"))) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, cat_activity, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*30\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_30.*", "\\1", name))  %>%
  select(-name) %>%
  group_by(metric, algorithm, cat_activity) %>%
  summarize(across(value,
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE),
                        n = ~ sum(.x < 0.1),
                        pct = ~sum(.x < 0.1)/n()))) %>%
  rowwise() %>%
  mutate(mean = case_when(
    metric == "bias" ~ paste0(round(value_mean, 0), " (", round(value_sd, 0), ")"),
    metric == "ape" ~ paste0(round(value_mean*100, 1), " (", round(value_sd*100, 1), ")")),
    npct = paste0(value_n, " (", round(value_pct*100, 1), ")")
  ) %>%
  select(algorithm, cat_activity, mean, npct, metric) %>%
  pivot_wider(names_from = c(cat_activity,metric), values_from = mean:npct) %>%
  select(-c(npct_oxwalk_bias,  npct_walk_semiregular_bias, npct_walk_regular_bias,
            npct_walk_irregular_bias)) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  select(algorithm, starts_with("npct")) %>%
  left_join(temp)




# final table?
ox10 %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson10) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("oxwalk", "walk_regular", "walk_semiregular", "walk_irregular"))) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, cat_activity, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*30\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_30.*", "\\1", name))  %>%
  select(-name) %>%
  group_by(metric, algorithm, cat_activity) %>%
  summarize(across(value,
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  rowwise() %>%
  mutate(mean = case_when(
    metric == "bias" ~ paste0(round(value_mean, 0), " (", round(value_sd, 0), ")"),
    metric == "ape" ~ paste0(round(value_mean*100, 1), " (", round(value_sd*100, 1), ")"))
  ) %>%
  select(algorithm, cat_activity, mean, metric) %>%
  pivot_wider(names_from = c(cat_activity,metric), values_from = mean) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll",booktabs = TRUE, format = "latex", col.names =
                      c("Algorithm", rep(c("Oxwalk", "Regular", "Semiregular", "Irregular"), 2))) %>%
  kableExtra::add_header_above(c(" " = 1, "APE" = 4, "Bias" = 4)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")


# PLOTS truth vs predicted
labs = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")
names(labs) = c("acti", "adept", "oak", "sc", "sdt", "vs")
reg = clemson10 %>%
  filter(cat_activity == "walk_regular") %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1250))+
  scale_y_continuous(limits=c(0,1250))+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps", title = "Clemson: Regular Walking")+
  coord_equal()

semi = clemson10 %>%
  filter(cat_activity == "walk_semiregular") %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1100))+
  scale_y_continuous(limits=c(0,1100))+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps", title = "Clemson: Semiregular Walking")+
  coord_equal()

irr = clemson10 %>%
  filter(cat_activity == "walk_irregular") %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1100))+
  scale_y_continuous(limits=c(0,1100))+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps", title = "Clemson: Irregular Walking")+
  coord_equal()


ox = ox10 %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 6000))+
  scale_y_continuous(limits=c(0,6000))+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps", title = "OxWalk")+
  coord_equal()

gridExtra::grid.arrange(reg, semi, irr, ox, nrow = 4)



# correlation plots
cor.mat_clem =
  clemson10 %>%
  filter(cat_activity == "walk_regular") %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor(method = "spearman")
colnames(cor.mat_clem) = rownames(cor.mat_clem) = sub(".*steps\\_", "", colnames(cor.mat_clem))

reg.cor = ggcorrplot::ggcorrplot(cor.mat_clem, type = "lower", lab = TRUE,
                       title = "Clemson Correlations, Regular Walking")+
  scale_x_discrete(labels = c("Verisense", "ADEPT", "SDT", "ActiLife", "Stepcount", "Truth"))+
  scale_y_discrete(labels = c("Stepcount", "ActiLife", "SDT", "ADEPT", "Verisense", "Oak"))


cor.mat_clem =
  clemson10 %>%
  filter(cat_activity == "walk_semiregular") %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor(method = "spearman")
colnames(cor.mat_clem) = rownames(cor.mat_clem) = sub(".*steps\\_", "", colnames(cor.mat_clem))

semi.cor = ggcorrplot::ggcorrplot(cor.mat_clem, type = "lower", lab = TRUE,
                       title = "Clemson Correlations, Semiregular Walking")+
  scale_x_discrete(labels = c("Verisense", "ADEPT", "SDT", "ActiLife", "Stepcount", "Truth"))+
  scale_y_discrete(labels = c("Stepcount", "ActiLife", "SDT", "ADEPT", "Verisense", "Oak"))


cor.mat_clem =
  clemson10 %>%
  filter(cat_activity == "walk_irregular") %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor(method = "spearman")
colnames(cor.mat_clem) = rownames(cor.mat_clem) = sub(".*steps\\_", "", colnames(cor.mat_clem))

irr.corr = ggcorrplot::ggcorrplot(cor.mat_clem, type = "lower", lab = TRUE,
                       title = "Clemson Correlations, Irregular Walking")+
  scale_x_discrete(labels = c("Verisense", "ADEPT", "SDT", "ActiLife", "Stepcount", "Truth"))+
  scale_y_discrete(labels = c("Stepcount", "ActiLife", "SDT", "ADEPT", "Verisense", "Oak"))



cor.mat_ox =
  ox10 %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor(method = "spearman")
colnames(cor.mat_ox) = rownames(cor.mat_ox) = sub(".*steps\\_", "", colnames(cor.mat_ox))

ox.cor = ggcorrplot::ggcorrplot(cor.mat_ox, type = "lower", lab = TRUE,
                       title = "Ox Correlations")+
  scale_x_discrete(labels = c("Verisense", "ADEPT", "SDT", "ActiLife", "Stepcount", "Truth"))+
  scale_y_discrete(labels = c("Stepcount", "ActiLife", "SDT", "ADEPT", "Verisense", "Oak"))



gridExtra::grid.arrange(reg.cor, semi.cor, irr.corr, ox.cor, nrow = 2)

# bland altman, by activity type
summary_df =
  clemson10 %>%
  select(ends_with("30"), contains("truth"), id_subject, cat_activity) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(average = ~ mean(c(.x, steps_truth)),
                     difference = ~steps_truth - .x))) %>%
  select(id_subject, cat_activity, ends_with("average"), ends_with("difference")) %>%
  pivot_longer(cols = c(ends_with("average") | ends_with("difference"))) %>%
  mutate(measure = sub(".*\\_", "", name),
         algorithm = sub('\\_[^\\_]*$', '', name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value)

means = summary_df %>%
  group_by(algorithm, cat_activity) %>%
  summarize(mean = mean(difference),
            sd  = sd(difference),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

ggplot(summary_df) +
  geom_point(aes(x = average, y = difference))+
  facet_grid(cat_activity ~ algorithm) +
  geom_hline(data = means, aes(yintercept = mean), col = "#4F7CBAFF")+
  geom_hline(data = means, aes(yintercept = ub), col = "#2CB5C0FF")+
  geom_hline(data = means, aes(yintercept = lb), col = "#2CB5C0FF")+
  theme_bw()+
  geom_hline(aes(yintercept = 0), col = "red", linetype  = 2)+
  labs(x = "Average of Truth, Method",
       y = "Difference (Truth - Method)",
       title = "Bland Altman, Clemson Resampled")

# averaging over activity types
newlabs = labs
names(newlabs) = paste("steps_", names(labs), "_30", sep = "")
summary_df =
  clemson10 %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(average = ~ mean(c(.x, steps_truth)),
                     difference = ~steps_truth - .x))) %>%
  select(id_subject, ends_with("average"), ends_with("difference")) %>%
  pivot_longer(cols = c(ends_with("average") | ends_with("difference"))) %>%
  mutate(measure = sub(".*\\_", "", name),
         algorithm = sub('\\_[^\\_]*$', '', name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value)

summary_df =
  clemson10 %>%
  mutate(across(starts_with("steps"),
                ~log(ifelse(is.na(.x), 0, .x)+1))) %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(average = ~ mean(c(.x, steps_truth)),
                     difference = ~steps_truth - .x))) %>%
  select(id_subject, ends_with("average"), ends_with("difference")) %>%
  pivot_longer(cols = c(ends_with("average") | ends_with("difference"))) %>%
  mutate(measure = sub(".*\\_", "", name),
         algorithm = sub('\\_[^\\_]*$', '', name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value)
hist(summary_df$difference)

qqnorm(summary_df$difference, pch = 1, frame = FALSE)
qqline(summary_df$difference, col = "steelblue", lwd = 2)

means = summary_df  %>%
  group_by(algorithm) %>%
  summarize(mean = mean(difference),
            sd  = sd(difference),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

ba_clem = ggplot(summary_df) +
  geom_point(aes(x = average, y = difference))+
  facet_grid(. ~ algorithm, labeller = labeller(algorithm = newlabs)) +
  geom_hline(data = means, aes(yintercept = mean), col = "#4F7CBAFF")+
  geom_hline(data = means, aes(yintercept = ub), col = "#2CB5C0FF")+
  geom_hline(data = means, aes(yintercept = lb), col = "#2CB5C0FF")+
  theme_bw()+
  geom_hline(aes(yintercept = 0), col = "red", linetype  = 2)+
  labs(x = "Average of Truth, Method",
       y = "Difference (Truth - Method)",
       title = "Bland Altman, Clemson")


summary_df =
  ox10 %>%
  ungroup() %>%
  mutate(across(starts_with("steps"),
                ~log(ifelse(is.na(.x), 0, .x)+1))) %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  group_by(id_subject) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(average = ~ mean(c(.x, steps_truth)),
                     difference = ~steps_truth - .x))) %>%
  select(id_subject, ends_with("average"), ends_with("difference")) %>%
  pivot_longer(cols = c(ends_with("average") | ends_with("difference"))) %>%
  mutate(measure = sub(".*\\_", "", name),
         algorithm = sub('\\_[^\\_]*$', '', name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value)

hist(summary_df$difference)
qqnorm(summary_df$difference, pch = 1, frame = FALSE)
qqline(summary_df$difference, col = "steelblue", lwd = 2)


means = summary_df  %>%
  group_by(algorithm) %>%
  summarize(mean = mean(difference),
            sd  = sd(difference),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

ba_ox = ggplot(summary_df) +
  geom_point(aes(x = average, y = difference))+
  facet_grid(.~ algorithm, labeller = labeller(algorithm = newlabs)) +
  geom_hline(data = means, aes(yintercept = mean), col = "#4F7CBAFF")+
  geom_hline(data = means, aes(yintercept = ub), col = "#2CB5C0FF")+
  geom_hline(data = means, aes(yintercept = lb), col = "#2CB5C0FF")+
  theme_bw()+
  geom_hline(aes(yintercept = 0), col = "red", linetype  = 2)+
  labs(x = "Average of Truth, Method",
       y = "Difference (Truth - Method)",
       title = "Bland Altman, OxWalk")

gridExtra::grid.arrange(ba_clem, ba_ox, nrow = 2)

# unclear whether to take log or not
# we can also report y axis as (a-b)/mean

summary_df =
  ox10 %>%
  ungroup() %>%
  mutate(across(starts_with("steps"),
                ~log(ifelse(is.na(.x), 0, .x)+1))) %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  group_by(id_subject) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(average = ~ mean(c(.x, steps_truth)),
                     difference = ~steps_truth - .x,
                     avgdiff = ~(steps_truth - .x)/(mean(c(.x, steps_truth)))))) %>%
  select(id_subject, ends_with("average"), ends_with("difference"), ends_with("avgdiff")) %>%
  pivot_longer(cols = c(ends_with("average") | ends_with("difference") | ends_with("avgdiff"))) %>%
  mutate(measure = sub(".*\\_", "", name),
         algorithm = sub('\\_[^\\_]*$', '', name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value)


means = summary_df  %>%
  group_by(algorithm) %>%
  summarize(mean = mean(difference),
            sd  = sd(difference),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

means = summary_df  %>%
  mutate(avgdiff = avgdiff * 100) %>%
  group_by(algorithm) %>%
  summarize(mean = mean(avgdiff),
            sd  = sd(avgdiff),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

ggplot(summary_df  %>%
                 mutate(avgdiff = avgdiff * 100)) +
  geom_point(aes(x = average, y = avgdiff))+
  facet_grid(.~ algorithm, labeller = labeller(algorithm = newlabs)) +
  geom_hline(data = means, aes(yintercept = mean), col = "#4F7CBAFF")+
  geom_hline(data = means, aes(yintercept = ub), col = "#2CB5C0FF")+
  geom_hline(data = means, aes(yintercept = lb), col = "#2CB5C0FF")+
  theme_bw()+
  geom_hline(aes(yintercept = 0), col = "red", linetype  = 2)+
  labs(x = "Average of Truth, Method",
       y = "Difference (Truth - Method)",
       title = "Bland Altman, OxWalk")




# old
# bar charts of totals vs truth
clemson10 %>%
  mutate(cat_activity = factor(cat_activity,
                               levels = c("walk_regular", "walk_semiregular", "walk_irregular"))) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(starts_with("steps") & !contains("truth")) %>%
  mutate(id_subject = as.numeric(sub("^0+(?!$)", "", id_subject, perl=TRUE))) %>%
  ggplot(aes(x = id_subject, y = value, fill = name))+
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_segment(aes(x = id_subject-.45, xend = id_subject  + .45,
                   y = steps_truth, yend = steps_truth))+
  facet_wrap(.~cat_activity, nrow = 3, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(
    values = c(col1, col2, col3, col4, col5, col6),
    name = "",
    labels = c("Acti",
               "Adept",
               "Oak",
               "Stepcount",
               "SDT", "Vs")
  )+
  labs(x = "ID", y = "Steps")+
  scale_x_continuous(limits=c(.5,30.5),breaks=seq(1,30,1))



ox10 %>%
  mutate(id_subject = as.numeric(sub(".*P", "", id_subject))) %>%
  group_by(id_subject, grp = as.factor(floor(id_subject/10))) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(starts_with("steps") & !contains("truth")) %>%
  ggplot(aes(x = id_subject, y = value, fill = name))+
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_segment(aes(x = id_subject-.45, xend = id_subject  + .45,
                   y = steps_truth, yend = steps_truth))+
  facet_wrap(.~grp, nrow = 2, scales = "free_x")+
  theme_bw()+
  scale_fill_manual(
    values = c(col1, col2, col3, col4, col5, col6),
    name = "",
    labels = c("Acti",
               "Adept",
               "Oak",
               "Stepcount",
               "SDT", "Vs")
  )+
  labs(x = "ID", y = "Steps")+
  scale_x_continuous(breaks=seq(1,39,1))



# correlations
cor.mat_clem =
  clemson10 %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor(method = "spearman")
colnames(cor.mat_clem) = rownames(cor.mat_clem) = sub(".*steps\\_", "", colnames(cor.mat_clem))

ggcorrplot::ggcorrplot(cor.mat_clem, type = "lower", lab = TRUE,
                       title = "Clemson Correlations")

cor.mat_clem =
  clemson10 %>%
  filter(cat_activity == "walk_regular") %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor(method = "spearman")
colnames(cor.mat_clem) = rownames(cor.mat_clem) = sub(".*steps\\_", "", colnames(cor.mat_clem))

ggcorrplot::ggcorrplot(cor.mat_clem, type = "lower", lab = TRUE,
                       title = "Clemson Correlations, Regular Walking")

cor.mat_clem =
  clemson10 %>%
  filter(cat_activity == "walk_semiregular") %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor(method = "spearman")
colnames(cor.mat_clem) = rownames(cor.mat_clem) = sub(".*steps\\_", "", colnames(cor.mat_clem))

ggcorrplot::ggcorrplot(cor.mat_clem, type = "lower", lab = TRUE,
                       title = "Clemson Correlations, Semiregular Walking")

cor.mat_clem =
  clemson10 %>%
  filter(cat_activity == "walk_irregular") %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor(method = "spearman")
colnames(cor.mat_clem) = rownames(cor.mat_clem) = sub(".*steps\\_", "", colnames(cor.mat_clem))

ggcorrplot::ggcorrplot(cor.mat_clem, type = "lower", lab = TRUE,
                       title = "Clemson Correlations, Irregular Walking")


cor.mat_ox =
  ox10 %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(starts_with("steps")) %>%
  cor()
colnames(cor.mat_ox) = rownames(cor.mat_ox) = sub(".*steps\\_", "", colnames(cor.mat_ox))

ggcorrplot::ggcorrplot(cor.mat_ox, type = "lower", lab = TRUE,
                       title = "Ox Correlations")

# truth vs predicted method

clemson10 %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_grid(method~cat_activity, scales = "free")+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()

clemson10 %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_grid(.~cat_activity, scales = "free")+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()

clemson10 %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_wrap(.~method)+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()

ox10 %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_wrap(.~method)+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  geom_abline()


# MAPE table
# bias table

# rows: method
# column - data

# MSE, MAPE
# mape = (1/n)*sum(abs(actual - predicted)/actual)
# mse = (1/n)*sum(actual-predicted)^2

clemson10 %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(starts_with("steps")&!contains("truth"),
                list(pe = ~abs((steps_truth - .x)/steps_truth),
                     se = ~(steps_truth - .x)^2,
                     bias = ~steps_truth - .x))) %>%
  group_by(cat_activity) %>%
  summarize(across(ends_with("pe")|ends_with("se") |ends_with("bias"),
                   ~mean(.x))) %>%
  pivot_longer(cols = -cat_activity)  %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name),
         measure = sub(".*30\\_", "", name)) %>%
  select(-name) %>%
  pivot_wider(names_from= c(cat_activity, measure), values_from = value)

clemson10 %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(starts_with("steps")&!contains("truth"),
                list(pe = ~abs((steps_truth - .x)/steps_truth),
                     se = ~(steps_truth - .x)^2,
                     bias = ~steps_truth - .x))) %>%
  group_by(cat_activity) %>%
  summarize(across(ends_with("pe")|ends_with("se") |ends_with("bias"),
                   list(mean = ~mean(.x),
                        sd = ~sd(.x)))) %>%
  pivot_longer(cols = -cat_activity)  %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name),
         measure = sub(".*30\\_", "", name),
         measure2 = sub(".*\\_", "", measure),
         measure3 = sub("\\_.*", "", measure)) %>%
  select(-name, -measure) %>%
  pivot_wider(names_from= c(measure2), values_from = value)  %>%
  mutate(value = paste0(round(mean,  2), " (", round(sd, 2), ")")) %>%
  select(-mean, -sd) %>%
  pivot_wider(names_from = c(cat_activity, measure3),
              values_from = value)
ox10 %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(starts_with("steps")&!contains("truth"),
                list(pe = ~abs((steps_truth - .x)/steps_truth),
                     se = ~(steps_truth - .x)^2,
                     bias = ~steps_truth - .x))) %>%
  summarize(across(ends_with("pe")|ends_with("se") |ends_with("bias"),
                   ~mean(.x))) %>%
  mutate(id = "id") %>%
  pivot_longer(cols = -id)  %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name),
         measure = sub(".*30\\_", "", name)) %>%
  select(-name) %>%
  pivot_wider(names_from=  measure, values_from = value) %>%
  select(-id)

ox10 %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(starts_with("steps")&!contains("truth"),
                list(pe = ~abs((steps_truth - .x)/steps_truth),
                     se = ~(steps_truth - .x)^2,
                     bias = ~steps_truth - .x))) %>%
  summarize(across(ends_with("pe")|ends_with("se") |ends_with("bias"),
                   list(mean = ~mean(.x),
                        sd = ~sd(.x)))) %>%
  mutate(id = "id") %>%
  pivot_longer(cols = -id)  %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name),
         measure = sub(".*30\\_", "", name),
         measure2 = sub(".*\\_", "", measure),
         measure3 = sub("\\_.*", "", measure)) %>%
  select(-name, -measure) %>%
  pivot_wider(names_from= c(measure2), values_from = value)  %>%
  mutate(value = paste0(round(mean,  2), " (", round(sd, 2), ")")) %>%
  select(-mean, -sd) %>%
  pivot_wider(names_from = measure3,
              values_from = value) %>%
  select(-id)


ox10 %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson10) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(starts_with("steps")&!contains("truth"),
                list(pe = ~abs((steps_truth - .x)/steps_truth),
                     se = ~(steps_truth - .x)^2,
                     bias = ~steps_truth - .x))) %>%
  group_by(cat_activity) %>%
  summarize(across(ends_with("pe")|ends_with("se") |ends_with("bias"),
                   list(mean = ~mean(.x),
                        sd = ~sd(.x)))) %>%
  pivot_longer(cols = -cat_activity)  %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name),
         measure = sub(".*30\\_", "", name),
         measure2 = sub(".*\\_", "", measure),
         measure3 = sub("\\_.*", "", measure)) %>%
  select(-name, -measure) %>%
  pivot_wider(names_from= c(measure2), values_from = value)  %>%
  mutate(value = paste0(round(mean,  2), " (", round(sd, 2), ")")) %>%
  select(-mean, -sd) %>%
  pivot_wider(names_from = c(cat_activity, measure3),
              values_from = value)



# calc # people with < 10% error

ox10 %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson10) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(starts_with("steps")&!contains("truth"),
                ~abs((steps_truth - .x)/steps_truth))) %>%
  mutate(across(starts_with("steps")&!contains("truth"),
                ~ifelse(.x <= .1, 1, 0))) %>%
  group_by(cat_activity) %>%
  summarize(across(contains("steps") & !contains("truth"),
                   list(n = ~n(),
                        criteria = ~sum(.x),
                        prop = ~sum(.x)/n()))) %>%
  pivot_longer(cols = -cat_activity) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name),
         measure = sub(".*30\\_", "", name)) %>%
  filter(measure != "n") %>%
  select(-name) %>%
  pivot_wider(names_from= c(measure), values_from = value)  %>%
  mutate(value = paste0(criteria, " (", round(prop*100, 1), "%)")) %>%
  select(-criteria, -prop) %>%
  pivot_wider(names_from = c(cat_activity),
              values_from = value)




# bland altman start here
# bland altman
# need average and difference


summary_df =
  clemson10 %>%
  select(ends_with("30"), contains("truth"), id_subject, cat_activity) %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(average = ~ mean(c(.x, steps_truth)),
                     difference = ~steps_truth - .x))) %>%
  select(id_subject, cat_activity, ends_with("average"), ends_with("difference")) %>%
  pivot_longer(cols = c(ends_with("average") | ends_with("difference"))) %>%
  mutate(measure = sub(".*\\_", "", name),
         algorithm = sub('\\_[^\\_]*$', '', name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value)

means = summary_df %>%
  group_by(algorithm, cat_activity) %>%
  summarize(mean = mean(difference),
            sd  = sd(difference),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

ggplot(summary_df) +
  geom_point(aes(x = average, y = difference))+
  facet_grid(cat_activity ~ algorithm) +
  geom_hline(data = means, aes(yintercept = mean), col = "#4F7CBAFF")+
  geom_hline(data = means, aes(yintercept = ub), col = "#2CB5C0FF")+
  geom_hline(data = means, aes(yintercept = lb), col = "#2CB5C0FF")+
  theme_bw()+
  geom_hline(aes(yintercept = 0), col = "red", linetype  = 2)+
  labs(x = "Average of Truth, Method",
       y = "Difference (Truth - Method)",
       title = "Bland Altman, Clemson Resampled")

# averaging over activity types
summary_df =
  clemson10 %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(average = ~ mean(c(.x, steps_truth)),
                     difference = ~steps_truth - .x))) %>%
  select(id_subject, ends_with("average"), ends_with("difference")) %>%
  pivot_longer(cols = c(ends_with("average") | ends_with("difference"))) %>%
  mutate(measure = sub(".*\\_", "", name),
         algorithm = sub('\\_[^\\_]*$', '', name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value)

means = summary_df  %>%
  group_by(algorithm) %>%
  summarize(mean = mean(difference),
            sd  = sd(difference),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

ggplot(summary_df) +
  geom_point(aes(x = average, y = difference))+
  facet_grid(. ~ algorithm) +
  geom_hline(data = means, aes(yintercept = mean), col = "#4F7CBAFF")+
  geom_hline(data = means, aes(yintercept = ub), col = "#2CB5C0FF")+
  geom_hline(data = means, aes(yintercept = lb), col = "#2CB5C0FF")+
  theme_bw()+
  geom_hline(aes(yintercept = 0), col = "red", linetype  = 2)+
  labs(x = "Average of Truth, Method",
       y = "Difference (Truth - Method)",
       title = "Bland Altman, Clemson")


summary_df =
  ox10 %>%
  ungroup() %>%
  mutate(across(starts_with("steps"),
                ~ifelse(is.na(.x), 0, .x))) %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"),
                   ~ sum(.x))) %>%
  group_by(id_subject) %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(average = ~ mean(c(.x, steps_truth)),
                     difference = ~steps_truth - .x))) %>%
  select(id_subject, ends_with("average"), ends_with("difference")) %>%
  pivot_longer(cols = c(ends_with("average") | ends_with("difference"))) %>%
  mutate(measure = sub(".*\\_", "", name),
         algorithm = sub('\\_[^\\_]*$', '', name)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure, values_from = value)

means = summary_df  %>%
  group_by(algorithm) %>%
  summarize(mean = mean(difference),
            sd  = sd(difference),
            ub = mean + 1.96 * sd,
            lb = mean - 1.96 * sd)

ggplot(summary_df) +
  geom_point(aes(x = average, y = difference))+
  facet_grid(.~ algorithm) +
  geom_hline(data = means, aes(yintercept = mean), col = "#4F7CBAFF")+
  geom_hline(data = means, aes(yintercept = ub), col = "#2CB5C0FF")+
  geom_hline(data = means, aes(yintercept = lb), col = "#2CB5C0FF")+
  theme_bw()+
  geom_hline(aes(yintercept = 0), col = "red", linetype  = 2)+
  labs(x = "Average of Truth, Method",
       y = "Difference (Truth - Method)",
       title = "Bland Altman, OxWalk")




