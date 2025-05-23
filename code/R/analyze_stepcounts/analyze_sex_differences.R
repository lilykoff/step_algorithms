# demo
library(tidyverse)
`%notin%` = Negate(`%in%`)
ox_meta = readr::read_csv(here::here("data", "raw", "oxwalk", "OxWalk_Dec2022", "metadata.csv"))
#
# oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz")) %>%
#   filter(sample_rate == 100)

step_df =
  readRDS(here::here("results", "all_algorithms", "step_stats_bysubject.rds")) %>%
  filter(cat_activity != "oxwalk25" &
           grepl("30", algorithm)) %>%
  filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30"))

step_df %>%
  filter(id_study == "oxwalk")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  ggplot(aes(x = algorithm, y = bias, col = sex))+
  geom_boxplot(position = position_dodge(width = .9), outlier.shape = NA)+
  geom_point(position = position_dodge(width = .9))+
  theme_bw()+
  geom_hline(aes(yintercept = 0))+
  labs(x = "Algorithm", y = "Bias (predicted - truth)", title = "OxWalk")+
  scale_color_manual(values = c("#006DDBFF", "#920000FF"), name = "")+
  scale_x_discrete(labels=c("Actilife", "ADEPT", "Oak", "SCRF", "SCSSL", "SDT", "Vs, Orig", "Vs, Rev"))


labs = c("ADEPT", "Oak", "SDT", "Verisense (orig)", "Verisense (rev)", "Stepcount SSL", "Stepcount RF", "ActiLife")
names(labs) = c("steps_adept_30",  "steps_oak_30",
                "steps_sdt_30",    "steps_vsores_30",
                "steps_vsrres_30", "steps_scssl_30",
                "steps_scrf_30",   "steps_acti_30")
step_df %>%
  filter(id_study == "oxwalk")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  ggplot(aes(x = sex, y = bias, col = sex))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .1)+
  theme_bw()+
  facet_wrap(.~algorithm, scales = "free_y", labeller = labeller(algorithm  = labs))+
  geom_hline(aes(yintercept = 0))+
  labs(x = "", y = "Bias (predicted - true) steps", title = "OxWalk")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#006DDBFF", "#920000FF"))

step_df %>%
  filter(id_study == "oxwalk" & algorithm != "steps_sdt_30")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  ggplot(aes(x = algorithm, y = ape, col = sex))+
  geom_boxplot(position = position_dodge(width = .9), outlier.shape = NA)+
  geom_point(position = position_dodge(width = .9))+
  theme_bw()+
  geom_hline(aes(yintercept = 0))+
  labs(x = "Algorithm", y = "Absolute % error", title = "OxWalk")+
  scale_color_manual(values = c("#006DDBFF", "#920000FF"), name = "")+
  scale_x_discrete(labels=c("Actilife", "ADEPT", "Oak", "SCRF", "SCSSL", "SDT", "Vs, Orig", "Vs, Rev"))

step_df %>%
  filter(id_study == "oxwalk" & algorithm =="steps_scssl_30")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  ggplot(aes(x = sex, y = bias, col = sex))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .05, size = 1.5)+
  theme_bw()+
  geom_hline(aes(yintercept = 0))+
  labs(x = "Algorithm", y = "Bias (predicted steps minus true steps)", title = "OxWalk")+
  scale_color_manual(values = c("#006DDBFF", "#920000FF"), name = "")+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Female", "Male"))+
  theme(axis.text.x=element_text(size=14),
        axis.title = element_text(size = 15))
step_df %>%
  filter(id_study == "oxwalk")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  ggplot(aes(x = age, y = bias, col = age))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .1)+
  theme_bw()+
  facet_wrap(.~algorithm, scales = "free_y", labeller = labeller(algorithm  = labs))+
  geom_hline(aes(yintercept = 0))+
  labs(x = "", y = "Bias (predicted - true) steps", title = "OxWalk")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#004949", "#DB6D00", "#490092"))

step_df %>%
  filter(id_study == "oxwalk")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  mutate(age_sex = paste0(age, sex)) %>%
  ggplot(aes(x = age_sex, y = bias, col = sex))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=.1)+
  theme_bw()+
  facet_wrap(.~algorithm, scales = "free_y", labeller = labeller(algorithm  = labs))+
  geom_hline(aes(yintercept = 0))+
  labs(x = "", y = "Bias (predicted - true) steps", title = "OxWalk")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#006DDBFF", "#920000FF"), name = "")



step_df %>%
  filter(id_study == "oxwalk" & algorithm!= "steps_sdt_30" & algorithm != "steps_adept_30")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  ggplot(aes(x = age, y = bias, col = age))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .1)+
  theme_bw()+
  facet_wrap(.~algorithm, labeller = labeller(algorithm  = labs))+
  geom_hline(aes(yintercept = 0))+
  labs(x = "", y = "Bias (predicted - true) steps", title = "OxWalk")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#004949", "#DB6D00", "#490092"))

step_df %>%
  filter(id_study == "oxwalk" & algorithm!= "steps_sdt_30" & algorithm != "steps_adept_30")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  mutate(age_sex = paste0(age, sex)) %>%
  ggplot(aes(x = age_sex, y = bias, col = sex))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=.1)+
  theme_bw()+
  facet_wrap(.~algorithm, labeller = labeller(algorithm  = labs))+
  geom_hline(aes(yintercept = 0))+
  labs(x = "", y = "Bias (predicted - true) steps", title = "OxWalk")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#006DDBFF", "#920000FF"), name = "")

step_df %>%
  filter(id_study == "oxwalk" & algorithm!= "steps_sdt_30" & algorithm != "steps_adept_30")  %>%
  left_join(ox_meta, by = c("id_subject" = "participant")) %>%
  mutate(age_sex = paste0(age, sex)) %>%
  ggplot(aes(x = age_sex, y = bias, col = sex))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=.1)+
  theme_bw()+
  facet_wrap(.~algorithm, labeller = labeller(algorithm  = labs))+
  geom_hline(aes(yintercept = 0))+
  labs(x = "", y = "Bias (predicted - true) steps", title = "OxWalk")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#006DDBFF", "#920000FF"), name = "")
