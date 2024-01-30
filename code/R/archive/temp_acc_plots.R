# temp acc plots

pdf(file = "accuracy_figures.pdf")

recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(position = position_dodge())+
  facet_grid(name ~ cat_activity, scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots, no labels or points")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))


recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.5, size = .7)+
  facet_grid(name ~ ., scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots, no labels")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))

recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(position = position_dodge())+
  facet_grid(name ~ cat_activity, scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots, means labeled")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) +
  geom_text(data = recog_mean %>%
              mutate(name = sub("\\_.*", "", name)),
            aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)

recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.2)+
  facet_grid(name ~ cat_activity, scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots, with points and means labeled")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) +
  geom_text(data = recog_mean %>%
              mutate(name = sub("\\_.*", "", name)),
            aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)

recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(position = position_dodge())+
  facet_grid(name ~ cat_activity, scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots, with medians labeled")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) +
  geom_text(data = recog_med %>%
              mutate(name = sub("\\_.*", "", name)),
            aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)

recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.5, size = .7)+
  facet_grid(name ~ cat_activity, scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots, with points and medians labeled")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) +
  geom_text(data = recog_med %>%
              mutate(name = sub("\\_.*", "", name)),
            aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)

plt3 = recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(position = position_dodge())+
  facet_grid(name ~ cat_activity, scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots with significance tests and means")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) +
  geom_text(data = recog_mean %>%
              mutate(name = sub("\\_.*", "", name)),
            aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)+
  scale_y_continuous(labels = c(0, 0.5, 1, "", ""))

my_comparisons = list(c("acti", "adept"), c("acti", "oak"),c("acti", "sdt"), c("acti", "vs"),
                      c("adept", "oak"),c("adept", "sdt"), c("adept", "vs"),
                      c("oak", "sdt"), c("oak", "vs"),
                      c("vs", "sdt"))

plt3 +
  ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE,
                             label= "p.signif")

plt3 +
  ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE,
                             label= "p.format")

plt3 = recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.5, size = .7)+
  facet_grid(name ~ cat_activity, scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots with significance tests and means")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense")) +
  geom_text(data = recog_mean %>%
              mutate(name = sub("\\_.*", "", name)),
            aes(x = algorithm, y = value, label = round(value, 2)), col = "black",size = 3)+
  scale_y_continuous(labels = c(0, 0.5, 1, "", ""))

my_comparisons = list(c("acti", "adept"), c("acti", "oak"),c("acti", "sdt"), c("acti", "vs"),
                      c("adept", "oak"),c("adept", "sdt"), c("adept", "vs"),
                      c("oak", "sdt"), c("oak", "vs"),
                      c("vs", "sdt"))

plt3 +
  ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE,
                             label= "p.signif")

plt3 +
  ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE,
                             label= "p.format")


dev.off()

recog_stats %>%
  mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
  pivot_longer(cols = recall:f1) %>%
  ggplot(aes(x = algorithm, y = value, col = algorithm))+
  geom_boxplot(outlier.shape = NA, position = position_dodge())+
  geom_jitter(width=.1, alpha=.5, size = .7)+
  facet_grid(name ~ ., scales = "free_y",
             labeller = labeller(name = labs,
                                 cat_activity = labs2))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#66A61EFF", "#E6AB02FF"),
                     labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "", y = "Value", title = "Boxplots, no labels")+
  scale_x_discrete(labels = c("ActiLife", "ADEPT", "Oak", "SDT", "Verisense"))+
  ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE,
                             label= "p.format")
