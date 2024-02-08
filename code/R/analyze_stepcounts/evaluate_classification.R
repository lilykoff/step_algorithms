library(tidyverse)
`%notin%` = Negate(`%in%`)


accuracy_df = readRDS(here::here("results", "all_algorithms", "accuracy_stats_bysubject.rds"))

# # supplemental classification table, wide
# accuracy_df %>%
#   filter(cat_activity != "oxwalk25" & cat_activity != "clemson_overall" &
#            grepl("30", algorithm)) %>%
#   group_by(algorithm, cat_activity) %>%
#   summarize(across(c(recall, prec, f1),
#                    list(mean = ~mean(.x),
#                         sd = ~ sd(.x)))) %>%
#   mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
#          precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
#          f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
#   select(algorithm, cat_activity, recall, precision, f1) %>%
#   pivot_wider(names_from = cat_activity, values_from = recall:f1) %>%
#   ungroup() %>%
#   mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")) %>%
#   kableExtra::kable(align = "llll", booktabs = TRUE,  col.names =
#                       c("Algorithm", rep(c("Regular", "Semiregular", "Irregular", "Regular", "Free-Living"), 3))) %>%
#   kableExtra::add_header_above(c(" " = 1, "Clemson" = 3,
#                                  "MAREA" = 1, "OxWalk" = 1, "Clemson" = 3,
#                                  "MAREA" = 1, "OxWalk" = 1, "Clemson" = 3,
#                                  "MAREA" = 1, "OxWalk" = 1)) %>%
#   kableExtra::add_header_above(c(" " = 1, "Recall" = 5,
#                                  "Precision" = 5, "F1 Score" = 5)) %>%
#   kableExtra::kable_styling(latex_options = "scale_down")


# supplemental classification table, long

  accuracy_df %>%
    filter(cat_activity != "oxwalk25" & cat_activity != "clemson_overall" &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>% # remove the raw 30 hz verisense, just use reampled
    group_by(algorithm, cat_activity) %>%
    summarize(across(c(recall, prec, f1),
                     list(mean = ~mean(.x),
                          sd = ~ sd(.x)))) %>%
    mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
           precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
           f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
    select(algorithm, cat_activity, recall, precision, f1) %>%
    pivot_longer(recall:f1) %>%
    pivot_wider(names_from = cat_activity, values_from = value) %>%
    ungroup() %>%
    select(name, algorithm, marea,  clemson_walk_regular, clemson_walk_semiregular, clemson_walk_irregular, oxwalk100) %>%
    arrange(name) %>%
    kableExtra::kable(align = "llllll", booktabs = TRUE,  format = "latex", col.names =
                        c("Metric", "Algorithm", rep(c("Regular", "Regular", "Semiregular", "Irregular", "Free-Living"), 1)))  %>%
    kableExtra::collapse_rows(columns = 1:2, valign = "top") %>%
    kableExtra::add_header_above(c(" " = 2, "MAREA" = 1,
                                   "Clemson" = 3, "OxWalk" = 1))  %>%
    kableExtra::kable_styling(latex_options = "scale_down")

  # main manuscript classification table



  tab_individual =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
    group_by(algorithm, cat_activity) %>%
    summarize(across(c(recall, prec, f1),
                     list(mean = ~mean(.x),
                          sd = ~ sd(.x)))) %>%
    # formatting
    mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
           precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
           f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
    select(algorithm, cat_activity, recall, precision, f1) %>%
    pivot_wider(names_from = cat_activity, values_from = recall:f1) %>%
    select(-precision_marea)

  tab_overall =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
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
    mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)", "Verisense (revised)")) %>%
    kableExtra::kable(align = "llll", booktabs = TRUE,  format = "latex", col.names =
                        c("Algorithm", "Clemson", "MAREA", "Oxwalk", "Overall",
                          "Clemson", "Oxwalk", "Overall",
                          "Clemson", "MAREA", "Oxwalk", "Overall")) %>%
    kableExtra::add_header_above(c(" " = 1, "Recall" = 4,
                                   "Precision" = 3, "F1 Score" = 4)) %>%
    kableExtra::kable_styling(latex_options = "scale_down")

  # determine best
  accuracy_df %>% group_by(algorithm, cat_activity) %>% summarize(f1 = mean(f1)) %>%
    select(algorithm, f1, cat_activity) %>%
    filter(cat_activity == "clemson_overall") %>%  arrange(desc(f1))

  accuracy_df %>% group_by(algorithm, cat_activity) %>% summarize(f1 = mean(f1)) %>%
    select(algorithm, f1, cat_activity) %>%
    filter(cat_activity == "marea") %>%  arrange(desc(f1))

  # main manuscript boxplots of metrics
  recog_median =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
    group_by(algorithm, cat_activity) %>%
    summarize(across(c(recall, prec, f1),
                     list(median = ~median(.x)))) %>%
    pivot_longer(cols = ends_with("median")) %>%
    mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
    ungroup()

  level_order =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
             grepl("30", algorithm)) %>%
    group_by(algorithm) %>%
    summarize(median = median(f1)) %>%
    ungroup() %>%
    arrange(median) %>%
    pull(algorithm)


  # each row is subject, activity
  recog_stats =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
    pivot_longer(cols = recall:f1)


  # overall panel
  overall =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
    pivot_longer(cols = recall:f1) %>%
    mutate(cat_activity = "overall")
  labs = c("F1 Score", "Precision", "Recall")
  names(labs) = c("f1", "prec", "recall")

  plot = overall %>%
    mutate(value= ifelse(name == "prec" & id_study == "marea", NA, value),
           cat_activity = "Overall") %>%
    ggplot(aes(x = factor(algorithm, levels = level_order), y = value, col = algorithm))+
    geom_boxplot(outlier.shape = NA, position = position_dodge())+
    geom_jitter(width=.1, alpha=.5)+
    facet_grid(name ~ cat_activity,
               labeller = labeller(name = labs)) +
    scale_color_brewer(palette  = "Dark2",name = "",
                       labels = c("ActiLife", "ADEPT", "Oak","Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)", "Verisense (revised"))+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1, vjust =1),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10))+
    labs(x = "", y = "")+
    guides(colour = guide_legend(nrow = 4))+
    scale_x_discrete(labels = c("ADEPT", "SDT", "Oak", "ActiLife", "Verisense (Original)", "Verisense (Revised)", "Stepcount (RF)", "Stepcount (SSL)"))
  svg(here::here("manuscript_figures", "boxplot_overall.svg"))
  plot
  dev.off()

  labs2 = c("Clemson", "MAREA", "OxWalk")
  names(labs2) = c("clemson_overall", "marea", "oxwalk100")

  # current figure
  plot =
    recog_stats %>%
    mutate(value= ifelse(name == "prec" & id_study == "marea", NA, value)) %>%
    mutate(cat_activity = factor(cat_activity, levels = c("clemson_overall", "marea", "oxwalk100"))) %>%
    ggplot(aes(x = factor(algorithm, levels = level_order), y = value, col = algorithm))+
    geom_boxplot(outlier.shape = NA, position = position_dodge())+
    geom_jitter(width=.1, alpha=.5)+
    facet_grid(name ~ cat_activity,
               labeller = labeller(name = labs,
                                   cat_activity = labs2)) +
    scale_color_brewer(palette  = "Dark2",name = "",
                       labels = c("ActiLife", "ADEPT", "Oak","Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)", "Verisense (revised)"))+
    theme_bw()+
    theme(legend.position = "none",
          # legend.position = c(.49, .6),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(1, 1, 1, 1),
          axis.text.x = element_text(angle = 30, hjust = 1, vjust =1),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 12))+
    labs(x = "", y = "")+
    guides(colour = guide_legend(nrow = 4))+
    scale_x_discrete(labels = c("ADEPT", "SDT", "Oak", "ActiLife", "Verisense (Original)", "Verisense (Revised)", "Stepcount (RF)", "Stepcount (SSL)"))

  svg(here::here("manuscript_figures", "boxplot_all.svg"))
  plot
  dev.off()




  # significance testing
  # generate tile plots with p values for ease of writing results
  recog_stats_tmp =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>% # remove the raw 30 hz
    select(recall, prec, f1, id_subject, id_study = cat_activity, algorithm)
  # create overall df
  recog_stats_test =
    recog_stats_tmp %>% mutate(id_study = "overall") %>%
    bind_rows(accuracy_df %>%
                filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
                         grepl("30", algorithm)) %>%
                filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>% # remove the raw 30 hz
                select(recall, prec, f1, id_subject, id_study=cat_activity, algorithm))

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
  pairs = expand.grid.unique(x = c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vsores", "vsrres"),
                             y =  c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vsores", "vsrres")) %>%
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
    scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt", "vso")) +
    scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vso", "vsr"))+
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
    scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt", "vso")) +
    scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vso", "vsr"))+
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
    scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt", "vso")) +
    scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vso", "vsr"))+
    labs(title = "p values for prec score")

  cowplot::plot_grid(f1, prec, recall)
} else{
  accuracy_df %>%
    filter(cat_activity != "oxwalk25" & cat_activity != "clemson_overall" &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>% # remove the raw 30 hz verisense, just use reampled
    group_by(algorithm, cat_activity) %>%
    summarize(across(c(recall, prec, f1),
                     list(mean = ~mean(.x),
                          sd = ~ sd(.x)))) %>%
    mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
           precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
           f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
    select(algorithm, cat_activity, recall, precision, f1) %>%
    pivot_longer(recall:f1) %>%
    pivot_wider(names_from = cat_activity, values_from = value) %>%
    ungroup() %>%
    select(name, algorithm, clemson_walk_regular, clemson_walk_semiregular, clemson_walk_irregular, oxwalk100) %>%
    arrange(name) %>%
    kableExtra::kable(align = "llllll", booktabs = TRUE,  format = "latex", col.names =
                        c("Metric", "Algorithm", rep(c( "Regular", "Semiregular", "Irregular", "Free-Living"), 1)))  %>%
    kableExtra::collapse_rows(columns = 1:2, valign = "top") %>%
    kableExtra::add_header_above(c(" " = 2,
                                   "Clemson" = 3, "OxWalk" = 1))  %>%
    kableExtra::kable_styling(latex_options = "scale_down")

  # main manuscript classification table



  tab_individual =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
    group_by(algorithm, cat_activity) %>%
    summarize(across(c(recall, prec, f1),
                     list(mean = ~mean(.x),
                          sd = ~ sd(.x)))) %>%
    # formatting
    mutate(recall = paste0(sprintf(recall_mean, fmt = "%#.2f"), " (", sprintf(recall_sd, fmt = "%#.2f"), ")"),
           precision = paste0(sprintf(prec_mean, fmt = "%#.2f"), " (", sprintf(prec_sd, fmt = "%#.2f"), ")"),
           f1 = paste0(sprintf(f1_mean, fmt = "%#.2f"), " (", sprintf(f1_sd, fmt = "%#.2f"), ")")) %>%
    select(algorithm, cat_activity, recall, precision, f1) %>%
    pivot_wider(names_from = cat_activity, values_from = recall:f1)

  tab_overall =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100",  "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
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
    mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)", "Verisense (revised)")) %>%
    kableExtra::kable(align = "llll", booktabs = TRUE,  format = "latex", col.names =
                        c("Algorithm", "Clemson", "Oxwalk", "Overall",
                          "Clemson", "Oxwalk", "Overall",
                          "Clemson",  "Oxwalk", "Overall")) %>%
    kableExtra::add_header_above(c(" " = 1, "Recall" = 3,
                                   "Precision" = 3, "F1 Score" =3)) %>%
    kableExtra::kable_styling(latex_options = "scale_down")

  # determine best
  accuracy_df %>% group_by(algorithm, cat_activity) %>% summarize(f1 = mean(f1)) %>%
    select(algorithm, f1, cat_activity) %>%
    filter(cat_activity == "clemson_overall") %>%  arrange(desc(f1))


  # main manuscript boxplots of metrics
  recog_median =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
    group_by(algorithm, cat_activity) %>%
    summarize(across(c(recall, prec, f1),
                     list(median = ~median(.x)))) %>%
    pivot_longer(cols = ends_with("median")) %>%
    mutate(algorithm = sub(".*steps\\_(.+)\\_30.*", "\\1", algorithm)) %>%
    ungroup()

  level_order =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100",  "clemson_overall") &
             grepl("30", algorithm)) %>%
    group_by(algorithm) %>%
    summarize(median = median(f1)) %>%
    ungroup() %>%
    arrange(median) %>%
    pull(algorithm)


  # each row is subject, activity
  recog_stats =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100","clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
    pivot_longer(cols = recall:f1)


  # overall panel
  overall =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
    pivot_longer(cols = recall:f1) %>%
    mutate(cat_activity = "overall")
  labs = c("F1 Score", "Precision", "Recall")
  names(labs) = c("f1", "prec", "recall")

  plot = overall %>%
    mutate(value= ifelse(name == "prec" & id_study == "marea", NA, value),
           cat_activity = "Overall") %>%
    ggplot(aes(x = factor(algorithm, levels = level_order), y = value, col = algorithm))+
    geom_boxplot(outlier.shape = NA, position = position_dodge())+
    geom_jitter(width=.1, alpha=.5)+
    facet_grid(name ~ cat_activity,
               labeller = labeller(name = labs)) +
    scale_color_brewer(palette  = "Dark2",name = "",
                       labels = c("ActiLife", "ADEPT", "Oak","Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)", "Verisense (revised"))+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1, vjust =1),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10))+
    labs(x = "", y = "")+
    guides(colour = guide_legend(nrow = 4))+
    scale_x_discrete(labels = c("ADEPT", "SDT", "Oak", "ActiLife", "Verisense (Original)", "Verisense (Revised)", "Stepcount (RF)", "Stepcount (SSL)"))
  svg(here::here("manuscript_figures", "boxplot_overall.svg"))
  plot
  dev.off()

  labs2 = c("Clemson", "OxWalk")
  names(labs2) = c("clemson_overall",  "oxwalk100")

  # current figure
  plot =
    recog_stats %>%
    mutate(value= ifelse(name == "prec" & id_study == "marea", NA, value)) %>%
    mutate(cat_activity = factor(cat_activity, levels = c("clemson_overall",  "oxwalk100"))) %>%
    ggplot(aes(x = factor(algorithm, levels = level_order), y = value, col = algorithm))+
    geom_boxplot(outlier.shape = NA, position = position_dodge())+
    geom_jitter(width=.1, alpha=.5)+
    facet_grid(name ~ cat_activity,
               labeller = labeller(name = labs,
                                   cat_activity = labs2)) +
    scale_color_brewer(palette  = "Dark2",name = "",
                       labels = c("ActiLife", "ADEPT", "Oak","Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)", "Verisense (revised)"))+
    theme_bw()+
    theme(legend.position = "none",
          # legend.position = c(.49, .6),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(1, 1, 1, 1),
          axis.text.x = element_text(angle = 30, hjust = 1, vjust =1),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 12))+
    labs(x = "", y = "")+
    guides(colour = guide_legend(nrow = 4))+
    scale_x_discrete(labels = c("ADEPT", "SDT", "Oak", "ActiLife", "Verisense (Original)", "Verisense (Revised)", "Stepcount (RF)", "Stepcount (SSL)"))

  svg(here::here("manuscript_figures", "boxplot_all.svg"))
  plot
  dev.off()




  # significance testing
  # generate tile plots with p values for ease of writing results
  recog_stats_tmp =
    accuracy_df %>%
    filter(cat_activity %in% c("oxwalk100", "clemson_overall") &
             grepl("30", algorithm)) %>%
    filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>% # remove the raw 30 hz
    select(recall, prec, f1, id_subject, id_study = cat_activity, algorithm)
  # create overall df
  recog_stats_test =
    recog_stats_tmp %>% mutate(id_study = "overall") %>%
    bind_rows(accuracy_df %>%
                filter(cat_activity %in% c("oxwalk100", "clemson_overall") &
                         grepl("30", algorithm)) %>%
                filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>% # remove the raw 30 hz
                select(recall, prec, f1, id_subject, id_study=cat_activity, algorithm))

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
  pairs = expand.grid.unique(x = c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vsores", "vsrres"),
                             y =  c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vsores", "vsrres")) %>%
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
    scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt", "vso")) +
    scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vso", "vsr"))+
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
    scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt", "vso")) +
    scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vso", "vsr"))+
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
    scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt", "vso")) +
    scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vso", "vsr"))+
    labs(title = "p values for prec score")

  cowplot::plot_grid(f1, prec, recall)

}

