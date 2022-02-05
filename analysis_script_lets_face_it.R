# Title     : Let's face it: Lateralization of the face perception 
# Objective : Test effect of handedness on lateralization score
# Created by: jose c. garcia alanis
# Created on: 2022-02-01
# R version : R version 4.1.2 (2021-11-01)

# set working directory
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

# function for installing and loading packages
pkgcheck <- function( packs ) {
  
  # Check which packages are not installed
  if ( sum(!packs %in% installed.packages()[, 'Package'])) {
    # install them
    install.packages( packs[ which(!packs %in% installed.packages()[, 'Package']) ], 
                      dependencies = T)
  }
  
  # load all packages
  sapply(packs, require, character.only =  T)
  
}

apa <- function(x, title = " ", stub = T) {
  gt(x, rownames_to_stub = stub) %>%
    tab_stubhead(label = "Predictor") %>%
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      stub.border.color = "white",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) %>%
    cols_align(align="center") %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    #title setup
    tab_header(
      title = html("<i>", title, "</i>")
    ) %>%
    opt_align_table_header(align = "left")
}


# 1) get the data --------------------------------------------------------------
pkgcheck(c('dplyr', 'tidyr'))

# import csv
data <- read.table('./LI_results_indiv-maxima_ROI-10mm_tthreshold_001unc.csv',
                   header = T, sep = ',', na.strings = c(9999.0000))


# 2) prepare demographic data for modelling ------------------------------------

# categorical variables to factors, mean-center continuous variables
data <- data %>%
  mutate(sex = ifelse(sex == 1, 'female', 'male'),
         hand = ifelse(EHI_handedness == 1, 'right handed', 'left handed'),
         age_c = age - mean(age)) %>%
  mutate(sex = factor(sex),
         hand = factor(hand))


# select relevant columns
data <- data %>%
  select(subID, FFA_LI_wm:STS_LI_wm, sex, age_c, hand, EHI_handedness) %>%
  gather(area, value, FFA_LI_wm:STS_LI_wm)

# remove suffix '_LI_wm' from variable area
data$area <- gsub(data$area, pattern = '_LI_wm', replacement = '')
data$area <- factor(data$area, levels = c('OFA', 'FFA', 'STS'))

# li-values should be positive for gamma model
data <- data %>% 
  mutate(li_positive = (value + 1))

# 3) plot distribution of LI-values --------------------------------------------
pkgcheck(c('dplyr', 'ggplot2', 'viridis', 'ggbeeswarm', 'see', 'Hmisc'))

# create density plots
pn <- position_nudge(x = 0.15)
li_plot <- ggplot(data,
                  aes(x = EHI_handedness, y = value,
                      fill = area, color = hand, shape = hand)) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = -0.2, ymax = 0.2, 
           alpha = .1) +
  geom_hline(yintercept = 0.0, linetype='dotted', size = 0.8) +
  geom_beeswarm(size = 2, color = 'black', alpha = 0.75,stroke = 1.0, 
                show.legend = F) +
  geom_violinhalf(position = pn, width = 0.70, alpha = 0.75, size = 0.8, show.legend = F) +
  geom_boxplot(position = pn, width = 0.10, alpha = 1.0, size = 0.8, 
               color = 'black', outlier.shape = NA, show.legend = F) +
  scale_shape_manual(values = c(23, 21)) +
  scale_color_manual(values = c('red2', 'black')) +
  facet_wrap(~ area, scales = 'free', ncol = 3) +
  scale_y_continuous(limits = c(-1.0, 1.0),
                     breaks = seq(-1.0 , 1.0, 0.5)) +
  scale_x_continuous(limits = c(0.8, 2.5),
                     breaks = seq(1, 2),
                     labels = c('Right', 'Left')) +
  coord_flip() +
  labs(title = 'ROI size: 10mm',
       x = 'Handedness',
       y = 'LI Value',
       fill = NULL,
       shape = 'Handedness',
       color = 'Handedness') +
  scale_fill_viridis(option = 'D', discrete = T, begin = 0.1) +
  # scale_color_manual(values = c('red', 'black')) +
  geom_segment(aes(x = -Inf, y = -1.0, xend = -Inf, yend = 1.0),
               color = 'black', size = rel(1.0), linetype = 1) +
  geom_segment(aes(x = 1, y = -Inf, xend = 2, yend = -Inf),
               color = 'black', size = rel(1.0), linetype = 1) +
  theme(plot.margin = unit(c(10, 10, 10, 10), 'pt'),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray98"),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
                                    margin = margin(t = 10)),
        axis.title.y= element_text(color = 'black', size = 12, face = 'bold',
                                   margin = margin(r = 10)),
        axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', face = 'bold', size = 12),
        strip.text = element_text(color = 'black', size = 12, face = 'bold'),
        strip.background = element_blank(),
        legend.position='bottom',
        legend.text = element_text(size = 9),
        legend.title = element_text(hjust = 0.5, face = 'bold'),
        panel.spacing = unit(1.0, "lines")) +
  guides(fill = "none",
         # shape = guide_legend(title.position = "bottom"),
         shape = "none"); li_plot
ggsave(filename = './LI_plot_10mm_hand.tiff',
       plot = li_plot, 
       width = 25, height = 10,  units = 'cm', dpi = 1200)

# create dot plot
int_plot <- ggplot(data = data, 
                   aes(x = sex, y = value, 
                       color = hand, shape = sex, fill = hand)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.2, ymax = 0.2,
           alpha = .1) +
  geom_segment(aes(x = -Inf, y = -1, xend = -Inf, yend = 1),
               color = 'black', linetype = 1, size = 1) +
  geom_segment(aes(x = 'female', y = -Inf, xend = 'male', yend = -Inf),
               color = 'black', linetype = 1, size = 1) +
  geom_segment(aes(x = -Inf, y = 0, xend = Inf, yend = 0),
               color = 'black', linetype = 3, size = 1) +
  geom_beeswarm(dodge.width = 1.0, size = 2.0, stroke = 1.0, fill = NA) +
  stat_summary(fun.data = 'mean_cl_boot', position = position_dodge(0.5),
               geom = 'linerange',
               size = 0.8, show.legend = F) +
  stat_summary(fun.data = 'mean_cl_boot', position = position_dodge(0.5),
               color = 'black', geom = 'point',
               size = 2.5, stroke = 1.0, show.legend = F) +
  facet_wrap(~ area, scales = 'free') +
  scale_color_manual(values = c("#EF8A62", "#67A9CF")) +
  scale_fill_manual(values = c("#EF8A62", "#67A9CF")) +
  scale_shape_manual(values = c(21, 23)) +
  theme(plot.margin = unit(c(5, 30, 5, 5), 'pt'),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 18, face = 'bold'), 
        plot.title = element_text(color = 'black', size = 18, face = 'bold', 
                                  hjust = 0),
        axis.title.x = element_text(color = 'black', size = 16, face = 'bold',
                                    margin = margin(t = 10)),
        axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
                                    margin = margin(r = 10)),
        axis.text.x = element_text(color = 'black', size = 14),
        axis.text.y = element_text(color = 'black', size = 14),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray98"),
        legend.text = element_text(color="black", size=rel(1.25)),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white"),
        legend.key = element_blank(),
        legend.position = "bottom") +
  labs(y = 'Observed LI-Value', x = '',
       fill = 'Handedness', color = 'Handedness',
       shape = 'Sex'); int_plot
ggsave(filename = './interaction_plot_area_sex_hand.tiff',
       plot = int_plot, 
       width = 30, height = 12.5, units = 'cm',
       dpi = 1200)

# 4) compute descriptive statistics --------------------------------------------
pkgcheck('sjPlot')

data %>% 
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './overall.html', digits = 3)

data %>% 
  group_by(sex) %>%
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  as.data.frame() %>%
  tab_df(file = './sex_descriptives.html', digits = 3)

data %>% 
  group_by(hand) %>%
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './hand_descriptives.html', digits = 3)

data %>% 
  group_by(area, hand) %>%
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './area_hand_descriptives.html', digits = 3)

data %>% 
  group_by(area) %>%
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './area_descriptives.html', digits = 3)

data %>% 
  group_by(hand, sex) %>%
  summarise(m = mean(value, na.rm=T),
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './hand_sex_descriptives.html', digits = 3)

data %>% 
  group_by(hand, sex, area) %>%
  summarise(m = mean(value, na.rm=T),
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './hand_sex_area_descriptives.html', digits = 3)

# 5) compute mixed effects model -----------------------------------------------
pkgcheck(c('lme4', 'car', 'performance', 'effectsize', 'gt'))

# overall model
mod_gamma_li <- glmer(data = data,
                      li_positive ~ age_c + sex * hand * area + (1|subID),
                      contrasts = list(area = 'contr.sum',
                                       sex = 'contr.sum',
                                       hand = 'contr.sum'), 
                      family = Gamma(link = "log"),
                      control = glmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=2e5)))
anova_mod_gamma <- car::Anova(mod_gamma_li, type = 'III')
anova_mod_gamma %>%
  apa("ANOVA table overall LI-model: Gamma model for LI values") %>%
  fmt_number(columns = c(2, 4), decimals = 4) %>%
  gtsave(
    "./anova_mod_gamma_overall.html", inline_css = TRUE)
tiff(filename = './gamma_overall_model_diagnostics.tiff',
     width = 15, height = 20, units = 'cm', res =  1200)
check_model(mod_gamma_li)
dev.off()
model_performance(mod_gamma_li) %>%
  apa("Overall Gamma LI-model performance", stub = F) %>%
  fmt_number(columns = c(1:7), decimals = 4) %>%
  tab_source_note(
    source_note = 
      md(paste0('*Note.* Foumla: `li ~ ', as.character(formula(mod_gamma_li))[3], '`' ))
  ) %>%
  gtsave(
    "./mod_gamma_overall_performance.html", inline_css = TRUE)
standardize_parameters(mod_gamma_li) %>%
  apa("Standardized beta coefficients: 
      Overall Gamma LI-model", stub = F) %>%
  fmt_number(columns = c(2, 5), decimals = 4) %>%
  tab_footnote(
    footnote = "age, centred around zero",
    locations = cells_body(
      columns = Parameter,
      rows = 2)) %>%
  tab_footnote(
    footnote = "sex1 = female",
    locations = cells_body(
      columns = Parameter,
      rows = 3)) %>%
  tab_footnote(
    footnote = "hand1 = left handed",
    locations = cells_body(
      columns = Parameter,
      rows = 4)) %>%
  tab_footnote(
    footnote = "area1 = OFA",
    locations = cells_body(
      columns = Parameter,
      rows = 5)) %>%
  tab_footnote(
    footnote = "area2 = FFA",
    locations = cells_body(
      columns = Parameter,
      rows = 6)) %>%
  gtsave(
    "./standardized_beta_mod_gamma.html", inline_css = TRUE)


# right-handers model
mod_gamma_li_rh <- glmer(data = filter(data, hand == 'right handed'),
                      li_positive ~ age_c + sex * area + (1|subID),
                      contrasts = list(area = 'contr.sum',
                                       sex = 'contr.sum'), 
                      family = Gamma(link = "log"))
anova_mod_gamma_rh <- car::Anova(mod_gamma_li_rh, type = 'III')
anova_mod_gamma_rh %>%
  apa("ANOVA table right handers' LI-model: 
      Gamma model for LI values from right handed participants") %>%
  fmt_number(columns = c(2, 4), decimals = 4) %>%
  gtsave(
    "./anova_mod_gamma_right_handers.html", inline_css = TRUE)
  


# left-handers model
mod_gamma_li_lh <- glmer(data = filter(data, hand == 'left handed'),
                         li_positive ~ age_c + sex * area + (1|subID),
                         contrasts = list(area = 'contr.sum',
                                          sex = 'contr.sum'), 
                         family = Gamma(link = "log"))
anova_mod_gamma_lh <- car::Anova(mod_gamma_li_lh, type = 'III')
anova_mod_gamma_lh %>%
  apa("ANOVA table left handers' LI-model: 
      Gamma model for LI values from left handed participants") %>%
  fmt_number(columns = c(2, 4), decimals = 4) %>%
  gtsave(
    "./anova_mod_gamma_left_handers.html", inline_css = TRUE)


# 6) compute exploratory OLS model ---------------------------------------------
pkgcheck('effectsize')

mod_ffa <- lm(data = filter(data, area == 'FFA'),
              value ~ age_c + sex * hand,
              contrasts = list(sex = 'contr.sum',
                               hand = 'contr.sum'))
anova_mod_ffa <- car::Anova(mod_ffa, type = 'III', test = 'F')
anova_mod_ffa %>%
  apa("ANOVA table FFA LI-model") %>%
  fmt_number(columns = c(2, 5), decimals = 4) %>%
  tab_source_note(
    source_note = 
      md(paste0('*Note.* Foumla: `li ~ ', as.character(formula(mod_ffa))[3], '`' ))
  ) %>%
  gtsave(
    "./anova_mod_ffa.html", inline_css = TRUE)

standardize_parameters(mod_ffa) %>%
  apa("Standardized beta coefficients: 
      FFA LI-model", stub = F) %>%
  fmt_number(columns = c(2:5), decimals = 4) %>%
  tab_footnote(
    footnote = "age, centred around zero",
    locations = cells_body(
      columns = Parameter,
      rows = 2)) %>%
  tab_footnote(
    footnote = "sex1 = female",
    locations = cells_body(
      columns = Parameter,
      rows = 3)) %>%
  tab_footnote(
    footnote = "hand1 = left handed",
    locations = cells_body(
      columns = Parameter,
      rows = 4))  %>%
  gtsave(
    "./standardized_beta_mod_ffa.html", inline_css = TRUE)
  
# check residuals
plot(mod_ffa, ask = F)

# 7) compute pairwise contrasts for interaction --------------------------------
pkgcheck('emmeans')

ffa_means <- emmeans(mod_ffa, ~ hand | sex)
ffa_means %>%
  data.frame() %>%
  apa("Estimated marginal means (emmeans) for FFA model",
      stub = F) %>%
  fmt_number(columns = c(3:7), decimals = 4) %>%
  gtsave(
    "./emmeans_mod_ffa.html", inline_css = TRUE)
  
contrast(ffa_means, 'tukey', adjust = 'fdr') %>%
  data.frame() %>%
  apa("Pairwise contrasts for FFA model",
      stub = F) %>%
  fmt_number(columns = c(3:7), decimals = 4) %>%
  gtsave(
    "./contrasts_mod_ffa.html", inline_css = TRUE)
