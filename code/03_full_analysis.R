.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tableone)
  library(pROC)
  library(logistf)
  library(ggplot2)
  library(cowplot)
  library(scales)
})

setwd('F:/openclaw_workspace')
outdir <- 'figures'
dir.create(outdir, showWarnings = FALSE)

# ============================================================
# Load improved classification dataset
# ============================================================
d <- readRDS('dataset/PIC_cardiac_improved.rds')
d <- d %>% filter(!is.na(age_years))

cat(sprintf('Analysis dataset: %d patients, %d deaths (%.1f%%)\n',
            nrow(d), sum(d$death), 100 * mean(d$death)))

# ============================================================
# TABLE 1: Baseline characteristics by procedure (improved groups)
# ============================================================
cat('\n========== TABLE 1: Baseline by Main Procedure Group ==========\n')

# Define display order
group_order <- c('VSD Repair alone', 'ASD Repair alone', 'PDA Closure alone',
                 'VSD+ASD Repair', 'VSD + Combined', 'ASD+PDA',
                 'TOF Repair', 'Complex Neonatal/Other', 'CoA/IAA+Combined',
                 'Valve Procedures', 'RVOT Procedure', 'Other')
d$surg_group_main <- factor(d$surg_group_main, levels = group_order)

t1 <- d %>%
  group_by(surg_group_main, .drop = FALSE) %>%
  summarise(
    N = n(),
    Deaths = sum(death),
    Mortality = sprintf('%.1f%%', 100 * mean(death)),
    Age_median = sprintf('%.2f [%.2f-%.2f]',
      median(age_years, na.rm = TRUE),
      quantile(age_years, 0.25, na.rm = TRUE),
      quantile(age_years, 0.75, na.rm = TRUE)),
    Male = sprintf('%d (%.1f%%)', sum(gender == 'Male', na.rm = TRUE),
                   100 * mean(gender == 'Male', na.rm = TRUE)),
    ICU_LOS = sprintf('%.1f [%.1f-%.1f]',
      median(icu_los_days, na.rm = TRUE),
      quantile(icu_los_days, 0.25, na.rm = TRUE),
      quantile(icu_los_days, 0.75, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  filter(!is.na(surg_group_main))

print(as.data.frame(t1), row.names = FALSE)

# Neonatal focus
cat('\n--- PDA Closure alone: Age breakdown ---\n')
pda_age <- d %>% filter(surg_group_main == 'PDA Closure alone') %>%
  mutate(age_group = ifelse(age_years < 1/12, 'Neonatal', 'Non-neonatal')) %>%
  group_by(age_group) %>%
  summarise(n = n(), deaths = sum(death), mortality = sprintf('%.1f%%', 100*mean(death)))
print(pda_age)

# ============================================================
# TABLE 2: Mortality by age group (improved)
# ============================================================
cat('\n\n========== TABLE 2: Mortality by Age Group ==========\n')
age_mort <- d %>%
  mutate(
    age_grp = case_when(
      age_years < 1/12 ~ '< 1 month',
      age_years < 1    ~ '1m-1yr',
      age_years < 5    ~ '1-5yr',
      age_years < 12   ~ '5-12yr',
      TRUE              ~ '>=12yr'
    ),
    age_grp = factor(age_grp, levels = c('< 1 month','1m-1yr','1-5yr','5-12yr','>=12yr'))
  ) %>%
  group_by(age_grp, .drop = FALSE) %>%
  summarise(
    n = n(),
    deaths = sum(death),
    mortality_pct = round(100 * mean(death), 1),
    .groups = 'drop'
  )
print(as.data.frame(age_mort), row.names = FALSE)

# ============================================================
# TABLE 3: Vasoactive drug use (improved)
# ============================================================
cat('\n\n========== TABLE 3: Vasoactive Drug Use ==========\n')
vaso <- d %>%
  group_by(surg_group_main, .drop = FALSE) %>%
  summarise(
    n = n(),
    vaso_n = sum(vasoactive_drug %in% TRUE, na.rm = TRUE),
    vaso_pct = round(100 * mean(vasoactive_drug %in% TRUE, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  filter(!is.na(surg_group_main))
print(as.data.frame(vaso), row.names = FALSE)

# ============================================================
# FIRTH LOGISTIC REGRESSION (improved)
# ============================================================
cat('\n\n========== FIRTH LOGISTIC REGRESSION ==========\n')

# Prepare: combine groups into meaningful model categories
# Event counts: TOF 4, Complex 10, VSD alone 6, PDA alone 3, VSD+Combined 0, etc.
# Must ensure adequate events per predictor

d_model <- d %>%
  mutate(
    # Create model groups with adequate event counts
    model_group = case_when(
      surg_group_main == 'Complex Neonatal/Other' ~ 'Complex Neonatal',
      surg_group_main == 'TOF Repair' ~ 'TOF Repair',
      surg_group_main %in% c('PDA Closure alone') ~ 'PDA alone',
      surg_group_main %in% c('VSD + Combined', 'ASD+PDA', 'CoA/IAA+Combined',
                              'VSD+ASD Repair', 'Valve Procedures', 'Other') ~ 'Other Combined',
      surg_group_main == 'VSD Repair alone' ~ 'VSD alone',
      surg_group_main == 'ASD Repair alone' ~ 'ASD alone',
      TRUE ~ 'Other Combined'
    ),
    model_group = factor(model_group),
    male = ifelse(gender == 'Male', 1, 0)
  )

# Set reference: VSD alone
d_model$model_group <- relevel(d_model$model_group, ref = 'VSD alone')

cat(sprintf('\nModel groups:\n'))
print(table(d_model$model_group))
cat(sprintf('Deaths per group:\n'))
print(aggregate(death ~ model_group, data = d_model, FUN = function(x) c(n=sum(x), total=length(x))))

# Firth regression
fit <- logistf(death ~ age_years + male + model_group, data = d_model)

cat('\nFirth logistic regression results:\n')
cat(sprintf('N = %d, Events = %d\n', nrow(d_model), sum(d_model$death)))
cat(sprintf('Model log-likelihood: %.2f\n', fit$loglik[2]))
cat(sprintf('Prob > chi2: %.6f\n', fit$prob))

# Extract results
res <- data.frame(
  Variable = names(coef(fit)),
  OR = round(exp(coef(fit)), 2),
  OR_lower = round(exp(fit$ci.lower), 2),
  OR_upper = round(exp(fit$ci.upper), 2),
  p_value = round(fit$prob, 4),
  row.names = NULL
)
res$CI <- sprintf('%.2f-%.2f', res$OR_lower, res$OR_upper)
res$OR_CI <- sprintf('%.2f (%.2f-%.2f)', res$OR, res$OR_lower, res$OR_upper)

cat('\n')
print(res[, c('Variable', 'OR', 'CI', 'p_value')], row.names = FALSE)

# C-statistic
pred <- fitted(fit)
roc_obj <- roc(d_model$death, pred)
cat(sprintf('\nC-statistic (AUC): %.3f (95%% CI: %.3f-%.3f)\n',
            auc(roc_obj), ci.auc(roc_obj)[1], ci.auc(roc_obj)[3]))

# ============================================================
# FIGURES (improved)
# ============================================================
cat('\n\n========== GENERATING FIGURES ==========\n')

theme_pub <- theme_classic(base_size = 11) +
  theme(plot.title = element_text(face = 'bold', size = 13),
        axis.title = element_text(size = 11),
        legend.position = 'bottom',
        panel.grid.major.y = element_line(color = 'grey90', linewidth = 0.3))

# Color palette for 12 groups
n_groups <- length(levels(d$surg_group_main))
colors <- c('#2C7BB6', '#A6CEE3', '#abd9e9', '#74add1',
            '#FDBF6F', '#FF7F00', '#E31A1C', '#FB9A99',
            '#CAB2D6', '#6A3D9A', '#B2DF8A', '#33A02C')[1:n_groups]
names(colors) <- levels(d$surg_group_main)

# ---- Figure 1: Age Distribution ----
valid_age <- d %>% filter(age_years < 18)
p1 <- valid_age %>%
  ggplot(aes(x = age_years)) +
  geom_histogram(binwidth = 0.5, fill = '#2C7BB6', color = 'white', alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 16, 2), limits = c(0, 16)) +
  scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
  labs(x = 'Age (years)', y = 'Number of Patients',
       title = 'A. Age Distribution') +
  theme_pub +
  annotate('text', x = 12, y = max(ggplot_build(p1)$data[[1]]$count) * 0.9,
           label = sprintf('N = %d', nrow(valid_age)), size = 4, hjust = 0)

# ---- Figure 2: Mortality by Procedure (Main Groups) ----
plot_data <- d %>%
  group_by(surg_group_main, .drop = FALSE) %>%
  summarise(
    n = n(),
    deaths = sum(death),
    mortality = mean(death),
    se = sqrt(mean(death) * (1 - mean(death)) / n()),
    .groups = 'drop'
  ) %>%
  filter(!is.na(surg_group_main)) %>%
  mutate(
    label_short = case_when(
      surg_group_main == 'Complex Neonatal/Other' ~ 'Complex\nNeonatal',
      surg_group_main == 'CoA/IAA+Combined' ~ 'CoA/IAA\n+Combined',
      surg_group_main == 'RVOT Procedure' ~ 'RVOT\nProcedure',
      TRUE ~ surg_group_main
    ),
    label_short = factor(label_short, levels = rev(unique(label_short)))
  )

# Fix SE for zero-mortality groups
plot_data$se[plot_data$mortality == 0] <- 0

p2 <- plot_data %>%
  ggplot(aes(x = label_short, y = mortality, fill = surg_group_main)) +
  geom_bar(stat = 'identity', width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = pmax(mortality - 1.96*se, 0), 
                    ymax = mortality + 1.96*se), width = 0.2) +
  geom_text(aes(label = sprintf('%.1f%%', 100*mortality)), 
            hjust = -0.2, size = 3.2) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0, 0.12, 0),
                     limits = c(0, 0.16)) +
  scale_fill_manual(values = colors, guide = 'none') +
  labs(x = '', y = 'In-Hospital Mortality',
       title = 'B. Mortality by Procedure Type') +
  theme_pub +
  coord_flip()

# ---- Figure 3: ICU LOS by Procedure ----
p3_data <- d %>%
  filter(!is.na(surg_group_main), icu_los_days < 60) %>%
  mutate(surg_group_main = factor(surg_group_main, levels = rev(levels(d$surg_group_main))))

p3 <- p3_data %>%
  ggplot(aes(x = surg_group_main, y = icu_los_days, fill = surg_group_main)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = median, geom = 'point', size = 2.5, shape = 18, color = 'black') +
  scale_fill_manual(values = colors, guide = 'none') +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.5, 1, 2, 5, 10, 20, 50)) +
  labs(x = '', y = 'ICU LOS (days, log scale)',
       title = 'C. ICU Length of Stay') +
  theme_pub +
  coord_flip()

# ---- Figure 4: Mortality by Age ----
age_data <- d %>%
  mutate(age_grp = case_when(
    age_years < 1/12 ~ '< 1 month',
    age_years < 1    ~ '1m-1yr',
    age_years < 5    ~ '1-5yr',
    age_years < 12   ~ '5-12yr',
    TRUE              ~ '>=12yr'
  )) %>%
  group_by(age_grp) %>%
  summarise(
    n = n(),
    deaths = sum(death),
    mortality = mean(death),
    se = sqrt(mean(death) * (1 - mean(death)) / n()),
    .groups = 'drop'
  ) %>%
  mutate(age_grp = factor(age_grp, levels = c('< 1 month','1m-1yr','1-5yr','5-12yr','>=12yr')))

p4 <- age_data %>%
  ggplot(aes(x = age_grp, y = mortality, fill = age_grp)) +
  geom_bar(stat = 'identity', width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = pmax(mortality - 1.96*se, 0), 
                    ymax = mortality + 1.96*se), width = 0.2) +
  geom_text(aes(label = sprintf('%.1f%%\n(n=%d)', 100*mortality, n)), 
            vjust = -0.3, size = 3.3, lineheight = 0.9) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0, 0.15, 0)) +
  scale_fill_manual(values = c('#d73027','#fc8d59','#fee090','#91bfdb','#4575b4'),
                    guide = 'none') +
  labs(x = 'Age Group', y = 'In-Hospital Mortality',
       title = 'D. Mortality by Age Group') +
  theme_pub

# ---- Figure 5: Vasoactive Use by Procedure ----
p5_data <- d %>%
  filter(!is.na(surg_group_main)) %>%
  group_by(surg_group_main, .drop = FALSE) %>%
  summarise(
    n = n(),
    vaso_pct = mean(vasoactive_drug %in% TRUE, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(surg_group_main)) %>%
  mutate(surg_group_main = factor(surg_group_main, levels = rev(levels(d$surg_group_main))))

p5 <- p5_data %>%
  ggplot(aes(x = surg_group_main, y = vaso_pct, fill = surg_group_main)) +
  geom_bar(stat = 'identity', width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf('%.1f%%', 100*vaso_pct)), 
            hjust = -0.1, size = 3.2) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0, 0.1, 0),
                     limits = c(0, 0.55)) +
  scale_fill_manual(values = colors, guide = 'none') +
  labs(x = '', y = 'Vasoactive Drug Use (%)',
       title = 'E. Vasoactive Requirement by Procedure') +
  theme_pub +
  coord_flip()

# Save individual figures (PNG + TIFF)
cat('\nSaving figures...\n')

ggsave(file.path(outdir, 'Fig1_Age_Distribution.png'), p1, width = 8, height = 5, dpi = 300)
ggsave(file.path(outdir, 'Fig2_Mortality_by_Surgery.png'), p2, width = 8, height = 5, dpi = 300)
ggsave(file.path(outdir, 'Fig3_ICU_LOS_by_Surgery.png'), p3, width = 8, height = 5, dpi = 300)
ggsave(file.path(outdir, 'Fig4_Mortality_by_Age.png'), p4, width = 8, height = 5, dpi = 300)
ggsave(file.path(outdir, 'Fig5_Vasoactive_Use.png'), p5, width = 8, height = 5, dpi = 300)

# High-res TIFF
ggsave(file.path(outdir, 'Fig1_Age_Distribution.tiff'), p1, width = 8, height = 5, dpi = 1200, compression = 'lzw')
ggsave(file.path(outdir, 'Fig2_Mortality_by_Surgery.tiff'), p2, width = 8, height = 5, dpi = 1200, compression = 'lzw')
ggsave(file.path(outdir, 'Fig3_ICU_LOS_by_Surgery.tiff'), p3, width = 8, height = 5, dpi = 1200, compression = 'lzw')
ggsave(file.path(outdir, 'Fig4_Mortality_by_Age.tiff'), p4, width = 8, height = 5, dpi = 1200, compression = 'lzw')
ggsave(file.path(outdir, 'Fig5_Vasoactive_Use.tiff'), p5, width = 8, height = 5, dpi = 1200, compression = 'lzw')

# Combined figure for manuscript
combined <- plot_grid(p1, p2, p3, p4, p5, ncol = 2, nrow = 3, 
                      rel_heights = c(1, 1, 1), align = 'v', axis = 'lr')
ggsave(file.path(outdir, 'Fig_Combined_Manuscript.png'), combined, width = 14, height = 18, dpi = 300)
ggsave(file.path(outdir, 'Fig_Combined_Manuscript.tiff'), combined, width = 14, height = 18, dpi = 1200, compression = 'lzw')

cat('\nAll figures saved.\n')

# Save model
saveRDS(fit, file.path(outdir, 'firth_model_improved.rds'))
saveRDS(roc_obj, file.path(outdir, 'roc_model_improved.rds'))

cat('\n========== ANALYSIS COMPLETE ==========\n')
