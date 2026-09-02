.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
suppressPackageStartupMessages({
  library(dplyr); library(logistf); library(ggplot2); library(cowplot)
})

d <- readRDS('F:/openclaw_workspace/dataset/PIC_cardiac_final.rds')

# === Prepare data ===
# Model specification matches Table 2: high-risk complex, CoA/IAA, valve/RVOT,
# and Other groups excluded (collinearity with neonatal age / limited events);
# Other Combined = VSD+ASD Repair, VSD+Combined, ASD+PDA. Analytic sample = 1,910 (9 deaths).
d_model <- d %>%
  mutate(
    age_cat = case_when(
      age_years < 28/365 ~ 'Neonatal (<28d)',
      age_years < 1 ~ 'Infant (28d-1yr)',
      TRUE ~ 'Child (>=1yr)'
    ),
    age_cat = factor(age_cat, levels = c('Child (>=1yr)', 'Infant (28d-1yr)', 'Neonatal (<28d)')),
    model_group = case_when(
      surg_group_main == 'VSD Repair alone' ~ 'VSD alone',
      surg_group_main == 'ASD Repair alone' ~ 'ASD alone',
      surg_group_main == 'PDA Closure alone' ~ 'PDA alone',
      surg_group_main == 'TOF Repair' ~ 'TOF Repair',
      surg_group_main %in% c('VSD+ASD Repair', 'VSD + Combined', 'ASD+PDA') ~ 'Other Combined',
      TRUE ~ 'EXCLUDED' # High-Risk Complex, CoA/IAA+Combined, Valve, RVOT, Other
    )) %>%
  filter(model_group != 'EXCLUDED') %>%
  mutate(model_group = factor(model_group),
         model_group = relevel(model_group, ref = 'VSD alone'),
         male = ifelse(gender == 'Male', 1, 0))

# === Primary model: age categorical + procedure ===
fit1 <- logistf(death ~ age_cat + model_group, data = d_model)

# === Sensitivity: + sex ===
fit2 <- logistf(death ~ age_cat + model_group + male, data = d_model)

# === Sensitivity: age continuous ===
fit3 <- logistf(death ~ age_years + model_group, data = d_model)

# === Extract results ===
extract_or <- function(fit, label_prefix = '') {
  res <- data.frame(
    Variable = names(coef(fit)),
    OR = exp(coef(fit)),
    Lower = exp(fit$ci.lower),
    Upper = exp(fit$ci.upper),
    P = fit$prob,
    stringsAsFactors = FALSE
  )
  # Remove intercept
  res <- res[!grepl('Intercept', res$Variable), ]
  # Clean variable names for display
  res$Label <- gsub('age_cat', '', res$Variable)
  res$Label <- gsub('model_group', '', res$Label)
  res$Label <- gsub('_', ' ', res$Label)
  res$Label <- gsub('male', 'Male vs Female', res$Label)
  res$Label <- gsub('age_years', 'Age (per year)', res$Label)
  res$Model <- label_prefix
  return(res)
}

res1 <- extract_or(fit1, 'Primary Model')
res2 <- extract_or(fit2, '+ Sex (Sensitivity)')
res3 <- extract_or(fit3, 'Age Continuous (Sensitivity)')

all_res <- rbind(res1, res2, res3)

# Categories for faceting
all_res$Panel <- ifelse(grepl('Male|Age\\(per', all_res$Variable), 'Sensitivity Analyses', 'Primary Model')
all_res$Panel <- ifelse(grepl('Sex|age_years', all_res$Label), 'Sensitivity Analyses', 'Primary Model')

# Manual panel assignment
all_res$Panel[all_res$Model == 'Primary Model'] <- 'Primary Model'
all_res$Panel[all_res$Model == '+ Sex (Sensitivity)'] <- 'Sensitivity: Adding Sex'
all_res$Panel[all_res$Model == 'Age Continuous (Sensitivity)'] <- 'Sensitivity: Age Continuous'

# Clean labels for display
all_res$Display <- all_res$Label
all_res$Display <- gsub('Neonatal \\(<28d\\)', 'Neonatal (<28d) vs Child', all_res$Display)
all_res$Display <- gsub('Infant \\(28d-1yr\\)', 'Infant (28d-1yr) vs Child', all_res$Display)

# Factor for ordering
all_res <- all_res %>%
  mutate(Display = factor(Display, levels = rev(unique(Display))))

# Significance asterisks (no longer displayed; values are reported in Table 2)

# === Forest plot ===
cat('Generating forest plot...\n')

p <- ggplot(all_res, aes(x = OR, y = Display)) +
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'grey50', linewidth = 0.5) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = 'grey30', linewidth = 0.8) +
  geom_point(aes(color = Panel), size = 3) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 300),
                labels = c('0.01', '0.1', '1', '10', '100', '300')) +
  scale_color_manual(values = c('#2C7BB6', '#E31A1C', '#FDBF6F')) +
  labs(x = 'Odds Ratio (log scale)', y = '') +
  theme_classic(base_size = 11) +
  theme(plot.title = element_text(face = 'bold', size = 13),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.y = element_text(size = 10),
        strip.background = element_rect(fill = 'grey95'),
        strip.text = element_text(face = 'bold', size = 10)) +
  facet_wrap(~ Panel, ncol = 1, scales = 'free_y')

# Adjust x-axis for readability
p <- p + coord_cartesian(xlim = c(0.005, 380))

# Save
outdir <- 'figures'
ggsave(file.path(outdir, 'Fig3_Forest_Plot.png'), p, width = 7, height = 6.5, dpi = 300)
ggsave(file.path(outdir, 'Fig3_Forest_Plot.tiff'), p, width = 7, height = 6.5, dpi = 1200, compression = 'lzw')

cat('Forest plot saved.\n')
