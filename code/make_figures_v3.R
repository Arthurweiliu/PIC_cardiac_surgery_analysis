# ============================================================
# make_figures_v3.R — Final figure set for manuscript v3
# Figure 1: perioperative outcomes by procedure (A/B/C)
# Figure 2: mortality by age group
# Figure S1: naive vs hierarchical classification mortality
# ============================================================
.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(cowplot); library(scales)
})

d <- readRDS('F:/openclaw_workspace/dataset/PIC_cardiac_final.rds')
outdir <- 'F:/openclaw_workspace/figures'
dir.create(outdir, showWarnings = FALSE)

# ---- Standardize terminology to manuscript ----
d <- d %>%
  mutate(group = case_when(
    surg_group_main == 'Complex Neonatal/Other' ~ 'High-Risk Complex',
    TRUE ~ surg_group_main
  ))

# Display order (Table 1 order; miscellaneous Tier-5 groups excluded from Fig 1)
order9 <- c('VSD Repair alone', 'ASD Repair alone', 'PDA Closure alone',
            'VSD+ASD Repair', 'VSD + Combined', 'ASD+PDA',
            'CoA/IAA+Combined', 'TOF Repair', 'High-Risk Complex')
d$group <- factor(d$group, levels = order9)

theme_pub <- theme_classic(base_size = 11) +
  theme(axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_line(color = 'grey90', linewidth = 0.3))

# Exact Clopper-Pearson 95% CI
exact_ci <- function(k, n) {
  t(sapply(seq_along(k), function(i) {
    if (is.na(n[i]) || n[i] == 0) return(c(NA, NA))
    as.numeric(binom.test(k[i], n[i])$conf.int)
  }))
}

# ---- Panel A: mortality by procedure (exact CI) ----
pa_data <- d %>%
  filter(!is.na(group)) %>%
  group_by(group, .drop = FALSE) %>%
  summarise(n = n(), deaths = sum(death, na.rm = TRUE), .groups = 'drop') %>%
  mutate(mort = deaths / n)
ci <- exact_ci(pa_data$deaths, pa_data$n)
pa_data$lo <- ci[, 1]; pa_data$hi <- ci[, 2]
pa_data$group <- factor(pa_data$group, levels = rev(levels(d$group)))

pA <- pa_data %>%
  ggplot(aes(x = group, y = mort)) +
  geom_bar(stat = 'identity', width = 0.62, fill = '#2C7BB6', alpha = 0.9) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, color = 'grey25', linewidth = 0.5) +
  geom_text(aes(label = sprintf('%.1f%%', 100 * mort)), hjust = -0.35, size = 3.4) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0, 0.06, 0),
                     limits = c(0, 0.33), breaks = seq(0, 0.30, 0.05)) +
  labs(x = '', y = 'In-hospital mortality') +
  theme_pub + coord_flip()

# ---- Panel B: ICU LOS (log scale) ----
d_b <- d %>% filter(!is.na(group), icu_los_days < 60) %>%
  mutate(group = factor(group, levels = rev(levels(d$group))))
pB <- d_b %>%
  ggplot(aes(x = group, y = icu_los_days)) +
  geom_boxplot(fill = '#74ADD1', alpha = 0.75, outlier.shape = NA, width = 0.62,
               color = '#2C7BB6') +
  stat_summary(fun = median, geom = 'point', size = 2.6, shape = 18, color = 'black') +
  scale_y_continuous(trans = 'log10', breaks = c(0.5, 1, 2, 5, 10, 20, 50)) +
  annotation_logticks(sides = 'b', colour = 'grey50', short = unit(0.08, 'cm'),
                      mid = unit(0.12, 'cm'), long = unit(0.2, 'cm')) +
  labs(x = '', y = 'ICU length of stay (days, log scale)') +
  theme_pub + coord_flip()

# ---- Panel C: vasoactive use ----
pc_data <- d %>%
  filter(!is.na(group)) %>%
  group_by(group, .drop = FALSE) %>%
  summarise(vaso = mean(vasoactive_drug %in% TRUE, na.rm = TRUE), .groups = 'drop') %>%
  mutate(group = factor(group, levels = rev(levels(d$group))))
pC <- pc_data %>%
  ggplot(aes(x = group, y = vaso)) +
  geom_bar(stat = 'identity', width = 0.62, fill = '#4575B4', alpha = 0.9) +
  geom_text(aes(label = sprintf('%.1f%%', 100 * vaso)), hjust = -0.25, size = 3.4) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0, 0.06, 0),
                     limits = c(0, 0.62)) +
  labs(x = '', y = 'Vasoactive medication use (%)') +
  theme_pub + coord_flip()

fig1 <- plot_grid(pA, pB, pC, ncol = 1, align = 'v', axis = 'lr',
                  labels = c('A', 'B', 'C'), label_size = 15,
                  rel_heights = c(1, 1, 1))

ggsave(file.path(outdir, 'Fig1_Periop_Outcomes.png'), fig1, width = 6.5, height = 9.4, dpi = 300)
ggsave(file.path(outdir, 'Fig1_Periop_Outcomes.tiff'), fig1, width = 6.5, height = 9.4, dpi = 1200, compression = 'lzw')
# Individual panels for flexible submission
ggsave(file.path(outdir, 'Fig1A_Mortality.png'), pA, width = 6.5, height = 3.13, dpi = 300)
ggsave(file.path(outdir, 'Fig1A_Mortality.tiff'), pA, width = 6.5, height = 3.13, dpi = 1200, compression = 'lzw')
ggsave(file.path(outdir, 'Fig1B_ICU_LOS.png'), pB, width = 6.5, height = 3.13, dpi = 300)
ggsave(file.path(outdir, 'Fig1B_ICU_LOS.tiff'), pB, width = 6.5, height = 3.13, dpi = 1200, compression = 'lzw')
ggsave(file.path(outdir, 'Fig1C_Vasoactive.png'), pC, width = 6.5, height = 3.13, dpi = 300)
ggsave(file.path(outdir, 'Fig1C_Vasoactive.tiff'), pC, width = 6.5, height = 3.13, dpi = 1200, compression = 'lzw')
cat('Figure 1 saved.\n')

# ---- Figure 2: mortality by age group ----
age_data <- d %>%
  mutate(age_grp = case_when(
    age_years < 28 / 365 ~ 'Neonate (<28d)',
    age_years < 1 ~ 'Infant (28d-1yr)',
    age_years < 5 ~ 'Child (1-5yr)',
    TRUE ~ 'Child (>=5yr)')) %>%
  group_by(age_grp) %>%
  summarise(n = n(), deaths = sum(death, na.rm = TRUE), .groups = 'drop') %>%
  mutate(mort = deaths / n,
         age_grp = factor(age_grp, levels = c('Neonate (<28d)', 'Infant (28d-1yr)',
                                              'Child (1-5yr)', 'Child (>=5yr)')))
ci2 <- exact_ci(age_data$deaths, age_data$n)
age_data$lo <- ci2[, 1]; age_data$hi <- ci2[, 2]

fig2 <- age_data %>%
  ggplot(aes(x = age_grp, y = mort, fill = age_grp)) +
  geom_bar(stat = 'identity', width = 0.62, alpha = 0.9) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, color = 'grey25', linewidth = 0.5) +
  geom_text(aes(label = sprintf('%.1f%%\n(n=%d)', 100 * mort, n)), vjust = -0.4, size = 3.6, lineheight = 0.9) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0, 0.12, 0),
                     limits = c(0, 0.20)) +
  scale_fill_manual(values = c('#d73027', '#fc8d59', '#fee090', '#91bfdb'), guide = 'none') +
  labs(x = 'Age group', y = 'In-hospital mortality') +
  theme_classic(base_size = 11) +
  theme(axis.title = element_text(size = 11))

ggsave(file.path(outdir, 'Fig2_Mortality_by_Age.png'), fig2, width = 6.5, height = 3.9, dpi = 300)
ggsave(file.path(outdir, 'Fig2_Mortality_by_Age.tiff'), fig2, width = 6.5, height = 3.9, dpi = 1200, compression = 'lzw')
cat('Figure 2 saved.\n')

# ---- Figure S1: naive vs hierarchical ----
dcsv <- read.csv('F:/openclaw_workspace/dataset/PIC_cardiac_improved.csv', stringsAsFactors = FALSE)
if (!'orig_class' %in% names(dcsv)) {
  dcsv <- dcsv %>% mutate(orig_class = case_when(
    grepl('VSD', all_names) & !grepl('ASD|PDA', all_names) ~ 'VSD Repair alone',
    grepl('ASD', all_names) & !grepl('VSD|PDA', all_names) ~ 'ASD Repair alone',
    grepl('VSD', all_names) & grepl('ASD', all_names) ~ 'VSD+ASD Repair',
    grepl('PDA', all_names) ~ 'PDA Closure alone',
    grepl('tetralogy|TOF', all_names, ignore.case = TRUE) ~ 'TOF Repair',
    grepl('pulmonary stenosis', all_names, ignore.case = TRUE) ~ 'Pulm Valve Intervention',
    TRUE ~ 'Other Cardiac'))
}
naive_stats <- dcsv %>% group_by(orig_class) %>%
  summarise(n = n(), deaths = sum(death, na.rm = TRUE), .groups = 'drop') %>%
  mutate(mort = deaths / n)

correct_stats <- dcsv %>% group_by(surg_group_main) %>%
  summarise(n = n(), deaths = sum(death, na.rm = TRUE), .groups = 'drop') %>%
  mutate(mort = deaths / n)

s1 <- rbind(
  data.frame(Procedure = 'VSD Repair', Method = 'Naive', naive_stats[match('VSD Repair alone', naive_stats$orig_class), c('mort')]),
  data.frame(Procedure = 'VSD Repair', Method = 'Hierarchical', correct_stats[match('VSD Repair alone', correct_stats$surg_group_main), c('mort')]),
  data.frame(Procedure = 'PDA Closure', Method = 'Naive', naive_stats[match('PDA Closure alone', naive_stats$orig_class), c('mort')]),
  data.frame(Procedure = 'PDA Closure', Method = 'Hierarchical', correct_stats[match('PDA Closure alone', correct_stats$surg_group_main), c('mort')]),
  data.frame(Procedure = 'VSD+ASD Repair', Method = 'Naive', naive_stats[match('VSD+ASD Repair', naive_stats$orig_class), c('mort')]),
  data.frame(Procedure = 'VSD+ASD Repair', Method = 'Hierarchical', correct_stats[match('VSD+ASD Repair', correct_stats$surg_group_main), c('mort')]),
  data.frame(Procedure = 'TOF Repair', Method = 'Naive', naive_stats[match('TOF Repair', naive_stats$orig_class), c('mort')]),
  data.frame(Procedure = 'TOF Repair', Method = 'Hierarchical', correct_stats[match('TOF Repair', correct_stats$surg_group_main), c('mort')])
)
names(s1)[3] <- 'mort'
s1$Procedure <- factor(s1$Procedure, levels = c('VSD Repair', 'PDA Closure', 'VSD+ASD Repair', 'TOF Repair'))
s1$Method <- factor(s1$Method, levels = c('Naive', 'Hierarchical'))

figS1 <- s1 %>%
  ggplot(aes(x = Procedure, y = 100 * mort, fill = Method)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.7), width = 0.62, alpha = 0.9) +
  geom_text(aes(label = sprintf('%.1f%%', 100 * mort)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c('Naive' = '#BDBDBD', 'Hierarchical' = '#2C7BB6')) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0), limits = c(0, 5.2)) +
  labs(x = '', y = 'In-hospital mortality (%)', fill = 'Classification') +
  theme_classic(base_size = 11) +
  theme(legend.position = 'bottom', axis.title = element_text(size = 11))

ggsave(file.path(outdir, 'FigS1_Naive_vs_Corrected.png'), figS1, width = 6.5, height = 3.9, dpi = 300)
ggsave(file.path(outdir, 'FigS1_Naive_vs_Corrected.tiff'), figS1, width = 6.5, height = 3.9, dpi = 1200, compression = 'lzw')
cat('Figure S1 saved.\n')

cat('\nAll figures regenerated.\n')
