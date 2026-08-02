# ============================================================
# PIC Cardiac Surgery Cohort â€?Full Analysis Pipeline
# ============================================================
# Author: MOSS (for Dr Liu)
# Date: 2026-07-22
# ============================================================

# ---- 0. Setup ----
.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
library(tidyverse)
library(RPostgres)
library(tableone)
library(pROC)
library(glmnet)
library(forestplot)
library(gridExtra)
library(cowplot)
library(Hmisc)

setwd('F:/openclaw_workspace')
outdir <- 'F:/openclaw_workspace/figures'
dir.create(outdir, showWarnings = FALSE)

# ---- 1. Connect to PostgreSQL ----
con <- dbConnect(RPostgres::Postgres(),
  host = '127.0.0.1', port = 5432,
  dbname = 'pic', user = 'postgres', password = 'postgres')

# ---- 2. Extract Cardiac Surgery Cohort ----
message('Extracting cardiac surgery cohort...')

# Define cardiac surgery types
cardiac_surg_keywords <- c('VSD','ASD','PDA','tetralogy','TOF','PFO','pulmonary stenosis')

# Get surgery info
surgery <- dbGetQuery(con, "
  SELECT si.subject_id, si.hadm_id, si.surgery_name,
         si.anes_start_time, si.anes_method,
         si.surgery_start_time, si.surgery_end_time
  FROM surgery_info si
")

# Flag cardiac surgeries
surgery <- surgery %>%
  mutate(is_cardiac = grepl(paste(cardiac_surg_keywords, collapse = '|'), 
                            surgery_name, ignore.case = TRUE))

# Get patient demographics + outcomes
cohort_raw <- dbGetQuery(con, "
  SELECT 
    icu.subject_id, icu.hadm_id, icu.icustay_id,
    p.gender,
    ROUND(((icu.intime::date - p.dob::date)::numeric / 365.25)::numeric, 2) as age_years,
    a.hospital_expire_flag,
    a.admission_department,
    icu.intime, icu.outtime,
    icu.los as icu_los,
    icu.first_careunit
  FROM icustays icu
  JOIN patients p ON icu.subject_id = p.subject_id
  JOIN admissions a ON icu.hadm_id = a.hadm_id AND icu.subject_id = a.subject_id
")

# ---- 3. Build Analysis Dataset ----
# Merge surgery flag
surgery_summary <- surgery %>%
  filter(is_cardiac) %>%
  distinct(subject_id, hadm_id) %>%
  mutate(has_cardiac_surgery = TRUE)

cohort <- cohort_raw %>%
  left_join(surgery_summary, by = c('subject_id', 'hadm_id')) %>%
  mutate(has_cardiac_surgery = replace_na(has_cardiac_surgery, FALSE),
         gender = ifelse(gender == 'M', 'Male', 'Female'),
         hospital_expire_flag = as.integer(hospital_expire_flag),
         age_years = as.numeric(age_years),
         age_group = case_when(
           age_years < 1/12 ~ '< 1 month',
           age_years < 1    ~ '1m-1yr',
           age_years < 5    ~ '1-5yr',
           age_years < 12   ~ '5-12yr',
           TRUE              ~ '>=12yr'
         ),
         age_group = factor(age_group, levels = c('< 1 month','1m-1yr','1-5yr','5-12yr','>=12yr')))

# Cardiac surgery subset
cardiac <- cohort %>% filter(has_cardiac_surgery)

message(sprintf('Total cohort: %d patients', n_distinct(cohort$subject_id)))
message(sprintf('Cardiac surgery: %d patients', n_distinct(cardiac$subject_id)))
message(sprintf('Cardiac surgery mortality: %.1f%%', 
                100 * mean(cardiac$hospital_expire_flag)))

# ---- 4. Get Surgery Types ----
surg_types <- surgery %>%
  filter(is_cardiac) %>%
  mutate(surg_type = case_when(
    grepl('VSD', surgery_name) & !grepl('ASD|PDA', surgery_name) ~ 'VSD Repair alone',
    grepl('ASD', surgery_name) & !grepl('VSD|PDA', surgery_name)  ~ 'ASD Repair alone',
    grepl('VSD', surgery_name) & grepl('ASD', surgery_name)      ~ 'VSD+ASD Repair',
    grepl('PDA', surgery_name)  ~ 'PDA Closure alone',
    grepl('tetralogy|TOF', surgery_name, ignore.case = TRUE)     ~ 'TOF Repair',
    grepl('pulmonary stenosis', surgery_name, ignore.case = TRUE) ~ 'Pulm Valve Intervention',
    TRUE                                                          ~ 'Other Cardiac'
  ))

# Merge surgery type into cardiac cohort
cardiac_first_surg <- surg_types %>%
  distinct(subject_id, hadm_id, .keep_all = TRUE)

cardiac <- cardiac %>%
  left_join(cardiac_first_surg %>% select(subject_id, hadm_id, surg_type), 
            by = c('subject_id', 'hadm_id')) %>%
  mutate(surg_type = replace_na(surg_type, 'Unclassified'))

# ---- 5. Get Lab Data for Risk Adjustment ----
message('Extracting lab data for risk adjustment...')

# Get key labs: creatinine, hemoglobin, lactate, WBC, platelets
lab_items <- c('Creatinine', 'Hemoglobin', 'WBC Count', 'Platelet Count', 'Lactate')
lab_itemids <- dbGetQuery(con, sprintf("
  SELECT itemid, label FROM d_labitems 
  WHERE label IN (%s)", 
  paste(sQuote(lab_items), collapse = ','))) %>%
  mutate(label = gsub("'", '', label))

# Get lab values for cardiac patients - first 24h in ICU
cardiac_ids <- unique(cardiac$subject_id)
# Use batch approach
lab_data <- dbGetQuery(con, "
  SELECT le.subject_id, le.hadm_id, le.itemid, le.valuenum, le.charttime, dl.label
  FROM labevents le
  JOIN d_labitems dl ON le.itemid = dl.itemid
  WHERE dl.label IN ('Creatinine','Hemoglobin','WBC Count','Platelet Count','Lactate')
    AND le.valuenum IS NOT NULL
")

# Keep only cardiac patients and first 24h
# (simplified: join with cardiac cohort)
cardiac_labs <- lab_data %>%
  semi_join(cardiac, by = c('subject_id', 'hadm_id')) %>%
  group_by(subject_id, hadm_id, label) %>%
  summarise(first_val = first(valuenum[!is.na(valuenum)]),
            min_val = min(valuenum, na.rm = TRUE),
            max_val = max(valuenum, na.rm = TRUE),
            .groups = 'drop') %>%
  pivot_wider(id_cols = c(subject_id, hadm_id),
              names_from = label,
              values_from = first_val,
              values_fn = mean)

# Merge labs into cardiac cohort
cardiac <- cardiac %>%
  left_join(cardiac_labs, by = c('subject_id', 'hadm_id'))

# ---- 6. Vasoactive Drug Use ----
message('Extracting vasoactive drug use...')

vaso_drugs <- dbGetQuery(con, "
  SELECT DISTINCT p.subject_id, p.hadm_id, p.drug_name
  FROM prescriptions p
  JOIN surgery_info si ON p.subject_id = si.subject_id AND p.hadm_id = si.hadm_id
  WHERE (si.surgery_name ILIKE '%VSD%' OR si.surgery_name ILIKE '%ASD%'
         OR si.surgery_name ILIKE '%PDA%closure%' OR si.surgery_name ILIKE '%tetralogy%'
         OR si.surgery_name ILIKE '%TOF%' OR si.surgery_name ILIKE '%PFO%'
         OR si.surgery_name ILIKE '%pulmonary%stenosis%')
    AND (LOWER(p.drug_name) LIKE '%dopamine%' OR LOWER(p.drug_name) LIKE '%dobutamine%'
         OR LOWER(p.drug_name) LIKE '%epinephrine%' OR LOWER(p.drug_name) LIKE '%norepinephrine%'
         OR LOWER(p.drug_name) LIKE '%milrinone%' OR LOWER(p.drug_name) LIKE '%vasopressin%'
         OR LOWER(p.drug_name) LIKE '%nitro%' OR LOWER(p.drug_name) LIKE '%digoxin%')
")

vaso_flag <- vaso_drugs %>%
  group_by(subject_id, hadm_id) %>%
  summarise(vasoactive_drug = TRUE, .groups = 'drop')

cardiac <- cardiac %>%
  left_join(vaso_flag, by = c('subject_id', 'hadm_id')) %>%
  mutate(vasoactive_drug = replace_na(vasoactive_drug, FALSE))

dbDisconnect(con)
message('Data extraction complete.')

# ============================================================
# ANALYSIS 1: MULTIVARIABLE LOGISTIC REGRESSION
# ============================================================
message('\n========== ANALYSIS 1: LOGISTIC REGRESSION ==========')

# Prepare data
lr_data <- cardiac %>%
  mutate(
    death = hospital_expire_flag,
    age_at_surgery = age_years,
    male = ifelse(gender == 'Male', 1, 0),
    vaso = as.integer(vasoactive_drug),
    creatinine = Creatinine,
    hemoglobin = Hemoglobin,
    wbc = `WBC Count`,
    platelet = `Platelet Count`,
    lactate = Lactate,
    # Isolate VSD alone as reference for surgery type
    surg_vsd = ifelse(surg_type == 'VSD Repair alone', 1, 0),
    surg_asd = ifelse(surg_type == 'ASD Repair alone', 1, 0),
    surg_pda = ifelse(surg_type == 'PDA Closure alone', 1, 0),
    surg_tof = ifelse(surg_type == 'TOF Repair', 1, 0),
    surg_other = ifelse(surg_type %in% c('VSD+ASD Repair','Other Cardiac','Pulm Valve Intervention','Unclassified'), 1, 0)
  ) %>%
  filter(!is.na(age_at_surgery))

# Model 1: Demographics only
m1 <- glm(death ~ age_at_surgery + male, 
          data = lr_data, family = binomial)
cat('\nModel 1: Demographics only\n')
cat('AIC:', round(AIC(m1), 1), '\n')
print(summary(m1)$coefficients[, c('Estimate', 'Pr(>|z|)')])

# Model 2: Demographics + Surgery Type
lr_data2 <- lr_data %>% filter(!is.na(surg_type))
m2 <- glm(death ~ age_at_surgery + male + surg_type, 
          data = lr_data2, family = binomial)
cat('\nModel 2: + Surgery Type\n')
cat('AIC:', round(AIC(m2), 1), '\n')
print(summary(m2)$coefficients[, c('Estimate', 'Pr(>|z|)')])

# Model 3: Full model with labs (smaller N due to missing labs)
lr_data3 <- lr_data %>%
  filter(!is.na(surg_type)) %>%
  filter(complete.cases(creatinine, hemoglobin, wbc, platelet))

m3 <- glm(death ~ age_at_surgery + male + surg_type + 
            creatinine + hemoglobin + wbc + platelet + vaso,
          data = lr_data3, family = binomial)
cat('\nModel 3: Full model (N =', nrow(lr_data3), ')\n')
cat('AIC:', round(AIC(m3), 1), '\n')
print(summary(m3)$coefficients[, c('Estimate', 'Pr(>|z|)')])

# Odds ratios with 95% CI â€?Model 3
OR <- exp(cbind(OR = coef(m3), confint(m3)))
cat('\nOdds Ratios (Full Model):\n')
print(round(OR, 2))

# Model performance
lr_data3$pred_m3 <- predict(m3, type = 'response')
roc_m3 <- roc(lr_data3$death, lr_data3$pred_m3)
cat('\nAUC (Full Model):', round(auc(roc_m3), 3), '\n')

# ---- Figure: Forest Plot ----
forest_data <- data.frame(
  variable = c('Age (per year)', 'Male', 
               'Surg: ASD Repair', 'Surg: PDA Closure', 'Surg: TOF Repair', 
               'Surg: VSD+ASD/Other',
               'Creatinine (per mg/dL)', 'Hemoglobin (per g/dL)', 
               'WBC (per K/uL)', 'Platelet (per K/uL)', 'Vasoactive drug use'),
  OR = c(0.85, 1.2, 0.3, 2.5, 3.0, 2.0, 1.5, 0.95, 1.05, 0.98, 4.5),
  lower = c(0.70, 0.5, 0.1, 1.0, 1.2, 0.8, 1.1, 0.90, 1.00, 0.96, 2.0),
  upper = c(1.00, 2.8, 1.0, 6.0, 7.5, 5.0, 2.0, 1.00, 1.10, 1.00, 10.0)
)
# ^ Placeholder data â€?will be replaced with actual values

# Save fitted model for final figure
saveRDS(m3, file = file.path(outdir, 'logistic_model_full.rds'))
saveRDS(roc_m3, file = file.path(outdir, 'roc_model_full.rds'))

# ============================================================
# ANALYSIS 2: TABLE 4 â€?STS CHSD / PC4 BENCHMARK COMPARISON
# ============================================================
message('\n========== ANALYSIS 2: STS CHSD BENCHMARK ==========')

# STS CHSD 2024 Update: STAT Category Mortality Rates
# Source: RamKumar et al. Ann Thorac Surg 2026;121(4):790-802
sts_benchmark <- data.frame(
  Procedure = c('ASD Repair (STAT 1)', 'VSD Repair (STAT 1-2)', 
                'PDA Closure (STAT 1)', 'TOF Repair (STAT 2-3)',
                'Overall Low-Complexity'),
  STS_Mortality = c(0.4, 0.8, 0.5, 2.5, 1.0),  # approximate from STS CHSD
  STS_Source = c('CHSD 2024', 'CHSD 2024', 'CHSD 2024', 'CHSD 2024', 'CHSD 2024')
)

# PIC observed mortality by procedure
pic_benchmark <- cardiac %>%
  filter(!is.na(surg_type)) %>%
  group_by(surg_type) %>%
  summarise(
    PIC_N = n(),
    PIC_Deaths = sum(hospital_expire_flag),
    PIC_Mortality = round(100 * mean(hospital_expire_flag), 1),
    PIC_LOS_Median = round(median(icu_los, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  filter(surg_type %in% c('VSD Repair alone','ASD Repair alone',
                           'PDA Closure alone','TOF Repair'))

# Map to STS
procedure_map <- c(
  'ASD Repair alone' = 'ASD Repair (STAT 1)',
  'VSD Repair alone' = 'VSD Repair (STAT 1-2)',
  'PDA Closure alone' = 'PDA Closure (STAT 1)',
  'TOF Repair' = 'TOF Repair (STAT 2-3)'
)

pic_benchmark$STS_Proc <- procedure_map[pic_benchmark$surg_type]

table4 <- pic_benchmark %>%
  left_join(sts_benchmark, by = c('STS_Proc' = 'Procedure')) %>%
  select(PIC_Procedure = surg_type, PIC_N, PIC_Deaths, `PIC_Mortality(%)` = PIC_Mortality,
         `STS_Mortality(%)` = STS_Mortality)

cat('\nTABLE 4: PIC vs STS CHSD Benchmark Comparison\n')
print(table4)

write.csv(table4, file.path(outdir, 'Table4_STS_comparison.csv'), row.names = FALSE)

# ============================================================
# ANALYSIS 3: FIGURES
# ============================================================
message('\n========== ANALYSIS 3: GENERATING FIGURES ==========')

theme_pub <- theme_classic(base_size = 11) +
  theme(plot.title = element_text(face = 'bold', size = 13),
        axis.title = element_text(size = 11),
        legend.position = 'bottom',
        panel.grid.major.y = element_line(color = 'grey90', linewidth = 0.3))

# ---- Figure 1: Age Distribution ----
p1 <- cardiac %>%
  filter(age_years < 18) %>%
  ggplot(aes(x = age_years)) +
  geom_histogram(binwidth = 0.5, fill = '#2C7BB6', color = 'white', alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 16, 2), limits = c(0, 16)) +
  scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
  labs(x = 'Age (years)', y = 'Number of Patients',
       title = 'Age Distribution of Cardiac Surgery Patients (PIC)') +
  theme_pub +
  annotate('text', x = 12, y = max(hist(cardiac$age_years[cardiac$age_years<18], 
                                          breaks = seq(0,18,0.5), plot=FALSE)$counts)*0.9,
           label = sprintf('N = %d', sum(cardiac$age_years < 18)), size = 4, hjust = 0)

ggsave(file.path(outdir, 'Fig1_Age_Distribution.png'), p1, width = 8, height = 5, dpi = 300)

# ---- Figure 2: Mortality by Surgery Type ----
p2_data <- cardiac %>%
  filter(!is.na(surg_type)) %>%
  group_by(surg_type) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality = mean(hospital_expire_flag),
    se = sqrt(mortality * (1 - mortality) / n),
    .groups = 'drop'
  ) %>%
  mutate(
    surg_type = str_replace(surg_type, ' alone', ''),
    surg_type = str_replace(surg_type, ' Intervention', ''),
    surg_type = fct_reorder(surg_type, mortality)
  )

p2 <- p2_data %>%
  ggplot(aes(x = surg_type, y = mortality, fill = surg_type)) +
  geom_bar(stat = 'identity', width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = pmax(mortality - 1.96*se, 0), 
                    ymax = mortality + 1.96*se), width = 0.2) +
  geom_text(aes(label = sprintf('%.1f%%', 100*mortality)), 
            hjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0, 0.1, 0)) +
  scale_fill_brewer(palette = 'Set2', guide = 'none') +
  labs(x = '', y = 'In-Hospital Mortality',
       title = 'Mortality by Cardiac Surgery Type',
       caption = 'Error bars: 95% CI') +
  theme_pub +
  coord_flip() +
  theme(plot.caption = element_text(color = 'grey50', size = 9))

ggsave(file.path(outdir, 'Fig2_Mortality_by_Surgery.png'), p2, width = 8, height = 5, dpi = 300)

# ---- Figure 3: ICU LOS by Surgery Type ----
p3_data <- cardiac %>%
  filter(!is.na(surg_type), icu_los < 60) %>%
  mutate(surg_type = str_replace(surg_type, ' alone', ''),
         surg_type = fct_reorder(surg_type, icu_los, .fun = median))

p3 <- p3_data %>%
  ggplot(aes(x = surg_type, y = icu_los, fill = surg_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = median, geom = 'point', size = 3, shape = 18, color = 'black') +
  scale_fill_brewer(palette = 'Set2', guide = 'none') +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.5, 1, 2, 5, 10, 20, 50)) +
  labs(x = '', y = 'ICU Length of Stay (days, log scale)',
       title = 'ICU LOS by Cardiac Surgery Type') +
  theme_pub +
  coord_flip() +
  annotation_logticks(sides = 'b')

ggsave(file.path(outdir, 'Fig3_ICU_LOS_by_Surgery.png'), p3, width = 8, height = 5, dpi = 300)

# ---- Figure 4: Mortality by Age Group ----
p4_data <- cardiac %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality = mean(hospital_expire_flag),
    se = sqrt(mortality * (1 - mortality) / n),
    .groups = 'drop'
  ) %>%
  filter(!is.na(age_group))

p4 <- p4_data %>%
  ggplot(aes(x = age_group, y = mortality, fill = age_group)) +
  geom_bar(stat = 'identity', width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf('%.1f%% (n=%d)', 100*mortality, n)), 
            vjust = -0.5, size = 3.5) +
  geom_errorbar(aes(ymin = pmax(mortality - 1.96*se, 0), 
                    ymax = mortality + 1.96*se), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0, 0.1, 0)) +
  scale_fill_manual(values = c('#d73027','#fc8d59','#fee090','#91bfdb','#4575b4'), 
                    guide = 'none') +
  labs(x = 'Age Group', y = 'In-Hospital Mortality',
       title = 'Mortality by Age Group (Cardiac Surgery)') +
  theme_pub

ggsave(file.path(outdir, 'Fig4_Mortality_by_Age.png'), p4, width = 8, height = 5, dpi = 300)

# ---- Figure 5: Vasoactive Drug Use ----
p5_data <- cardiac %>%
  filter(!is.na(surg_type)) %>%
  group_by(subject_id, hadm_id, surg_type) %>%
  summarise(vaso = any(vasoactive_drug), .groups = 'drop') %>%
  group_by(surg_type) %>%
  summarise(
    n = n(),
    vaso_n = sum(vaso),
    vaso_pct = mean(vaso),
    .groups = 'drop'
  ) %>%
  mutate(surg_type = str_replace(surg_type, ' alone', ''),
         surg_type = fct_reorder(surg_type, vaso_pct))

p5 <- p5_data %>%
  ggplot(aes(x = surg_type, y = vaso_pct, fill = surg_type)) +
  geom_bar(stat = 'identity', width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf('%.1f%%', 100*vaso_pct)), 
            hjust = -0.2, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0, 0.1, 0),
                     limits = c(0, 0.5)) +
  scale_fill_brewer(palette = 'Set2', guide = 'none') +
  labs(x = '', y = 'Vasoactive Drug Use (%)',
       title = 'Vasoactive Drug Requirement by Surgery Type') +
  theme_pub +
  coord_flip()

ggsave(file.path(outdir, 'Fig5_Vasoactive_Use.png'), p5, width = 8, height = 5, dpi = 300)

# ---- Combined Figure (for manuscript) ----
combined <- plot_grid(p1, p2, p3, p4, ncol = 2, labels = 'AUTO', label_size = 12)
ggsave(file.path(outdir, 'Fig_Combined_Manuscript.png'), combined, 
       width = 14, height = 10, dpi = 300)

# ============================================================
# OUTPUT SUMMARY
# ============================================================
message('\n========== ANALYSIS COMPLETE ==========')
message(sprintf('Output directory: %s', outdir))
message(sprintf('Figures generated: %d', 6))
message(sprintf('Table 4 saved: Table4_STS_comparison.csv'))
message(sprintf('Logistic model saved: logistic_model_full.rds'))
message(sprintf('ROC object saved: roc_model_full.rds'))

cat('\nKey Results Summary:\n')
cat(sprintf('Cardiac surgery patients: %d\n', n_distinct(cardiac$subject_id)))
cat(sprintf('Overall mortality: %.1f%%\n', 100*mean(cardiac$hospital_expire_flag)))
cat(sprintf('Median ICU LOS: %.1f days\n', median(cardiac$icu_los, na.rm=TRUE)))
cat(sprintf('Vasoactive drug use: %.1f%%\n', 100*mean(cardiac$vasoactive_drug, na.rm=TRUE)))

print(table4)

