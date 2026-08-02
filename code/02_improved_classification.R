.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
suppressPackageStartupMessages({
  library(dplyr)
  library(RPostgres)
})

# Load the original cohort to preserve the same patient set
cohort <- readRDS('F:/openclaw_workspace/dataset/PIC_cardiac_surgery_cohort.rds')
cat(sprintf('Original cohort size: %d\n', nrow(cohort)))

# Connect to get surgery info for these exact patients
con <- dbConnect(RPostgres::Postgres(),
  host='localhost', port=5432,
  dbname='pic', user='postgres', password='your_password')

# Get ALL surgeries for these patients
subject_ids <- unique(cohort$subject_id)
ids_str <- paste(subject_ids, collapse = ',')

surgeries <- dbGetQuery(con, sprintf("
  SELECT subject_id, hadm_id, surgery_name
  FROM surgery_info
  WHERE subject_id IN (%s)
  ORDER BY subject_id, hadm_id
", ids_str))

cat(sprintf('Surgeries retrieved: %d\n', nrow(surgeries)))

# Apply improved classification
patient_procedures <- surgeries %>%
  group_by(subject_id, hadm_id) %>%
  summarise(
    n_surgeries = n(),
    all_names = paste(unique(surgery_name), collapse = ' | '),
    ever_VSD = any(grepl('\\bVSD\\b', surgery_name, ignore.case = TRUE)),
    ever_ASD = any(grepl('\\bASD\\b', surgery_name, ignore.case = TRUE)),
    ever_PDA = any(grepl('\\bPDA\\b', surgery_name, ignore.case = TRUE)),
    ever_TOF = any(grepl('tetralogy|\\bTOF\\b', surgery_name, ignore.case = TRUE)),
    ever_PFO = any(grepl('\\bPFO\\b', surgery_name, ignore.case = TRUE)),
    ever_pulm_sten = any(grepl('pulmonary stenosis', surgery_name, ignore.case = TRUE)),
    ever_CoA_IAA = any(grepl('CoA|coarctation|interruption|IAA|aortic arch', surgery_name, ignore.case = TRUE)),
    ever_TAPVC = any(grepl('total anomalous|TAPVC|TAPVR|PAPVC', surgery_name, ignore.case = TRUE)),
    ever_ASO = any(grepl('arterial switch', surgery_name, ignore.case = TRUE)),
    ever_Norwood = any(grepl('Norwood', surgery_name, ignore.case = TRUE)),
    ever_valve = any(grepl('valvulo|valv', surgery_name, ignore.case = TRUE)),
    ever_RVOT = any(grepl('right ventricular outflow', surgery_name, ignore.case = TRUE)),
    ever_DORV = any(grepl('DORV|double outlet', surgery_name, ignore.case = TRUE)),
    ever_AVSD = any(grepl('atrioventricular septal|common atrioventricular canal|AVSD|AV canal', surgery_name, ignore.case = TRUE)),
    ever_Glenn = any(grepl('Glenn|Bidirectional', surgery_name, ignore.case = TRUE)),
    ever_Blalock = any(grepl('Blalock|BT shunt', surgery_name, ignore.case = TRUE)),
    ever_catheter = any(grepl('catheterization|angiograph', surgery_name, ignore.case = TRUE)),
    .groups = 'drop'
  ) %>%
  mutate(
    # Hierarchical classification (most complex first)
    improved_group = case_when(
      ever_Norwood ~ 'Norwood Procedure',
      ever_ASO ~ 'Arterial Switch',
      ever_TAPVC ~ 'TAPVC Repair',
      ever_DORV ~ 'DORV Repair',
      ever_AVSD ~ 'AVSD Repair',
      ever_TOF ~ 'TOF Repair',
      ever_VSD & ever_ASD & ever_PDA ~ 'VSD+ASD+PDA',
      ever_VSD & ever_PDA & (ever_valve | ever_RVOT) ~ 'VSD+PDA+Valve/RVOT',
      ever_VSD & ever_PDA ~ 'VSD+PDA',
      ever_ASD & ever_PDA ~ 'ASD+PDA',
      ever_VSD & ever_ASD & (ever_valve | ever_RVOT) ~ 'VSD+ASD+Valve/RVOT',
      ever_VSD & ever_ASD ~ 'VSD+ASD Repair',
      ever_CoA_IAA & (ever_VSD | ever_ASD | ever_PDA) ~ 'CoA/IAA+Combined',
      ever_CoA_IAA ~ 'CoA/IAA Repair',
      ever_valve & ever_VSD ~ 'VSD+Valve',
      ever_valve & ever_ASD ~ 'ASD+Valve',
      ever_valve & ever_PDA ~ 'PDA+Valve',
      ever_valve ~ 'Valvuloplasty',
      ever_RVOT ~ 'RVOT Procedure',
      ever_VSD ~ 'VSD Repair alone',
      ever_ASD ~ 'ASD Repair alone',
      ever_PDA ~ 'PDA Closure alone',
      ever_PFO ~ 'PFO Closure',
      ever_Glenn ~ 'Glenn Procedure',
      ever_Blalock ~ 'Blalock-Taussig Shunt',
      ever_pulm_sten ~ 'Pulmonary Valve Intervention',
      ever_catheter ~ 'Diagnostic Catheterization',
      TRUE ~ 'Other Cardiac'
    ),
    
    # Simplified for main display (merge groups with <15 patients)
    surg_group_main = case_when(
      improved_group %in% c('Norwood Procedure', 'Arterial Switch', 'TAPVC Repair', 
                             'DORV Repair', 'AVSD Repair', 'CoA/IAA Repair',
                             'Glenn Procedure', 'Blalock-Taussig Shunt') ~ 'Complex Neonatal/Other',
      improved_group == 'TOF Repair' ~ 'TOF Repair',
      improved_group %in% c('VSD+ASD+PDA', 'VSD+PDA', 'VSD+PDA+Valve/RVOT',
                             'VSD+ASD+Valve/RVOT') ~ 'VSD + Combined',
      improved_group == 'ASD+PDA' ~ 'ASD+PDA',
      improved_group %in% c('VSD+ASD Repair') ~ 'VSD+ASD Repair',
      improved_group == 'CoA/IAA+Combined' ~ 'CoA/IAA+Combined',
      improved_group %in% c('VSD+Valve', 'ASD+Valve', 'PDA+Valve', 'Valvuloplasty') ~ 'Valve Procedures',
      improved_group == 'RVOT Procedure' ~ 'RVOT Procedure',
      improved_group == 'VSD Repair alone' ~ 'VSD Repair alone',
      improved_group == 'ASD Repair alone' ~ 'ASD Repair alone',
      improved_group == 'PDA Closure alone' ~ 'PDA Closure alone',
      improved_group %in% c('PFO Closure', 'Pulmonary Valve Intervention',
                             'Diagnostic Catheterization', 'Other Cardiac') ~ 'Other',
      TRUE ~ 'Other'
    ),
    
    # For model: collapse further to ensure adequate events
    surg_group_model = case_when(
      surg_group_main %in% c('Complex Neonatal/Other', 'TOF Repair', 'CoA/IAA+Combined') ~ 'High Risk',
      surg_group_main %in% c('VSD+ASD Repair', 'VSD + Combined', 'ASD+PDA') ~ 'Intermediate Risk',
      surg_group_main %in% c('Valve Procedures', 'RVOT Procedure', 'Other') ~ 'Other/Valve',
      surg_group_main == 'VSD Repair alone' ~ 'VSD alone (ref)',
      surg_group_main == 'ASD Repair alone' ~ 'ASD alone',
      surg_group_main == 'PDA Closure alone' ~ 'PDA alone',
      TRUE ~ 'Other/Valve'
    )
  )

# Merge with cohort
cohort_updated <- cohort %>%
  left_join(patient_procedures, by = c('subject_id', 'hadm_id')) %>%
  mutate(
    surg_group_main = coalesce(surg_group_main, 'Other'),
    surg_group_model = coalesce(surg_group_model, 'Other/Valve'),
    improved_group = coalesce(improved_group, 'Other Cardiac'),
    death = as.integer(hospital_expire_flag)
  )

cat(sprintf('\nUpdated cohort size: %d\n', nrow(cohort_updated)))
cat(sprintf('Deaths: %d (%.1f%%)\n', sum(cohort_updated$death), 
            100 * mean(cohort_updated$death)))

# ========== SUMMARY TABLE ==========
cat('\n\n===== MORTALITY BY IMPROVED GROUP =====\n')
t1_main <- cohort_updated %>%
  group_by(surg_group_main) %>%
  summarise(
    n = n(),
    deaths = sum(death),
    mortality_pct = round(100 * mean(death), 1),
    age_med = round(median(age_years, na.rm=TRUE), 2),
    age_q1q3 = paste(round(quantile(age_years, c(0.25,0.75), na.rm=TRUE),2), collapse='-'),
    male_pct = round(100 * mean(gender == 'Male', na.rm=TRUE), 1),
    los_med = round(median(icu_los_days, na.rm=TRUE), 1),
    los_q1q3 = paste(round(quantile(icu_los_days, c(0.25,0.75), na.rm=TRUE),1), collapse='-'),
    .groups = 'drop'
  ) %>%
  arrange(desc(n))
print(t1_main, n=30)

# Detailed reclassification
cat('\n\n===== ORIGINAL VS IMPROVED CROSS-TAB =====\n')
# Simulate original classification
cohort_updated2 <- cohort_updated %>%
  mutate(
    orig_class = case_when(
      # Simulate: take first surgery_name per patient and apply old logic
      grepl('VSD', all_names) & !grepl('ASD|PDA', all_names) ~ 'VSD Repair alone',
      grepl('ASD', all_names) & !grepl('VSD|PDA', all_names) ~ 'ASD Repair alone',
      grepl('VSD', all_names) & grepl('ASD', all_names) ~ 'VSD+ASD Repair',
      grepl('PDA', all_names) ~ 'PDA Closure alone',
      grepl('tetralogy|TOF', all_names, ignore.case = TRUE) ~ 'TOF Repair',
      grepl('pulmonary stenosis', all_names, ignore.case = TRUE) ~ 'Pulm Valve Intervention',
      TRUE ~ 'Other Cardiac'
    )
  )

cross <- cohort_updated2 %>%
  group_by(orig_class, surg_group_main) %>%
  summarise(
    n = n(),
    deaths = sum(death),
    mortality_pct = round(100 * mean(death), 1),
    .groups = 'drop'
  ) %>%
  arrange(orig_class, desc(n))
print(cross, n=50)

cat(sprintf('\nTotal patients with changed classification: %d / %d (%.1f%%)\n',
            sum(cohort_updated2$orig_class != cohort_updated2$surg_group_main),
            nrow(cohort_updated2),
            100 * mean(cohort_updated2$orig_class != cohort_updated2$surg_group_main)))

# Save updated dataset
saveRDS(cohort_updated, 'F:/openclaw_workspace/dataset/PIC_cardiac_improved.rds')
write.csv(cohort_updated2, 'F:/openclaw_workspace/dataset/PIC_cardiac_improved.csv', row.names = FALSE)
cat('\nDataset saved.\n')

dbDisconnect(con)

