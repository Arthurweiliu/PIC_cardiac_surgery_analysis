.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
suppressPackageStartupMessages({
  library(RPostgres)
  library(dplyr)
})

con <- dbConnect(RPostgres::Postgres(),
  host='localhost', port=5432,
  dbname='pic', user='postgres', password='your_password')

surgeries <- dbGetQuery(con, "
  SELECT si.subject_id, si.hadm_id, si.surgery_name, si.anes_start_time
  FROM surgery_info si
  WHERE (si.surgery_name ILIKE '%VSD%' OR si.surgery_name ILIKE '%ASD%' 
         OR si.surgery_name ILIKE '%PDA%' OR si.surgery_name ILIKE '%tetralogy%'
         OR si.surgery_name ILIKE '%TOF%' OR si.surgery_name ILIKE '%PFO%'
         OR si.surgery_name ILIKE '%pulmonary%stenosis%')
  ORDER BY si.subject_id, si.hadm_id, si.anes_start_time
")

mortality <- dbGetQuery(con, "
  SELECT a.subject_id, a.hadm_id, a.hospital_expire_flag
  FROM admissions a
")

# Patient-level: original classification (first row per patient)
orig_patient <- surgeries %>%
  group_by(subject_id, hadm_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    orig_class = case_when(
      grepl('VSD', surgery_name) & !grepl('ASD|PDA', surgery_name) ~ 'VSD Repair alone',
      grepl('ASD', surgery_name) & !grepl('VSD|PDA', surgery_name) ~ 'ASD Repair alone',
      grepl('VSD', surgery_name) & grepl('ASD', surgery_name) ~ 'VSD+ASD Repair',
      grepl('PDA', surgery_name) ~ 'PDA Closure alone',
      grepl('tetralogy|TOF', surgery_name, ignore.case = TRUE) ~ 'TOF Repair',
      grepl('pulmonary stenosis', surgery_name, ignore.case = TRUE) ~ 'Pulm Valve Intervention',
      TRUE ~ 'Other Cardiac'
    )
  )

# Patient-level: improved classification (analyze ALL procedure components across all rows)
improved_patient <- surgeries %>%
  group_by(subject_id, hadm_id) %>%
  summarise(
    surgery_names = paste(unique(surgery_name), collapse = ' | '),
    ever_VSD = any(grepl('\\bVSD\\b', surgery_name, ignore.case = TRUE)),
    ever_ASD = any(grepl('\\bASD\\b', surgery_name, ignore.case = TRUE)),
    ever_PDA = any(grepl('\\bPDA\\b', surgery_name, ignore.case = TRUE)),
    ever_TOF = any(grepl('tetralogy|\\bTOF\\b', surgery_name, ignore.case = TRUE)),
    ever_PFO = any(grepl('\\bPFO\\b', surgery_name, ignore.case = TRUE)),
    ever_CoA = any(grepl('CoA|coarctation|interruption|IAA|aortic arch|Norwood', surgery_name, ignore.case = TRUE)),
    ever_TAPVC = any(grepl('total anomalous|TAPVC|TAPVR|PAPVC', surgery_name, ignore.case = TRUE)),
    ever_switch = any(grepl('arterial switch', surgery_name, ignore.case = TRUE)),
    ever_valve = any(grepl('valvulo|valve', surgery_name, ignore.case = TRUE)),
    ever_RVOT = any(grepl('right ventricular outflow', surgery_name, ignore.case = TRUE)),
    ever_DORV = any(grepl('DORV|double outlet', surgery_name, ignore.case = TRUE)),
    ever_AVSD = any(grepl('atrioventricular septal|common atrioventricular canal|AVSD|AV canal', surgery_name, ignore.case = TRUE)),
    ever_catheter = any(grepl('catheterization|angiograph', surgery_name, ignore.case = TRUE)),
    n_records = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    # Classification hierarchy (from most complex downward)
    improved_group = case_when(
      ever_TOF ~ 'TOF Repair',
      ever_switch ~ 'Arterial Switch',
      ever_TAPVC ~ 'TAPVC Repair',
      ever_AVSD ~ 'AVSD Repair',
      ever_DORV ~ 'DORV Repair',
      ever_CoA ~ 'CoA/IAA Repair',
      ever_VSD & ever_ASD & ever_PDA ~ 'VSD+ASD+PDA',
      ever_VSD & ever_PDA & ever_valve ~ 'VSD+PDA+Valve',
      ever_ASD & ever_PDA & ever_valve ~ 'ASD+PDA+Valve',
      ever_VSD & ever_PDA & ever_RVOT ~ 'VSD+PDA+RVOT',
      ever_VSD & ever_PDA & !ever_ASD ~ 'VSD+PDA',
      ever_ASD & ever_PDA & ever_valve & ever_RVOT ~ 'ASD+PDA+Valve/RVOT',
      ever_ASD & ever_PDA & !ever_VSD ~ 'ASD+PDA',
      ever_PDA & ever_RVOT & !ever_VSD & !ever_ASD ~ 'PDA+RVOT',
      ever_PDA & ever_valve & !ever_VSD & !ever_ASD ~ 'PDA+Valve',
      ever_PDA & ever_CoA & !ever_VSD & !ever_ASD ~ 'PDA+CoA',
      ever_PDA & !ever_VSD & !ever_ASD & !ever_TOF & !ever_CoA ~ 'PDA alone (true)',
      ever_VSD & ever_ASD & ever_valve ~ 'VSD+ASD+Valve',
      ever_VSD & ever_ASD & ever_RVOT ~ 'VSD+ASD+RVOT',
      ever_VSD & ever_ASD & !ever_PDA ~ 'VSD+ASD Repair',
      ever_VSD & ever_valve ~ 'VSD+Valve',
      ever_VSD & ever_RVOT ~ 'VSD+RVOT',
      ever_VSD & !ever_PDA & !ever_ASD ~ 'VSD Repair alone',
      ever_ASD & ever_valve ~ 'ASD+Valve',
      ever_ASD & ever_RVOT ~ 'ASD+RVOT',
      ever_ASD & !ever_PDA & !ever_VSD ~ 'ASD Repair alone',
      ever_PFO ~ 'PFO Closure',
      grepl('pulmonary stenosis', surgery_names, ignore.case = TRUE) ~ 'Pulm Valve Intervention',
      ever_catheter ~ 'Diagnostic',
      TRUE ~ 'Other'
    )
  )

# === COMPARISON: Original vs Improved at patient level ===
comparison <- improved_patient %>%
  left_join(orig_patient %>% select(subject_id, hadm_id, orig_class),
            by = c('subject_id', 'hadm_id'))

# Find ALL patients where original != improved
mismatch <- comparison %>% filter(orig_class != improved_group)
cat(sprintf('Total patients: %d\n', nrow(comparison)))
cat(sprintf('Mismatched classification: %d (%.1f%%)\n\n', nrow(mismatch),
            100 * nrow(mismatch) / nrow(comparison)))

cat('======= MISMATCH TYPES =======\n')
mismatch_summary <- mismatch %>%
  group_by(orig_class, improved_group) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(desc(n))
print(mismatch_summary, n = 50)

# === ORIGINAL CLASSIFICATION MORTALITY ===
cat('\n\n======= ORIGINAL CLASSIFICATION: Mortality by group =======\n')
orig_full <- orig_patient %>%
  left_join(mortality, by = c('subject_id', 'hadm_id')) %>%
  group_by(orig_class) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality_pct = round(100 * mean(hospital_expire_flag), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(n))
print(orig_full, n = 20)

# === IMPROVED CLASSIFICATION MORTALITY ===
cat('\n\n======= IMPROVED CLASSIFICATION: Mortality by group =======\n')
improved_full <- improved_patient %>%
  left_join(mortality, by = c('subject_id', 'hadm_id')) %>%
  group_by(improved_group) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality_pct = round(100 * mean(hospital_expire_flag), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(n))
print(improved_full, n = 30)

# === FOCUS: All misclassifications INTO and OUT OF each group ===
cat('\n\n======= CROSS-TABULATION: orig vs improved =======\n')
cross <- comparison %>%
  left_join(mortality, by = c('subject_id', 'hadm_id')) %>%
  group_by(orig_class, improved_group) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality_pct = round(100 * mean(hospital_expire_flag), 1),
    .groups = 'drop'
  ) %>%
  arrange(orig_class, desc(n))
print(cross, n = 60)

dbDisconnect(con)

