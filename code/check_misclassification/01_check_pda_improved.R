.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
suppressPackageStartupMessages({
  library(RPostgres)
  library(dplyr)
})

con <- dbConnect(RPostgres::Postgres(),
  host='localhost', port=5432,
  dbname='pic', user='postgres', password='your_password')

# Get ALL surgeries for cardiac patients
surgeries <- dbGetQuery(con, "
  SELECT si.subject_id, si.hadm_id, si.surgery_name, si.anes_start_time
  FROM surgery_info si
  WHERE (si.surgery_name ILIKE '%VSD%' OR si.surgery_name ILIKE '%ASD%' 
         OR si.surgery_name ILIKE '%PDA%' OR si.surgery_name ILIKE '%tetralogy%'
         OR si.surgery_name ILIKE '%TOF%' OR si.surgery_name ILIKE '%PFO%'
         OR si.surgery_name ILIKE '%pulmonary%stenosis%')
  ORDER BY si.subject_id, si.hadm_id, si.anes_start_time
")

# Get mortality
mortality <- dbGetQuery(con, "
  SELECT a.subject_id, a.hadm_id, a.hospital_expire_flag
  FROM admissions a
")

# Improved surgery classification
surgery_improved <- surgeries %>%
  mutate(
    # Check what procedures are in the surgery_name
    has_VSD = grepl('\\bVSD\\b', surgery_name, ignore.case = TRUE),
    has_ASD = grepl('\\bASD\\b', surgery_name, ignore.case = TRUE),
    has_PDA = grepl('\\bPDA\\b', surgery_name, ignore.case = TRUE),
    has_TOF = grepl('tetralogy|\\bTOF\\b', surgery_name, ignore.case = TRUE),
    has_PFO = grepl('\\bPFO\\b', surgery_name, ignore.case = TRUE),
    
    # Improved classification: account for ALL procedures in surgery_name
    surg_type_improved = case_when(
      # TOF takes precedence (most complex)
      has_TOF ~ 'TOF Repair',
      # VSD+ASD combined
      has_VSD & has_ASD & !has_TOF ~ 'VSD+ASD Repair',
      # VSD+PDA â†?should NOT be 'PDA alone'
      has_VSD & has_PDA & !has_ASD & !has_TOF ~ 'VSD Repair + PDA Closure',
      # ASD+PDA â†?should NOT be 'PDA alone'
      has_ASD & has_PDA & !has_VSD & !has_TOF ~ 'ASD Repair + PDA Closure',
      # VSD alone
      has_VSD & !has_PDA & !has_ASD ~ 'VSD Repair alone',
      # ASD alone
      has_ASD & !has_PDA & !has_VSD ~ 'ASD Repair alone',
      # PDA alone (truly isolated)
      has_PDA & !has_VSD & !has_ASD & !has_TOF ~ 'PDA Closure alone (true)',
      # PFO closure
      has_PFO ~ 'PFO Closure',
      # Other
      grepl('pulmonary stenosis', surgery_name, ignore.case = TRUE) ~ 'Pulm Valve Intervention',
      TRUE ~ 'Other Cardiac'
    ),
    
    # Paper's original classification (simulated)
    surg_type_original = case_when(
      grepl('VSD', surgery_name) & !grepl('ASD|PDA', surgery_name) ~ 'VSD Repair alone',
      grepl('ASD', surgery_name) & !grepl('VSD|PDA', surgery_name) ~ 'ASD Repair alone',
      grepl('VSD', surgery_name) & grepl('ASD', surgery_name) ~ 'VSD+ASD Repair',
      grepl('PDA', surgery_name) ~ 'PDA Closure alone',
      grepl('tetralogy|TOF', surgery_name, ignore.case = TRUE) ~ 'TOF Repair',
      grepl('pulmonary stenosis', surgery_name, ignore.case = TRUE) ~ 'Pulm Valve Intervention',
      TRUE ~ 'Other Cardiac'
    )
  )

# Per patient: original method uses distinct(subject_id, hadm_id, .keep_all = TRUE) = first row
original_labels <- surgery_improved %>%
  distinct(subject_id, hadm_id, .keep_all = TRUE)

# Improved method: roll up per patient considering ALL procedures across ALL surgery rows
improved_labels <- surgery_improved %>%
  group_by(subject_id, hadm_id) %>%
  summarise(
    # Overall, what did this patient have?
    ever_VSD = any(has_VSD),
    ever_ASD = any(has_ASD),
    ever_PDA = any(has_PDA),
    ever_TOF = any(has_TOF),
    ever_PFO = any(has_PFO),
    surg_names = paste(unique(surgery_name), collapse = ' | '),
    n_surgeries = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    improved_group = case_when(
      ever_TOF & ever_PDA ~ 'TOF + PDA',
      ever_TOF ~ 'TOF Repair',
      ever_VSD & ever_ASD & ever_PDA ~ 'VSD+ASD+PDA',
      ever_VSD & ever_PDA & !ever_ASD ~ 'VSD + PDA',
      ever_ASD & ever_PDA & !ever_VSD ~ 'ASD + PDA',
      ever_PDA & !ever_VSD & !ever_ASD & !ever_TOF ~ 'PDA alone (true)',
      ever_VSD & ever_ASD & !ever_PDA ~ 'VSD+ASD Repair',
      ever_VSD & !ever_PDA & !ever_ASD ~ 'VSD Repair alone',
      ever_ASD & !ever_PDA & !ever_VSD ~ 'ASD Repair alone',
      ever_PFO ~ 'PFO Closure',
      TRUE ~ 'Other'
    )
  )

# Merge with mortality
original_with_outcome <- original_labels %>%
  left_join(mortality, by = c('subject_id', 'hadm_id')) %>%
  filter(!is.na(hospital_expire_flag))

improved_with_outcome <- improved_labels %>%
  left_join(mortality, by = c('subject_id', 'hadm_id')) %>%
  filter(!is.na(hospital_expire_flag))

# === ORIGINAL CLASSIFICATION: Mortality by group ===
cat('\n========== ORIGINAL CLASSIFICATION (Paper method) ==========\n')
orig_summary <- original_with_outcome %>%
  group_by(surg_type_original) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality_pct = round(100 * mean(hospital_expire_flag), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(n))
print(orig_summary, n = 20)

# === IMPROVED CLASSIFICATION: Mortality by group ===
cat('\n========== IMPROVED CLASSIFICATION (All procedures) ==========\n')
improved_summary <- improved_with_outcome %>%
  group_by(improved_group) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality_pct = round(100 * mean(hospital_expire_flag), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(n))
print(improved_summary, n = 20)

# === FOCUS: PDA group comparison ===
cat('\n========== PDA GROUP DEEP DIVE ==========\n')

# Original PDA group
orig_pda <- original_with_outcome %>%
  filter(surg_type_original == 'PDA Closure alone')

cat(sprintf('\n--- Original "PDA Closure alone" group ---\n'))
cat(sprintf('N = %d, Deaths = %d, Mortality = %.1f%%\n',
            nrow(orig_pda), sum(orig_pda$hospital_expire_flag),
            100 * mean(orig_pda$hospital_expire_flag)))

# How many of these are actually combined procedures?
orig_pda_improved <- orig_pda %>%
  left_join(improved_labels %>% select(subject_id, hadm_id, improved_group),
            by = c('subject_id', 'hadm_id'))

cat('\nBreakdown of original "PDA Closure alone" by improved classification:\n')
pda_breakdown <- orig_pda_improved %>%
  group_by(improved_group) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality_pct = round(100 * mean(hospital_expire_flag), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(n))
print(pda_breakdown, n = 20)

# True isolated PDA mortality
true_pda <- improved_with_outcome %>%
  filter(improved_group == 'PDA alone (true)')

cat(sprintf('\n--- TRUE isolated PDA closure ---\n'))
cat(sprintf('N = %d, Deaths = %d, Mortality = %.1f%%\n',
            nrow(true_pda), sum(true_pda$hospital_expire_flag),
            100 * mean(true_pda$hospital_expire_flag)))

# All PDA-combined groups
pda_combined_groups <- improved_with_outcome %>%
  filter(grepl('PDA', improved_group) & improved_group != 'PDA alone (true)')

cat(sprintf('\n--- Combined PDA groups (PDA + other procedure) ---\n'))
combined_summary <- pda_combined_groups %>%
  group_by(improved_group) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality_pct = round(100 * mean(hospital_expire_flag), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(n))
print(combined_summary, n = 20)

cat(sprintf('\nTotal combined PDA: N = %d, Deaths = %d, Mortality = %.1f%%\n',
            nrow(pda_combined_groups), sum(pda_combined_groups$hospital_expire_flag),
            100 * mean(pda_combined_groups$hospital_expire_flag)))

# === COMPARE WITH improved groups that include the combined procedures ===
cat('\n========== MORTALITY BY TRUE PROCEDURE (Improved) ==========\n')
improved_short <- improved_with_outcome %>%
  group_by(improved_group) %>%
  summarise(
    n = n(),
    deaths = sum(hospital_expire_flag),
    mortality_pct = round(100 * mean(hospital_expire_flag), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(n))
print(improved_short, n = 20)

dbDisconnect(con)

