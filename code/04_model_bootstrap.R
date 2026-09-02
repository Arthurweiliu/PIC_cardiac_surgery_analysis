.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
suppressPackageStartupMessages({
  library(dplyr); library(logistf)
})

# ============================================================
# Bootstrap internal validation - FINAL MODEL
# Manuscript Table 2: primary model (age categorical + procedure group)
# Analytic sample: 1,910 patients, 9 deaths (High-Risk Complex, CoA/IAA,
# valve/RVOT and Other excluded due to collinearity with neonatal age
# and limited event counts).
# Seed fixed at 20260901 for reproducibility (200 resamples).
# ============================================================

d <- readRDS('dataset/PIC_cardiac_final.rds')

# --- Final model specification (matches manuscript Table 2 primary model) ---
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
      TRUE ~ 'EXCLUDED'  # High-Risk Complex, CoA/IAA, Valve, RVOT, Other
    )) %>%
  filter(model_group != 'EXCLUDED') %>%
  mutate(model_group = factor(model_group),
         model_group = relevel(model_group, ref = 'VSD alone'))

cat(sprintf('Final model analytic sample: n = %d, deaths = %d\n', nrow(d_model), sum(d_model$death)))
print(table(d_model$model_group, d_model$death))

# --- Point estimates (Firth) ---
fit <- logistf(death ~ age_cat + model_group, data = d_model)
vars <- names(coef(fit))[-1]
cat('\n=== Firth point estimates ===\n')
pt <- data.frame(
  Variable = vars,
  OR = sprintf('%.2f', exp(coef(fit))[-1]),
  CI = sprintf('%.2f-%.2f', exp(fit$ci.lower)[-1], exp(fit$ci.upper)[-1]),
  p = sprintf('%.3f', fit$prob[-1]),
  stringsAsFactors = FALSE
)
print(pt, row.names = FALSE)

# --- Bootstrap: 200 resamples, FIXED SEED ---
set.seed(20260901)
B <- 200
boot_or <- matrix(NA, nrow = B, ncol = length(vars))
colnames(boot_or) <- vars

for (b in 1:B) {
  idx <- sample(nrow(d_model), replace = TRUE)
  tryCatch({
    bf <- logistf(death ~ age_cat + model_group, data = d_model[idx, ],
                  control = logistf.control(maxit = 50))
    boot_or[b, ] <- exp(coef(bf))[-1]
  }, error = function(e) {})
  if (b %% 50 == 0) cat(sprintf('  Bootstrap %d/%d\n', b, B))
}

n_ok <- sum(apply(boot_or, 1, function(r) !any(is.na(r))))
cat(sprintf('\nBootstrap: %d/%d resamples converged\n', n_ok, B))

cat('\n=== Bootstrap results (median + 95% percentile CI) ===\n')
res <- data.frame(
  Variable = vars,
  Boot_OR_median = sprintf('%.2f', apply(boot_or, 2, median, na.rm = TRUE)),
  Boot_CI_2.5 = sprintf('%.2f', apply(boot_or, 2, quantile, 0.025, na.rm = TRUE)),
  Boot_CI_97.5 = sprintf('%.2f', apply(boot_or, 2, quantile, 0.975, na.rm = TRUE)),
  stringsAsFactors = FALSE
)
print(res, row.names = FALSE)

# --- Save for manuscript Table 2 ---
saveRDS(list(fit = fit, bootstrap = boot_or, seed = 20260901, n = nrow(d_model),
             B = B, vars = vars),
        'figures/firth_model_final_boot.rds')
cat('\nSaved: figures/firth_model_final_boot.rds\n')
# NOTE: res rows follow coefficient order: Infant, Neonatal, ASD, OtherCombined, PDA, TOF
cat('\nManuscript Table 2 Bootstrap OR column (update to):\n')
cat('  Infant:', res$Boot_OR_median[1], '| Neonatal:', res$Boot_OR_median[2],
    '| ASD:', res$Boot_OR_median[3], '| OtherCombined:', res$Boot_OR_median[4],
    '| PDA:', res$Boot_OR_median[5], '| TOF:', res$Boot_OR_median[6], '\n')
