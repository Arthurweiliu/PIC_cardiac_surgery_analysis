.libPaths(c('F:/openclaw_workspace/R_lib', .libPaths()))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(logistf))

d <- readRDS('dataset/PIC_cardiac_improved.rds')

d_model <- d %>%
  mutate(
    model_group = case_when(
      surg_group_main == 'Complex Neonatal/Other' ~ 'Complex Neonatal',
      surg_group_main == 'TOF Repair' ~ 'TOF Repair',
      surg_group_main == 'PDA Closure alone' ~ 'PDA alone',
      surg_group_main == 'VSD Repair alone' ~ 'VSD alone',
      surg_group_main == 'ASD Repair alone' ~ 'ASD alone',
      TRUE ~ 'Other Combined'
    ),
    male = ifelse(gender == 'Male', 1, 0)
  )

# Set reference level
d_model$model_group <- factor(d_model$model_group)
d_model$model_group <- relevel(d_model$model_group, ref = 'VSD alone')

cat(sprintf('Dataset: %d patients, %d deaths\n', nrow(d_model), sum(d_model$death)))
print(table(d_model$model_group))

# Firth regression
fit <- logistf(death ~ age_years + male + model_group, data = d_model)

# Bootstrap
set.seed(42)
B <- 200
var_names <- names(coef(fit))
boot_or <- matrix(NA, nrow = B, ncol = length(var_names))
colnames(boot_or) <- var_names

for (b in 1:B) {
  idx <- sample(nrow(d_model), replace = TRUE)
  tryCatch({
    bf <- logistf(death ~ age_years + male + model_group,
                  data = d_model[idx, ],
                  control = logistf.control(maxit = 50))
    boot_or[b, ] <- exp(coef(bf))
  }, error = function(e) {})
  if (b %% 50 == 0) cat(sprintf('  Bootstrap %d/%d\n', b, B))
}

# Results table
cat('\n\n========== FINAL MODEL RESULTS ==========\n')
res <- data.frame(
  Variable = var_names,
  OR = sprintf('%.2f', exp(coef(fit))),
  CI_Firth = sprintf('%.2f-%.2f', exp(fit$ci.lower), exp(fit$ci.upper)),
  p_Firth = sprintf('%.4f', fit$prob),
  Boot_OR = sprintf('%.2f', apply(boot_or, 2, median, na.rm = TRUE)),
  Boot_CI = sprintf('%.2f-%.2f',
    apply(boot_or, 2, quantile, 0.025, na.rm = TRUE),
    apply(boot_or, 2, quantile, 0.975, na.rm = TRUE)),
  stringsAsFactors = FALSE
)
print(res, row.names = FALSE)

cat(sprintf('\nAUC: %.3f\n', as.numeric(pROC::auc(fit$death, fitted(fit)))))

saveRDS(list(fit = fit, bootstrap = boot_or), 'figures/firth_model_improved.rds')
cat('\nModel saved.\n')
