source("01_data.R")

# ALS vs CTR
data_V0_ALS_CTR = cbind(score_V0_ALS_CTR,status = status_V0_ALS_CTR)
data_V1_ALS_CTR = cbind(score_V1_ALS_CTR, status = status_V1_ALS_CTR)
# data_V0$status = as.numeric(ifelse(data_V0$status=="ALS",1,0))
# data_V1$status = as.numeric(ifelse(data_V1$status=="ALS",1,0))
data_V0_ALS_CTR$status = factor(data_V0_ALS_CTR$status, levels = c("CTR","ALS"))
data_V0_ALS_CTR$status = relevel(data_V0_ALS_CTR$status, ref = "CTR")
data_V1_ALS_CTR$status = factor(data_V1_ALS_CTR$status, levels = c("CTR","ALS"))
data_V1_ALS_CTR$status = relevel(data_V1_ALS_CTR$status, ref = "CTR")

res_VO_ALS_CTR = univariate_model_new(data_V0_ALS_CTR)[[1]]
res_V1_ALS_CTR = univariate_model_new(data_V1_ALS_CTR)[[1]]

res_VO_ALS_CTR = summary_stats(res_VO_ALS_CTR)
res_V1_ALS_CTR = summary_stats(res_V1_ALS_CTR)

writexl::write_xlsx(res_VO_ALS_CTR,"results/V0_ALS_CTR.xlsx")
writexl::write_xlsx(res_V1_ALS_CTR,"results/V1_ALS_CTR.xlsx")

# ALS vs PGMC
data_V0_ALS_PGMC = cbind(score_V0_ALS_PGMC, status = status_V0_PGMC)
data_V1_ALS_PGMC = cbind(score_V1_ALS_PGMC, status = status_V1_PGMC)
data_V0_ALS_PGMC$status = factor(data_V0_ALS_PGMC$status, levels = c("PGMC","ALS"))
data_V0_ALS_PGMC$status = relevel(data_V0_ALS_PGMC$status, ref = "PGMC")
data_V1_ALS_PGMC$status = factor(data_V1_ALS_PGMC$status, levels = c("PGMC","ALS"))
data_V1_ALS_PGMC$status = relevel(data_V1_ALS_PGMC$status, ref = "PGMC")

res_VO_ALS_PGMC = univariate_model_new(data_V0_ALS_PGMC)[[1]]
res_V1_ALS_PGMC = univariate_model_new(data_V1_ALS_PGMC)[[1]]

res_VO_ALS_PGMC = summary_stats(res_VO_ALS_PGMC)
res_V1_ALS_PGMC = summary_stats(res_V1_ALS_PGMC)

writexl::write_xlsx(res_VO_ALS_PGMC,"results/V0_ALS_PGMC.xlsx")
writexl::write_xlsx(res_V1_ALS_PGMC,"results/V1_ALS_PGMC.xlsx")

# CTR vs PGMC
data_V0_CTR_PGMC = cbind(score_V0_CTR_PGMC, status = status_V0_CTR_PGMC)
data_V1_CTR_PGMC = cbind(score_V1_CTR_PGMC, status = status_V1_CTR_PGMC)
data_V0_CTR_PGMC$status = factor(data_V0_CTR_PGMC$status, levels = c("PGMC","CTR"))
data_V0_CTR_PGMC$status = relevel(data_V0_CTR_PGMC$status, ref = "PGMC")
data_V1_CTR_PGMC$status = factor(data_V1_CTR_PGMC$status, levels = c("PGMC","CTR"))
data_V1_CTR_PGMC$status = relevel(data_V1_CTR_PGMC$status, ref = "PGMC")

res_VO_CTR_PGMC = univariate_model_new(data_V0_CTR_PGMC)[[1]]
res_V1_CTR_PGMC = univariate_model_new(data_V1_CTR_PGMC)[[1]]

res_VO_CTR_PGMC = summary_stats(res_VO_CTR_PGMC)
res_V1_CTR_PGMC = summary_stats(res_V1_CTR_PGMC)

writexl::write_xlsx(res_VO_CTR_PGMC,"results/V0_CTR_PGMC.xlsx")
writexl::write_xlsx(res_V1_CTR_PGMC,"results/V1_CTR_PGMC.xlsx")

# PGMC (other) vs PGMC (C9orf72)
# -> V0
data_V0_PGMC_other_C9orf72 = cbind(score_data_V0_PGMC_mut_other_C9orf72, status = status_V0_PGMC_other_C9orf72)
data_V0_PGMC_other_C9orf72$status = factor(data_V0_PGMC_other_C9orf72$status, levels = c("PGMC_other","PGMC_C9orf72"))
data_V0_PGMC_other_C9orf72$status = relevel(data_V0_PGMC_other_C9orf72$status, ref = "PGMC_C9orf72")

res_V0_PGMC_other_C9orf72 = univariate_model_new(data_V0_PGMC_other_C9orf72)[[1]]
res_V0_PGMC_other_C9orf72 = summary_stats(res_V0_PGMC_other_C9orf72)

writexl::write_xlsx(res_V0_PGMC_other_C9orf72,"results/V0_PGMC_other_C9orf72.xlsx")

# -> V1
data_V1_PGMC_other_C9orf72 = cbind(score_data_V1_PGMC_mut_other_C9orf72, status = status_V1_PGMC_other_C9orf72)
data_V1_PGMC_other_C9orf72$status = factor(data_V1_PGMC_other_C9orf72$status, levels = c("PGMC_other","PGMC_C9orf72"))
data_V1_PGMC_other_C9orf72$status = relevel(data_V1_PGMC_other_C9orf72$status, ref = "PGMC_C9orf72")

res_V1_PGMC_other_C9orf72 = univariate_model_new(data_V1_PGMC_other_C9orf72)[[1]]
res_V1_PGMC_other_C9orf72 = summary_stats(res_V1_PGMC_other_C9orf72)

writexl::write_xlsx(res_V1_PGMC_other_C9orf72,"results/V1_PGMC_other_C9orf72.xlsx")

# PGMC (other) vs PGMC (SOD1)
# -> V0
data_V0_PGMC_other_SOD1 = cbind(score_data_V0_PGMC_mut_other_SOD1, status = status_V0_PGMC_other_SOD1)
data_V0_PGMC_other_SOD1$status = factor(data_V0_PGMC_other_SOD1$status, levels = c("PGMC_other","PGMC_SOD1"))
data_V0_PGMC_other_SOD1$status = relevel(data_V0_PGMC_other_SOD1$status, ref = "PGMC_SOD1")

res_V0_PGMC_other_SOD1 = univariate_model_new(data_V0_PGMC_other_SOD1)[[1]]
res_V0_PGMC_other_SOD1 = summary_stats(res_V0_PGMC_other_SOD1)

writexl::write_xlsx(res_V0_PGMC_other_SOD1,"results/V0_PGMC_other_SOD1.xlsx")

# -> V1
data_V1_PGMC_other_SOD1 = cbind(score_data_V1_PGMC_mut_other_SOD1, status = status_V1_PGMC_other_SOD1)
data_V1_PGMC_other_SOD1$status = factor(data_V1_PGMC_other_SOD1$status, levels = c("PGMC_other","PGMC_SOD1"))
data_V1_PGMC_other_SOD1$status = relevel(data_V1_PGMC_other_SOD1$status, ref = "PGMC_SOD1")

res_V1_PGMC_other_SOD1 = univariate_model_new(data_V1_PGMC_other_SOD1)[[1]]
res_V1_PGMC_other_SOD1 = summary_stats(res_V1_PGMC_other_SOD1)

writexl::write_xlsx(res_V1_PGMC_other_SOD1,"results/V1_PGMC_other_SOD1.xlsx")

# PGMC (other) vs PGMC (TARDBP)
# -> V0
data_V0_PGMC_other_TARDBP = cbind(score_data_V0_PGMC_mut_other_TARDBP, status = status_V0_PGMC_other_TARDBP)
data_V0_PGMC_other_TARDBP$status = factor(data_V0_PGMC_other_TARDBP$status, levels = c("PGMC_other","PGMC_TARDBP"))
data_V0_PGMC_other_TARDBP$status = relevel(data_V0_PGMC_other_TARDBP$status, ref = "PGMC_TARDBP")

res_V0_PGMC_other_TARDBP = univariate_model_new(data_V0_PGMC_other_TARDBP)[[1]]
res_V0_PGMC_other_TARDBP = summary_stats(res_V0_PGMC_other_TARDBP)

writexl::write_xlsx(res_V0_PGMC_other_TARDBP,"results/V0_PGMC_other_TARDBP.xlsx")

# -> V1
data_V1_PGMC_other_TARDBP = cbind(score_data_V1_PGMC_mut_other_TARDBP, status = status_V1_PGMC_other_TARDBP)
data_V1_PGMC_other_TARDBP$status = factor(data_V1_PGMC_other_TARDBP$status, levels = c("PGMC_other","PGMC_TARDBP"))
data_V1_PGMC_other_TARDBP$status = relevel(data_V1_PGMC_other_TARDBP$status, ref = "PGMC_TARDBP")

res_V1_PGMC_other_TARDBP = univariate_model_new(data_V1_PGMC_other_TARDBP)[[1]]
res_V1_PGMC_other_TARDBP = summary_stats(res_V1_PGMC_other_TARDBP)

writexl::write_xlsx(res_V1_PGMC_other_TARDBP,"results/V1_PGMC_other_TARDBP.xlsx")

# mean and standard deviation
t.test(smell_data_V0_ALS$score,smell_data_V0_CTR$score)
wilcox.test(smell_data_V1_ALS$score,smell_data_V1_CTR$score)
sd(smell_data_V0_ALS$score)
sd(smell_data_V0_CTR$score)

# ------------------------------------------------------------------------------
# auxiliar functions

univariate_model_new <- function(data_final){
  res = list()
  ftr = list()
  eff_CI = list()
  for (i in colnames(data_final)){
    if (i %in% c("status")){
      next
    }
    if (nlevels(as.factor(data_final[,i])) < 2){
      next
    }
    form = as.formula(paste("status ~",i))
    mod = glm(form, data_final, family = "binomial")
    tmp = summary(mod)$coefficients
    rn = rownames(tmp)
    rn = rn[rn != "(Intercept)"]
    rn = sub(i,"",rn,ignore.case = T)
    tmp = tmp[rownames(tmp) != "(Intercept)",, drop = F] %>% as.data.frame()
    rownames(tmp) = rn
    eff = vector("numeric", length = length(rn))
    eff_CI_tmp = exp(confint.default(mod))
    eff_CI_tmp <- eff_CI_tmp[rownames(eff_CI_tmp) != "(Intercept)",, drop = F] %>% as.data.frame()
    meandiff = mean(data_final[,i][data_final[,"status"] == "ALS"], na.rm = T) - mean(data_final[,i][data_final[,"status"] == "CTR"], na.rm = T)
    eff = meandiff
    eff = exp(coef(mod))
    eff = log(eff[names(eff) != "(Intercept)"])
    tmp$eff = eff
    res[[i]] = tmp
    ftr[[i]] = rn
    eff_CI[[i]] = eff_CI_tmp
  }
  df = do.call("rbind", res) %>% as.data.frame() %>% rownames_to_column("Variables")
  df$Features = do.call("c", ftr)
  df$t_stat = df$Estimate/df$`Std. Error`
  df$fdr = p.adjust(df$`Pr(>|z|)`, "fdr")
  eff_CI_new = do.call("rbind",eff_CI) %>% as.data.frame() %>% rownames_to_column("Variables") 
  colnames(eff_CI_new) <- c("Variables","2.5 %","97.5 %")
  df <- df %>%
    left_join(eff_CI_new)
  return(list(df,eff_CI_new))
}

summary_stats = function(data){
  data = data %>%
    mutate(fdr = p.adjust(`Pr(>|z|)`, "fdr"),
           lower = log(`2.5 %`),
           upper = log(`97.5 %`),
           log10_pval = -log10(`Pr(>|z|)`),
           log10_padj = -log10(p.adjust(`Pr(>|z|)`, "fdr")))
  return(data)
}