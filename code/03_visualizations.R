## volcano plot
p = volcano_plot_CTR(res_VO_ALS_CTR,"p-value","Visits at V0 (p-value = 0.01)",0.01) # P-VALUE = 0.01
save_plot_pdf(p, filename = "plots/V0_p0.01_ALS_CTR.pdf")
p = volcano_plot_CTR(res_VO_ALS_CTR,"adjusted p-value","Visits at V0 (5% FDR)",0.05) # 5% FDR
save_plot_pdf(p, filename = "plots/V0_FDR0.05_ALS_CTR.pdf")

p = volcano_plot_CTR(res_V1_ALS_CTR,"p-value","Visits at V1 (p-value = 0.01)",0.01) # P-VALUE = 0.01
save_plot_pdf(p, filename = "plots/V1_p0.01_ALS_CTR.pdf")
p = volcano_plot_CTR(res_V1_ALS_CTR,"adjusted p-value","Visits at V1 (5% FDR)",0.05) # 5% FDR
save_plot_pdf(p, filename = "plots/V1_FDR0.05_ALS_CTR.pdf")

p = volcano_plot_PGMC(res_VO_ALS_PGMC,"p-value","Visits at V0 (p-value = 0.01)",0.01) # P-VALUE = 0.01
save_plot_pdf(p, filename = "plots/V0_p0.01_ALS_PGMC.pdf")
p = volcano_plot_PGMC(res_VO_ALS_PGMC,"adjusted p-value","Visits at V0 (5% FDR)",0.05) # 5% FDR
save_plot_pdf(p, filename = "plots/V0_FDR0.05_ALS_PGMC.pdf")

p = volcano_plot_PGMC(res_V1_ALS_PGMC,"p-value","Visits at V1 (p-value = 0.01)",0.01) # P-VALUE = 0.01
save_plot_pdf(p, filename = "plots/V1_p0.01_ALS_PGMC.pdf")
p = volcano_plot_PGMC(res_V1_ALS_PGMC,"adjusted p-value","Visits at V1 (5% FDR)",0.05) # 5% FDR
save_plot_pdf(p, filename = "plots/V1_FDR0.05_ALS_PGMC.pdf")

# prepare data for histograms and violin plots
score_status_V0_ALS_CTR = data.frame(score = score_V0_ALS_CTR$score,
                                     status = status_V0_ALS_CTR)
score_status_V0_ALS_CTR_reduced = score_status_V0_ALS_CTR[which(smell_data_V0_ALS_CTR$PatientID %in% smell_data_V1_ALS_CTR$PatientID),]
score_status_V1_ALS_CTR = data.frame(score = score_V1_ALS_CTR$score,
                                     status = status_V1_ALS_CTR)
score_status_V0_ALS_PGMC = data.frame(score = score_V0_ALS_PGMC$score,
                                      status = status_V0_ALS_PGMC)
score_status_V0_ALS_PGMC_reduced = score_status_V0_ALS_PGMC[which(smell_data_V0_ALS_PGMC$PatientID %in% smell_data_V1_ALS_PGMC$PatientID),]
score_status_V1_ALS_PGMC = data.frame(score = score_V1_ALS_PGMC$score,
                                      status = status_V1_ALS_PGMC)
score_status_V0_V1_ALS_CTR = rbind(score_status_V0_ALS_CTR_reduced %>%
  mutate(visit = rep("V0",34)),score_status_V1_ALS_CTR %>% mutate(visit = rep("V1",34)))
score_status_V0_V1_ALS_PGMC = rbind(score_status_V0_ALS_PGMC_reduced %>%
                                     mutate(visit = rep("V0",38)),
                                    score_status_V1_ALS_PGMC %>% mutate(visit = rep("V1",38)))
score_status_V0_CTR_PGMC = data.frame(score = score_V0_CTR_PGMC$score,
                                     status = status_V0_CTR_PGMC)
score_status_V0_CTR_PGMC_reduced = score_status_V0_CTR_PGMC[which(smell_data_V0_CTR_PGMC$PatientID %in%
                                                                    smell_data_V1_CTR_PGMC$PatientID),]
score_status_V1_CTR_PGMC = data.frame(score = score_V1_CTR_PGMC$score,
                                      status = status_V1_CTR_PGMC)
score_status_V0_V1_CTR_PGMC = rbind(score_status_V0_CTR_PGMC_reduced %>%
                                     mutate(visit = rep("V0",46)),
                                    score_status_V1_CTR_PGMC %>% mutate(visit = rep("V1",46)))
score_status_V0_PGMC_other_C9orf72 = data.frame(score = score_data_V0_PGMC_mut_other_C9orf72$score,
                                                    status = status_V0_PGMC_other_C9orf72)
score_status_V0_PGMC_other_SOD1 = data.frame(score = score_data_V0_PGMC_mut_other_SOD1$score,
                                                status = status_V0_PGMC_other_SOD1)
score_status_V0_PGMC_other_TARDBP = data.frame(score = score_data_V0_PGMC_mut_other_TARDBP$score,
                                             status = status_V0_PGMC_other_TARDBP)
score_status_V0_PGMC_mutations <- rbind(score_status_V0_PGMC_other_C9orf72,
                                        score_status_V0_PGMC_other_SOD1 %>% filter(status == "PGMC_SOD1"),
                                        score_status_V0_PGMC_other_TARDBP %>% filter(status == "PGMC_TARDBP"))
score_status_V0_PGMC_mutations$status <- factor(score_status_V0_PGMC_mutations$status,
                                                levels = c("PGMC_C9orf72","PGMC_SOD1","PGMC_TARDBP","PGMC_other"))

# data to send Laura
score_status_V0_V1_reduced <- rbind(score_status_V0_V1_ALS_CTR %>%
  mutate(PatientID = c(smell_data_V1_ALS$PatientID,
                       smell_data_V1_CTR$PatientID,
                       smell_data_V1_ALS$PatientID,
                       smell_data_V1_CTR$PatientID)),
  score_status_V0_V1_ALS_PGMC %>% 
    filter(status == "PGMC") %>%
    mutate(PatientID = c(smell_data_V1_PGMC$PatientID,smell_data_V1_PGMC$PatientID))) %>%
  arrange(visit)
score_status_V0_V1 <- rbind(rbind(score_status_V0_ALS_CTR %>%
                                    mutate(visit = rep("V0",95)),score_status_V1_ALS_CTR %>% mutate(visit = rep("V1",34))) %>%
                                      mutate(PatientID = c(smell_data_V0_ALS$PatientID,
                                                           smell_data_V0_CTR$PatientID,
                                                           smell_data_V1_ALS$PatientID,
                                                           smell_data_V1_CTR$PatientID)),
                            rbind(score_status_V0_ALS_PGMC %>%
                                    mutate(visit = rep("V0",99)),
                                  score_status_V1_ALS_PGMC %>% mutate(visit = rep("V1",38))) %>% 
                                      filter(status == "PGMC") %>%
                                      mutate(PatientID = c(smell_data_V0_PGMC$PatientID,smell_data_V1_PGMC$PatientID))) %>%
  arrange(visit)
writexl::write_xlsx(score_status_V0_V1,"results/score_status_V0_V1_all.xlsx")
writexl::write_xlsx(score_status_V0_V1_reduced,"results/score_status_V0_V1_reduced.xlsx")
  

# histograms of ALS, CTR, PGMC
p = histogram_plot(score_status_V0_ALS_CTR,"ALS and CTR score at V0",22) 
save_plot_pdf(p, filename = "plots/score_V0_ALS_CTR.pdf")
p = histogram_plot(score_status_V0_ALS_CTR %>% filter(status == "ALS"),"ALS score at V0",22) 
save_plot_pdf(p, filename = "plots/score_V0_ALS.pdf")
p = histogram_plot(score_status_V0_ALS_CTR %>% filter(status == "CTR"),"CTR score at V0",22) 
save_plot_pdf(p, filename = "plots/score_V0_CTR.pdf")
p = histogram_plot(score_status_V0_ALS_CTR_reduced,"ALS and CTR score at V0 (IDs only in V1)",10) 
save_plot_pdf(p, filename = "plots/score_V0_ALS_CTR_reduced.pdf")
p = histogram_plot(score_status_V1_ALS_CTR,"ALS and CTR score at V1",8) 
save_plot_pdf(p, filename = "plots/score_V1_ALS_CTR.pdf")
p = histogram_plot(score_status_V1_ALS_CTR %>% filter(status == "ALS"),"ALS score at V1",8) 
save_plot_pdf(p, filename = "plots/score_V1_ALS.pdf")
p = histogram_plot(score_status_V1_ALS_CTR %>% filter(status == "CTR"),"CTR score at V1",8) 
save_plot_pdf(p, filename = "plots/score_V1_CTR.pdf")
p = histogram_plot(score_status_V0_ALS_PGMC,"ALS and PGMC score at V0",22) 
save_plot_pdf(p, filename = "plots/score_V0_ALS_PGMC.pdf")
p = histogram_plot(score_status_V0_ALS_PGMC %>% filter(status == "PGMC"),"PGMC score at V0",22) 
save_plot_pdf(p, filename = "plots/score_V0_PGMC.pdf")
p = histogram_plot(score_status_V0_ALS_PGMC_reduced,"ALS and PGMC score at V0 (IDs only in V1)",8) 
save_plot_pdf(p, filename = "plots/score_V0_ALS_PGMC_reduced.pdf")
p = histogram_plot(score_status_V1_ALS_PGMC,"ALS and PGMC score at V1",8) 
save_plot_pdf(p, filename = "plots/score_V1_ALS_PGMC.pdf")
p = histogram_plot(score_status_V1_ALS_PGMC %>% filter(status == "PGMC"),"PGMC score at V1",8) 
save_plot_pdf(p, filename = "plots/score_V1_PGMC.pdf")
p = histogram_plot(score_status_V0_CTR_PGMC,"CTR and PGMC score at V0",22) 
save_plot_pdf(p, filename = "plots/score_V0_CTR_PGMC.pdf")
p = histogram_plot(score_status_V0_CTR_PGMC_reduced,"CTR and PGMC score at V0 (IDs only in V1)",10) 
save_plot_pdf(p, filename = "plots/score_V0_CTR_PGMC_reduced.pdf")
p = histogram_plot(score_status_V1_CTR_PGMC,"CTR and PGMC score at V1",8) 
save_plot_pdf(p, filename = "plots/score_V1_CTR_PGMC.pdf")

# violin plots
p = violin_plots(score_status_V0_ALS_CTR,"ALS and CTR score at V0")
save_plot_pdf(p, filename = "plots/violin_V0_ALS_CTR.pdf")
p = violin_plots(score_status_V0_ALS_CTR_reduced,"ALS and CTR score at V0 (IDs only in V1)")
save_plot_pdf(p, filename = "plots/violin_V0_ALS_CTR_reduced.pdf")
p = violin_plots(score_status_V1_ALS_CTR,"ALS and CTR score at V1")
save_plot_pdf(p, filename = "plots/violin_V1_ALS_CTR.pdf")
p = violin_plots(score_status_V0_ALS_PGMC,"ALS and PGMC score at V0")
save_plot_pdf(p, filename = "plots/violin_V0_ALS_PGMC.pdf")
p = violin_plots(score_status_V0_ALS_PGMC_reduced,"ALS and PGMC score at V0 (IDs only in V1)")
save_plot_pdf(p, filename = "plots/violin_V0_ALS_PGMC_reduced.pdf")
p = violin_plots(score_status_V1_ALS_PGMC,"ALS and PGMC score at V1")
save_plot_pdf(p, filename = "plots/violin_V1_ALS_PGMC.pdf")
p = violin_plots(score_status_V0_CTR_PGMC,"CTR and PGMC score at V0")
save_plot_pdf(p, filename = "plots/violin_V0_CTR_PGMC.pdf")
p = violin_plots(score_status_V0_CTR_PGMC_reduced,"CTR and PGMC score at V0 (IDs only in V1)")
save_plot_pdf(p, filename = "plots/violin_V0_CTR_PGMC_reduced.pdf")
p = violin_plots(score_status_V1_CTR_PGMC,"CTR and PGMC score at V1")
save_plot_pdf(p, filename = "plots/violin_V1_CTR_PGMC.pdf")
p = violin_plots(score_status_V0_PGMC_other_C9orf72,"PGMC (other mutation) vs PGMC (C9orf72) at V0")
save_plot_pdf(p, filename = "plots/violin_V0_PGMC_other_vs_C9orf72.pdf")
p = violin_plots(score_status_V0_PGMC_other_SOD1,"PGMC (other mutation) vs PGMC (SOD1) at V0")
save_plot_pdf(p, filename = "plots/violin_V0_PGMC_other_vs_SOD1.pdf")
p = violin_plots(score_status_V0_PGMC_other_TARDBP,"PGMC (other mutation) vs PGMC (TARDBP) at V0")
save_plot_pdf(p, filename = "plots/violin_V0_PGMC_other_vs_TARDBP.pdf")

# grouped violins
p = grouped_violin_plot(score_status_V0_V1_ALS_PGMC, "ALS and PGMC scores at V0 and V1")
save_plot_pdf(p, filename = "plots/grouped_violin_ALS_PGMC.pdf")
p = grouped_violin_plot(score_status_V0_V1_ALS_CTR, "ALS and CTR scores at V0 and V1")
save_plot_pdf(p, filename = "plots/grouped_violin_ALS_CTR.pdf")
p = grouped_violin_plot(score_status_V0_V1_CTR_PGMC, "CTR and PGMC scores at V0 and V1")
save_plot_pdf(p, filename = "plots/grouped_violin_CTR_PGMC.pdf")
score_status_V0_V1_reduced$status <- factor(score_status_V0_V1_reduced$status,levels = c("CTR","PGMC","ALS"))
p = grouped_violin_plot(score_status_V0_V1_reduced, "ALS, CTR and PGMC scores at V0 and V1 (both visits)")
save_plot_pdf(p, filename = "plots/grouped_violin_ALS_CTR_PGMC_bothvisits.pdf",width = 8,height = 6)
score_status_V0_V1$status <- factor(score_status_V0_V1$status,levels = c("CTR","PGMC","ALS"))
p = grouped_violin_plot(score_status_V0_V1, "ALS, CTR and PGMC scores at V0 and V1 (all participants)") 
save_plot_pdf(p, filename = "plots/grouped_violin_ALS_CTR_PGMC_all.pdf",width = 8,height = 6)
p = grouped_violin_plot_mutation(score_status_V0_PGMC_mutations,"PGMC mutations at V0")
save_plot_pdf(p, filename = "plots/grouped_violin_PGMC_mutations.pdf",width = 8,height = 6)

# pairwise violin plots
score_status_V0_V1_reduced_PGMC = score_status_V0_V1_reduced %>% filter(status == "PGMC") 
p = pairwise_plots(score_status_V0_V1_reduced_PGMC,"PGMC at V0 and V1",'#ad5291')
save_plot_pdf(p, filename = "plots/pairwise_violin_PGMC_bothvisits.pdf")
# score_status_V0_V1_PGMC = rbind(score_status_V0_V1_reduced_PGMC,
#                                 score_status_V0_V1 %>% 
#                                   filter(status == "PGMC" & (!PatientID %in% score_status_V0_V1_reduced_PGMC$PatientID))) 
# p = pairwise_plots(score_status_V0_V1_PGMC,"PGMC at V0 and V1",'#ad5291')
# save_plot_pdf(p, filename = "plots/pairwise_violin_PGMC_all.pdf")
p = pairwise_plots(score_status_V0_V1_reduced %>% filter(status == "ALS"),"ALS at V0 and V1",'#B2936F')
save_plot_pdf(p, filename = "plots/pairwise_violin_ALS_bothvisits.pdf")
p = pairwise_plots(score_status_V0_V1_reduced %>% filter(status == "CTR"),"CTR at V0 and V1",'#6F8EB2')
save_plot_pdf(p, filename = "plots/pairwise_violin_CTR_bothvisits.pdf")

# other possible visualizations 
p = ggviolin(score_status_V0_V1_reduced, x= "visit", y="score", color = "visit",palette = "jco") + 
  stat_compare_means(comparisons = list(c("V0","V1"))) + facet_wrap(~status)
save_plot_pdf(p, filename = "plots/pairwise_violin_facet.pdf")
p = ggline(score_status_V0_V1_reduced,x= "visit", y ="score", color = "status", add = c("mean_sd","jitter")) + 
  scale_color_manual(values  = c('CTR' = '#6F8EB2','ALS' = '#B2936F','PGMC' = '#ad5291'))  + 
  facet_wrap(~status) + stat_compare_means(comparisons = list(c("V0","V1")))
save_plot_pdf(p, filename = "plots/pairwise_line_facet.pdf")
p = ggline(score_status_V0_V1_reduced,x= "visit", y ="score", color = "status", add = c("median_sd","jitter")) + 
  scale_color_manual(values  = c('CTR' = '#6F8EB2','ALS' = '#B2936F','PGMC' = '#ad5291'))  + 
  facet_wrap(~status) + stat_compare_means(comparisons = list(c("V0","V1")))
save_plot_pdf(p, filename = "plots/pairwise_line_median_facet.pdf")
res.aov = anova_test(
  data = score_status_V0_V1, dv = score, wid = PatientID,
  between = status, within = visit
)
sign <- score_status_V0_V1 %>%
  group_by(visit) %>%
  pairwise_t_test(score ~ status, p.adjust.method = "bonferroni")  %>%
  add_xy_position(x = "visit")
p = ggboxplot(score_status_V0_V1, x = "visit", y = "score",color = "status",palette = "jco") + 
  stat_pvalue_manual(sign,label = "p", y.position = 13, tip.length = 0.02,step.increase = 0.1) +
 labs(subtitle = get_test_label(res.aov, detailed = TRUE), caption = get_pwc_label(sign)) +
  geom_label_repel(stat = "summary", fun = mean, size = 3.5,
                   aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                      round(after_stat(y), 2))),
                   parse = TRUE, position = position_dodge(0.6)) +
  scale_color_manual(values  = c('CTR' = '#6F8EB2',  
                                 'ALS' = '#B2936F',
                                 'PGMC' = '#ad5291')) +



# ## timeline of visits (DEPRECATED)
# # ALS vs CTR
# nr_odors_all <- nr_odors_cohorts(data_V0,data_V1,48,48,37,39)
# 
# odors = nr_odors_all$Odor %>% unique()
# list_plots = list()
# for (i in 1:length(odors)) {
#   odor_i = nr_odors_all %>%
#     filter(Odor == odors[i]) %>%
#     select(-Odor)
#   p = ggplot(odor_i, aes(x = visit, y = value, color = status, shape = status, group = status)) +
#     geom_point(aes(size = 2)) +
#     geom_line() + 
#     scale_color_manual(values  = c('CTR' = '#6F8EB2',  
#                                    'ALS' = '#B2936F')) +
#     theme_minimal() + 
#     xlab("visits") +
#     ylab("Percentage of patients that detected smell (%)") + 
#     ggtitle(odors[i]) 
#   list_plots[[i]] <- p
# }
# 
# pdf(file="plots/timeline_odors_ALS_CTR.pdf",width = 12, height = 10)
# plot_grid(list_plots[[1]], list_plots[[2]], list_plots[[3]], list_plots[[4]], 
#           list_plots[[5]], list_plots[[6]], list_plots[[7]], list_plots[[8]],
#           list_plots[[9]], list_plots[[10]], list_plots[[11]], list_plots[[12]], 
#           ncol = 4, nrow=3)
# dev.off()
# 
# # ALS vs PGMC
# nr_odors_PGMC_all = nr_odors_cohorts(data_V0_PGMC,data_V1_PGMC,
#                                      52,48,45,39)
# 
# odors = nr_odors_PGMC_all$Odor %>% unique()
# list_plots = list()
# for (i in 1:length(odors)) {
#   odor_i = nr_odors_PGMC_all %>%
#     filter(Odor == odors[i]) %>%
#     select(-Odor)
#   p = ggplot(odor_i, aes(x = visit, y = value, color = status, shape = status, group = status)) +
#     geom_point(aes(size = 2)) +
#     geom_line() + 
#     scale_color_manual(values  = c('PGMC' = '#ad5291',  
#                                    'ALS' = '#B2936F')) +
#     theme_minimal() + 
#     xlab("visits") +
#     ylab("Percentage of patients that detected smell (%)") + 
#     ggtitle(odors[i]) 
#   list_plots[[i]] <- p
# }
# 
# pdf(file="plots/timeline_odors_ALS_PGMC.pdf",width = 12, height = 10)
# plot_grid(list_plots[[1]], list_plots[[2]], list_plots[[3]], list_plots[[4]], 
#           list_plots[[5]], list_plots[[6]], list_plots[[7]], list_plots[[8]],
#           list_plots[[9]], list_plots[[10]], list_plots[[11]], list_plots[[12]], 
#           ncol = 4, nrow=3)
# dev.off()


# ------------------------------------------------------------------------------
# auxiliar functions
volcano_plot_CTR <- function(data,type_stat,title_plot,threshold){
  if(type_stat == "p-value"){
    data = data %>%
      mutate(detection = ifelse(((`Pr(>|z|)` < threshold) & (eff < 0)), "detected in CTR",
                                ifelse(((`Pr(>|z|)` < threshold) & (eff > 0)), "detected in ALS","not significant")))
    ggplot(data,aes(eff, forcats::fct_reorder(Variables, `Pr(>|z|)`,.desc=TRUE))) + 
      geom_segment(aes(xend=0, yend = Variables)) +
      geom_point(aes(color=detection, size = log10_pval)) +
      #scale_color_viridis_c(guide=guide_colorbar(reverse=FALSE)) +
      scale_color_manual(values  = c('detected in CTR' = '#6F8EB2', 
                                     'not significant' = 'grey48', 
                                     'detected in ALS' = '#B2936F')) +
      #    c("#6F8EB2", "grey48", "#B2936F"),  labels = c("detected in CTR", "not significant", "detected in ALS")) +
      scale_size_continuous(range=c(2, 7)) +
      theme_minimal() + 
      xlab("log(odds-ratio)") +
      ylab(NULL) + 
      ggtitle(title_plot) +
      labs(size = expression("-log"[10]*"(p-value)"))
    # ggplot(data, aes(x = eff, y = log10_pval, color = detection)) + 
    #   geom_point(size = 2) +
    #   scale_color_manual(values = c("#6F8EB2", "grey48", "#B2936F"), 
    #                      labels = c("detected in CTR", "not significant", "detected in ALS")) +
    #   geom_text_repel(data = filter(data, `Pr(>|z|)` < threshold), 
    #                   mapping = aes(x = eff, y = log10_pval,  label = Variables), 
    #                   min.segment.length = 0, 
    #                   seed = 42, box.padding = 0.8, 
    #                   nudge_x = 0.5,
    #                   max.overlaps = 15,
    #                   segment.color = "grey52") +
    #   geom_hline(yintercept=(-log10(threshold)), linetype='dotted', col = 'grey')+
    #   theme(axis.text=element_text(size=14),
    #         axis.title=element_text(size=14,face="bold"),
    #         plot.title = element_text(hjust = 0.5),
    #         #axis.text.x = element_blank(),
    #         #axis.ticks.x = element_blank(),
    #         panel.background = element_rect(fill = "white")) +
    #   labs(title = title_plot,
    #        x = "log(OR)",
    #        y = expression("-log"[10]*"(p-value)")) 
  }
  else{
    data = data %>%
      mutate(detection = ifelse(fdr < threshold & eff < 0, "detected in CTR",
                                ifelse(fdr < threshold & eff > 0, "detected in ALS","not significant")))
    print(data)
    ggplot(data,aes(eff, forcats::fct_reorder(Variables, fdr,.desc=TRUE))) + 
      geom_segment(aes(xend=0, yend = Variables)) +
      geom_point(aes(color=detection, size = log10_padj)) +
      #scale_color_viridis_c(guide=guide_colorbar(reverse=FALSE)) +
      scale_color_manual(values  = c('detected in CTR' = '#6F8EB2', 
                                     'not significant' = 'grey48', 
                                     'detected in ALS' = '#B2936F')) +
      scale_size_continuous(range=c(2, 7)) +
      theme_minimal() + 
      xlab("log(odds-ratio)") +
      ylab(NULL) + 
      ggtitle(title_plot) +
      labs(size = expression("-log"[10]*"(adjusted p-value)"))
    # ggplot(data, aes(x = eff, y = log10_pval, colour = detection)) + 
    #   geom_point(size = 2) +
    #   scale_color_manual(values = c("#6F8EB2", "grey48", "#B2936F"), 
    #                      labels = c("detected in CTR", "not significant", "detected in ALS")) +
    #   geom_text_repel(data = filter(data, (fdr) < threshold), mapping = aes(x = eff, y = log10_padj, 
    #                                                                   label = Labels_plot), 
    #                   min.segment.length = 0, 
    #                   seed = 42, box.padding = 0.7, 
    #                   nudge_x = 0.4,
    #                   max.overlaps = 30,
    #                   segment.color = "grey52") +
    #   geom_hline(yintercept=(-log10(0.2)), linetype='dotted', col = 'grey') +
    #   theme(axis.text=element_text(size=14),
    #         axis.title=element_text(size=14,face="bold"),
    #         plot.title = element_text(hjust = 0.5),
    #         legend.text=element_text(size=14),
    #         legend.position = c(0.1, 0.9),
    #         axis.text.x = element_blank(),
    #         axis.ticks.x = element_blank(),
    #         panel.background = element_rect(fill = "white")) +
    #   labs(title = title_plot,
    #        x = "log(OR)",
    #        y = expression("-log"[10]*"(adjusted p-value)"))
  }
}


volcano_plot_PGMC <- function(data,type_stat,title_plot,threshold){
  if(type_stat == "p-value"){
    data = data %>%
      mutate(detection = ifelse(((`Pr(>|z|)` < threshold) & (eff < 0)), "detected in PGMC",
                                ifelse(((`Pr(>|z|)` < threshold) & (eff > 0)), "detected in ALS","not significant")))
    ggplot(data,aes(eff, forcats::fct_reorder(Variables, `Pr(>|z|)`,.desc=TRUE))) + 
      geom_segment(aes(xend=0, yend = Variables)) +
      geom_point(aes(color=detection, size = log10_pval)) +
      scale_color_manual(values  = c('detected in PGMC' = '#ad5291', 
                                     'not significant' = 'grey48', 
                                     'detected in ALS' = '#B2936F')) +
      scale_size_continuous(range=c(2, 7)) +
      theme_minimal() + 
      xlab("log(odds-ratio)") +
      ylab(NULL) + 
      ggtitle(title_plot) +
      labs(size = expression("-log"[10]*"(p-value)"))
  }
  else{
    data = data %>%
      mutate(detection = ifelse(fdr < threshold & eff < 0, "detected in PGMC",
                                ifelse(fdr < threshold & eff > 0, "detected in ALS","not significant")))
    print(data)
    ggplot(data,aes(eff, forcats::fct_reorder(Variables, fdr,.desc=TRUE))) + 
      geom_segment(aes(xend=0, yend = Variables)) +
      geom_point(aes(color=detection, size = log10_padj)) +
      scale_color_manual(values  = c('detected in PGMC' = '#ad5291', 
                                     'not significant' = 'grey48', 
                                     'detected in ALS' = '#B2936F')) +
      scale_size_continuous(range=c(2, 7)) +
      theme_minimal() + 
      xlab("log(odds-ratio)") +
      ylab(NULL) + 
      ggtitle(title_plot) +
      labs(size = expression("-log"[10]*"(adjusted p-value)"))
  }
}

save_plot_pdf <- function(x, filename, width=11/2, height=8/2) {
  stopifnot(!missing(x))
  stopifnot(!missing(filename))
  pdf(filename, width=width, height=height)
  print(x)
  # grid::grid.newpage()
  # grid::grid.draw(x$gtable)
  dev.off()
}

nr_odors_cohorts <- function(data_V0, 
                             data_V1, 
                             amount_patients_1_V0,amount_patients_2_V0,
                             amount_patients_1_V1,amount_patients_2_V1){
  nr_V0 = data_V0 %>%  
    group_by(status) %>% 
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(visit = rep("V0",2))
  nr_V0[1,2:13] <- round(nr_V0[1,2:13]/amount_patients_1_V0 * 100,2)
  nr_V0[2,2:13] <- round(nr_V0[2,2:13]/amount_patients_2_V0 * 100,2)
  nr_V1 = data_V1 %>%  
    group_by(status) %>% 
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(visit = rep("V1",2))
  nr_V1[1,2:13] <- round(nr_V1[1,2:13]/amount_patients_1_V1 * 100,2)
  nr_V1[2,2:13] <- round(nr_V1[2,2:13]/amount_patients_2_V1 * 100,2)
  nr_all = rbind(nr_V0,nr_V1) |> 
    pivot_longer(-c(status, visit),
                 names_to = "Odor",
                 values_to = "value") 
  return(nr_all)
}

histogram_plot = function(data,title_plot,max_y){
  ggplot(data, aes(x=score, fill=status)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values  = c('CTR' = '#6F8EB2',  
                                  'ALS' = '#B2936F',
                                  'PGMC' = '#ad5291')) +
    labs(fill="") + 
    theme_minimal() +
    xlab("B-SIT score") + 
    ylab("Participants (n)") +
    xlim(-2,14) + 
    ylim(0,max_y) + 
    ggtitle(title_plot)
}

violin_plots <- function(data,title_plot){
  # stats_results <- ggbetweenstats(data, status, score) %>% extract_subtitle()
  ggbetweenstats(data,x = status, y = score,
                 type = "np",
                 conf.level = 0.95,
                 results.subtitle = TRUE) + 
    labs(x = "",
         y = "B-SIT score",
         #subtitle = stats_results,
         title = title_plot) +
    scale_color_manual(values  = c('CTR' = '#6F8EB2',  
                                   'ALS' = '#B2936F',
                                   'PGMC' = '#ad5291',
                                   'PGMC_other' = '#ad5291',
                                   'PGMC_C9orf72' = '#55aa82',
                                   'PGMC_SOD1' = '#55aa82',
                                   'PGMC_TARDBP' = '#55aa82'))
}

pairwise_plots <- function(data,title_plot,colour_plot){
  stats_results <- ggbetweenstats(data, visit, score) %>% extract_subtitle()
  ggwithinstats(data,x = visit, y = score,
                 type = "p",
                 conf.level = 0.95,
                 results.subtitle = FALSE) + 
    labs(x = "",
         y = "B-SIT score",
         subtitle = stats_results,
         title = title_plot) +
    scale_color_manual(values = c(colour_plot,colour_plot))
}

grouped_violin_plot <- function(data,title_plot){
  # stat.test <- data %>%
  #   group_by(visit) %>%
  #   t_test(status ~ score)
  # stat.test <- stat.test %>%
  #   add_xy_position(x = "visit", dodge = 0.8)
  # res.aov = anova_test(
  #   data = data, dv = score, wid = PatientID,
  #   between = status, within = visit
  # )
  stats_results_V0 <-  kruskal.test(score ~ status, data = data %>% filter(visit == "V0"))
  stats_results_V1 <-  kruskal.test(score ~ status, data = data %>% filter(visit == "V1"))
  sign <- data %>%
    group_by(visit) %>%
    pairwise_wilcox_test(score ~ status, p.adjust.method = "bonferroni")  %>%
    add_xy_position(x = "visit")
  ggplot(data, aes(visit, score, group = interaction(visit,status))) +
    geom_point(aes(color = status, fill = after_scale(alpha(colour, 0.5))), 
               position = position_jitterdodge(dodge.width = 0.6, 0.1),
               size = 3, shape = 21) +
    geom_boxplot(fill = NA, color = "black", width = 0.2, linewidth = 0.4,
                 position = position_dodge(0.6)) +
    geom_violin(fill = NA, color = "black", width = 0.6, linewidth = 0.4,
                position = position_dodge(0.6)) +
    geom_point(stat = "summary", size = 4, color = "#8a0f00",
               position = position_dodge(0.6), fun = mean) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_size = 12) +
    theme(axis.title = element_text(face = 2),
          legend.position = "bottom",
          axis.text.y.right = element_blank()) + 
    geom_label_repel(stat = "summary", fun = mean, size = 3.5,
                     aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                        round(after_stat(y), 2))),
                     parse = TRUE, position = position_dodge(0.6)) +
    scale_color_manual(values  = c('CTR' = '#6F8EB2',  
                                   'ALS' = '#B2936F',
                                   'PGMC' = '#ad5291')) +
    # stat_pvalue_manual(
    #   stat.test, label = "p.adj", tip.length = 0.01,
    #   bracket.nudge.y = -2) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Visits",
         y = "B-SIT score",
         title = title_plot) +
    stat_pvalue_manual(sign,label = "p",
                       y.position = 13, 
                       tip.length = 0.02,
                       dodge = 0.8,
                       step.increase = 0.1,
                       step.group.by = "visit") +
    labs(subtitle = paste0("Kruskal-Wallis (V0) = ",round(stats_results_V0$p.value,3),
                           "; Kruskal-Wallis (V1) = ",round(stats_results_V1$p.value,3)))
}

grouped_violin_plot_mutation <- function(data,title_plot){
  stats_results_V0 <-  kruskal.test(score ~ status, data = data)
  sign <- data %>%
    pairwise_wilcox_test(score ~ status, p.adjust.method = "bonferroni") 
  ggplot(data, aes(status, score, group = status)) +
    geom_point(aes(color = status, fill = after_scale(alpha(colour, 0.5))), 
               position = position_jitterdodge(dodge.width = 0.6, 0.1),
               size = 3, shape = 21) +
    geom_boxplot(fill = NA, color = "black", width = 0.2, linewidth = 0.4,
                 position = position_dodge(0.6)) +
    geom_violin(fill = NA, color = "black", width = 0.6, linewidth = 0.4,
                position = position_dodge(0.6)) +
    geom_point(stat = "summary", size = 4, color = "#8a0f00",
               position = position_dodge(0.6), fun = mean) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_size = 12) +
    theme(axis.title = element_text(face = 2),
          legend.position = "bottom",
          axis.text.y.right = element_blank()) + 
    geom_label_repel(stat = "summary", fun = mean, size = 3.5,
                     aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                        round(after_stat(y), 2))),
                     parse = TRUE, position = position_dodge(0.6)) +
    scale_color_manual(values  = c('PGMC_other' = '#ad5291',
                                   'PGMC_C9orf72' = '#55aa82',
                                   'PGMC_SOD1' = '#4661b9',
                                   'PGMC_TARDBP' = '#B99E46')) +
    # stat_pvalue_manual(
    #   stat.test, label = "p.adj", tip.length = 0.01,
    #   bracket.nudge.y = -2) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Mutations at PGMC",
         y = "B-SIT score",
         title = title_plot) +
    stat_pvalue_manual(sign,label = "p",
                       y.position = 13, 
                       tip.length = 0.02,
                       dodge = 0.8,
                       step.increase = 0.1) +
    labs(subtitle = paste0("Kruskal-Wallis = ",round(stats_results_V0$p.value,3)))
}
