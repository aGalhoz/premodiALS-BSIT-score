source("00_initialization.R")

## All patients
patients_all <- GeneralDocumentation %>%
  select(PatientID, PGMC, ALSuncertainty, ALSFUdiagnosis,LFU) %>%
  unique()

## control patients
patients_ctr = patients_all %>% # 48 control patients
  filter(PGMC == 2)

## ALS patients: ALS certainly @V0 + ALS diagnosis @V1
patients_ALS = patients_all %>% # 48 ALS patients
  filter(ALSuncertainty == 1 | ALSFUdiagnosis == 1)

# PGMC = 3 but did not answered ALS uncertainty
patients_PGMC3_ALSuncertainty_NA <- patients_all %>%
  filter(PGMC == 3 & is.na(ALSuncertainty)) %>%
  left_join(GeneralDocumentation %>% select(PatientID,ParticipantCode))# 14 patients
writexl::write_xlsx(patients_PGMC3_ALSuncertainty_NA,"results/patients_ALSuncertaintyNA.xlsx")

# PGMC = 3, with BSITExist = 1 but did not answered ALSFU diagnosis
patients_PGMC3_ALSFUdiagnosis_NA <- patients_all %>%
  left_join(BSIT %>% select(PatientID, BsitExists)) %>%
  filter(PGMC == 3 & BsitExists == 1 &  is.na(ALSFUdiagnosis)) %>%
  left_join(GeneralDocumentation %>% select(PatientID,ParticipantCode)) %>%
  left_join(patients_ALSFUdiagnosisNA_LT %>% select(PatientID,`...7`)) %>%
  rename(comment = `...7`)# 111 patients 
writexl::write_xlsx(patients_PGMC3_ALSFUdiagnosis_NA,"results/patients_ALSFUdiagnosisNA.xlsx")

# PGMC patients 
patients_pgmc = GeneralDocumentation %>%
  select(PatientID, PGMC,MutationTypeC9orf72,
         MutationTypeOther, MutationTypeSOD1, MutationTypeTARDBP) %>% # 52 pgmc patients
  filter(PGMC == 1)
patients_pgmc_mut_other = GeneralDocumentation %>%
  select(PatientID, PGMC,MutationTypeC9orf72,
         MutationTypeOther, MutationTypeSOD1, MutationTypeTARDBP) %>% # 5 pgmc patients
  filter(PGMC == 1 & MutationTypeOther == 1)
patients_pgmc_mut_C9orf72 = GeneralDocumentation %>%
  select(PatientID, PGMC,MutationTypeC9orf72,
         MutationTypeOther, MutationTypeSOD1, MutationTypeTARDBP) %>% # 21 pgmc patients
  filter(PGMC == 1 & MutationTypeC9orf72 == 1)
patients_pgmc_mut_TARDBP = GeneralDocumentation %>%
  select(PatientID, PGMC,MutationTypeC9orf72,
         MutationTypeOther, MutationTypeSOD1, MutationTypeTARDBP) %>% # 5 pgmc patients
  filter(PGMC == 1 & MutationTypeTARDBP == 1)
patients_pgmc_mut_SOD1 = GeneralDocumentation %>%
  select(PatientID, PGMC,MutationTypeC9orf72,
         MutationTypeOther, MutationTypeSOD1, MutationTypeTARDBP) %>% # 15 pgmc patients
  filter(PGMC == 1 & MutationTypeSOD1 == 1)

# smell data
smell_data = BSIT %>%
  select(PatientID,contains("Odor")) %>%
  unique()

smell_data_V0 = smell_data[!duplicated(smell_data$PatientID),]
smell_data_V1 = smell_data[duplicated(smell_data$PatientID),]

# smell data ALS
smell_data_V0_ALS = smell_data_V0 %>% # 47 ALS with V0 info
  filter(PatientID %in% patients_ALS$PatientID) %>%
  filter(rowSums(is.na(.))<12)
smell_data_V1_ALS = smell_data_V1 %>% # 13 ALS with V1 info
  filter(PatientID %in% patients_ALS$PatientID) %>%
  filter(rowSums(is.na(.))<12)

# smell data CTR
smell_data_V0_CTR = smell_data_V0 %>% # 48 CTR with V0 info
  filter(PatientID %in% patients_ctr$PatientID) %>%
  filter(rowSums(is.na(.))<12)
smell_data_V1_CTR = smell_data_V1 %>% # 21 CTR with V1 info
  filter(PatientID %in% patients_ctr$PatientID) %>%
  filter(rowSums(is.na(.))<12)

# smell data PGMC
smell_data_V0_PGMC = smell_data_V0 %>% # 52 PGMC with V0 info
  filter(PatientID %in% patients_pgmc$PatientID) %>%
  filter(rowSums(is.na(.))<12)
smell_data_V1_PGMC = smell_data_V1 %>% # 25 PGMC with V1 info
  filter(PatientID %in% patients_pgmc$PatientID) %>%
  filter(rowSums(is.na(.))<12)

# smell data PGMC (other mutation)
smell_data_V0_PGMC_mut_other = smell_data_V0 %>% # 5 PGMC with V0 info
  filter(PatientID %in% patients_pgmc_mut_other$PatientID) %>%
  filter(rowSums(is.na(.))<12)

# smell data PGMC (mutation C9orf72)
smell_data_V0_PGMC_mut_C9orf72 = smell_data_V0 %>% # 21 PGMC with V0 info
  filter(PatientID %in% patients_pgmc_mut_C9orf72$PatientID) %>%
  filter(rowSums(is.na(.))<12)

# smell data PGMC (mutation SOD1)
smell_data_V0_PGMC_mut_SOD1 = smell_data_V0 %>% # 15 PGMC with V0 info
  filter(PatientID %in% patients_pgmc_mut_SOD1$PatientID) %>%
  filter(rowSums(is.na(.))<12)

# smell data PGMC (mutation TARDBP)
smell_data_V0_PGMC_mut_TARDBP = smell_data_V0 %>% # 5 PGMC with V0 info
  filter(PatientID %in% patients_pgmc_mut_TARDBP$PatientID) %>%
  filter(rowSums(is.na(.))<12)

# check which patients have NA in the data
data_V0_na <- smell_data_V0[rowSums(is.na(smell_data_V0)) > 0,] %>%
  left_join(GeneralDocumentation %>% select(PatientID,ParticipantCode)) %>%
  mutate(visit = rep("V0",1)) %>%
  unique()
data_V1_na <- smell_data_V1[rowSums(is.na(smell_data_V1)) > 0,] %>%
  left_join(GeneralDocumentation %>% select(PatientID,ParticipantCode)) %>%
  mutate(visit = rep("V1",88)) %>%
  unique()

data_na_allvisits <- rbind(data_V0_na,
                           data_V1_na)

writexl::write_xlsx(data_na_allvisits,"results/data_NA.xlsx")

# # imputation of data (if non available I assume undetected)
# imputation_data = function(data){
#   data[is.na(data)] <- 2 # this is the value if not detected
#   return(data)
# }
# # -> ALS
# smell_data_V0_ALS = imputation_data(smell_data_V0_ALS)
# smell_data_V1_ALS = imputation_data(smell_data_V1_ALS)
# # -> CTR
# smell_data_V0_CTR = imputation_data(smell_data_V0_CTR)
# smell_data_V1_CTR = imputation_data(smell_data_V1_CTR)
# # -> PGMC
# smell_data_V0_PGMC = imputation_data(smell_data_V0_PGMC)
# smell_data_V1_PGMC = imputation_data(smell_data_V1_PGMC)

# change coding of 2 -> 0
change_coding_data = function(data){
  data[data == 2] <- 0 # this is the value if not detected
  return(data)
}
# -> ALS
smell_data_V0_ALS = change_coding_data(smell_data_V0_ALS)
smell_data_V1_ALS = change_coding_data(smell_data_V1_ALS)
# -> CTR
smell_data_V0_CTR = change_coding_data(smell_data_V0_CTR)
smell_data_V1_CTR = change_coding_data(smell_data_V1_CTR)
# -> PGMC
smell_data_V0_PGMC = change_coding_data(smell_data_V0_PGMC)
smell_data_V1_PGMC = change_coding_data(smell_data_V1_PGMC)
# -> PGMC with several mutations
smell_data_V0_PGMC_mut_other = change_coding_data(smell_data_V0_PGMC_mut_other)
smell_data_V0_PGMC_mut_C9orf72 = change_coding_data(smell_data_V0_PGMC_mut_C9orf72)
smell_data_V0_PGMC_mut_SOD1 = change_coding_data(smell_data_V0_PGMC_mut_SOD1)
smell_data_V0_PGMC_mut_TARDBP = change_coding_data(smell_data_V0_PGMC_mut_TARDBP)

# summarise all odors to a single score
summarise_score <- function(data){
  data = data %>%
    group_by(PatientID) %>%
    mutate(score = rowSums(across(where(is.numeric))))
  return(data)
}
smell_data_V0_ALS = summarise_score(smell_data_V0_ALS)
smell_data_V0_CTR = summarise_score(smell_data_V0_CTR)
smell_data_V0_PGMC = summarise_score(smell_data_V0_PGMC)
smell_data_V0_PGMC_mut_other = summarise_score(smell_data_V0_PGMC_mut_other)
smell_data_V0_PGMC_mut_C9orf72 = summarise_score(smell_data_V0_PGMC_mut_C9orf72)
smell_data_V0_PGMC_mut_SOD1 = summarise_score(smell_data_V0_PGMC_mut_SOD1)
smell_data_V0_PGMC_mut_TARDBP = summarise_score(smell_data_V0_PGMC_mut_TARDBP)
smell_data_V1_ALS = summarise_score(smell_data_V1_ALS)
smell_data_V1_CTR = summarise_score(smell_data_V1_CTR)
smell_data_V1_PGMC = summarise_score(smell_data_V1_PGMC)


# make final data for analysis 
smell_data_V0_ALS_CTR = rbind(smell_data_V0_ALS,
                              smell_data_V0_CTR)
score_V0_ALS_CTR = data.frame(score = c(smell_data_V0_ALS$score,
                         smell_data_V0_CTR$score))
smell_data_V1_ALS_CTR = rbind(smell_data_V1_ALS,
                              smell_data_V1_CTR)
score_V1_ALS_CTR = data.frame(score = c(smell_data_V1_ALS$score,
                        smell_data_V1_CTR$score))
smell_data_V0_ALS_PGMC = rbind(smell_data_V0_ALS,
                               smell_data_V0_PGMC)
score_V0_ALS_PGMC = data.frame(score = c(smell_data_V0_ALS$score,
                          smell_data_V0_PGMC$score))
score_data_V0_PGMC_mut_other_C9orf72 = data.frame(score = c(smell_data_V0_PGMC_mut_other$score,
                                                            smell_data_V0_PGMC_mut_C9orf72$score))
score_data_V0_PGMC_mut_other_SOD1 = data.frame(score = c(smell_data_V0_PGMC_mut_other$score,
                                                            smell_data_V0_PGMC_mut_SOD1$score))
score_data_V0_PGMC_mut_other_TARDBP = data.frame(score = c(smell_data_V0_PGMC_mut_other$score,
                                                            smell_data_V0_PGMC_mut_TARDBP$score))
smell_data_V1_ALS_PGMC = rbind(smell_data_V1_ALS,
                               smell_data_V1_PGMC)
score_V1_ALS_PGMC = data.frame(score = c(smell_data_V1_ALS$score,
                          smell_data_V1_PGMC$score))
smell_data_V0_CTR_PGMC = rbind(smell_data_V0_CTR,
                               smell_data_V0_PGMC)
score_V0_CTR_PGMC = data.frame(score = c(smell_data_V0_CTR$score,
                                         smell_data_V0_PGMC$score))
smell_data_V1_CTR_PGMC = rbind(smell_data_V1_CTR,
                               smell_data_V1_PGMC)
score_V1_CTR_PGMC = data.frame(score = c(smell_data_V1_CTR$score,
                                         smell_data_V1_PGMC$score))

status_V0_ALS_CTR = c(rep("ALS",nrow(smell_data_V0_ALS)),
                  rep("CTR", nrow(smell_data_V0_CTR)))
status_V1_ALS_CTR  = c(rep("ALS",nrow(smell_data_V1_ALS)),
                  rep("CTR", nrow(smell_data_V1_CTR)))
status_V0_ALS_PGMC = c(rep("ALS",nrow(smell_data_V0_ALS)),
                   rep("PGMC", nrow(smell_data_V0_PGMC)))
status_V1_ALS_PGMC = c(rep("ALS",nrow(smell_data_V1_ALS)),
                   rep("PGMC", nrow(smell_data_V1_PGMC)))
status_V0_CTR_PGMC = c(rep("CTR",nrow(smell_data_V0_CTR)),
                       rep("PGMC", nrow(smell_data_V0_PGMC)))
status_V1_CTR_PGMC = c(rep("CTR",nrow(smell_data_V1_CTR)),
                       rep("PGMC", nrow(smell_data_V1_PGMC)))
status_V0_PGMC_other_C9orf72 = c(rep("PGMC_other", nrow(smell_data_V0_PGMC_mut_other)),
                                 rep("PGMC_C9orf72", nrow(smell_data_V0_PGMC_mut_C9orf72)))
status_V0_PGMC_other_SOD1 = c(rep("PGMC_other", nrow(smell_data_V0_PGMC_mut_other)),
                                 rep("PGMC_SOD1", nrow(smell_data_V0_PGMC_mut_SOD1)))
status_V0_PGMC_other_TARDBP = c(rep("PGMC_other", nrow(smell_data_V0_PGMC_mut_other)),
                                 rep("PGMC_TARDBP", nrow(smell_data_V0_PGMC_mut_TARDBP)))