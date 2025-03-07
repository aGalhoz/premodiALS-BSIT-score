source("00_initialization.R")

## All patients
patients_all <- GeneralDocumentation %>%
  select(PatientID, PGMC, ALSuncertainty, ALSFUdiagnosis) %>%
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
  left_join(GeneralDocumentation %>% select(PatientID,ParticipantCode))# 111 patients
writexl::write_xlsx(patients_PGMC3_ALSFUdiagnosis_NA,"results/patients_ALSFUdiagnosisNA.xlsx")

# PGMC patients 
patients_pgmc = patients_all %>% # 52 pgmc patients
  filter(PGMC == 1)

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