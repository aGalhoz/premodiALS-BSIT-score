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

# smell data
smell_data = BSIT %>%
  select(PatientID,contains("Odor")) %>%
  unique()

smell_data_V0 = smell_data[!duplicated(smell_data$PatientID),]
smell_data_V1 = smell_data[duplicated(smell_data$PatientID),]

# smell data ALS
smell_data_V0_ALS = smell_data_V0 %>% # 48 ALS with V0 info
  filter(PatientID %in% patients_ALS$PatientID)
smell_data_V1_ALS = smell_data_V1 %>% # 39 ALS with V1 info
  filter(PatientID %in% patients_ALS$PatientID)

# smell data CTR
smell_data_V0_CTR = smell_data_V0 %>% # 48 CTR with V0 info
  filter(PatientID %in% patients_ctr$PatientID)
smell_data_V1_CTR = smell_data_V1 %>% # 37 CTR with V1 info
  filter(PatientID %in% patients_ctr$PatientID)

# imputation of data (if non available I assume undetected)
imputation_data = function(data){
  data[is.na(data)] <- 2 # this is the value if not detected
  return(data)
}
# -> ALS
smell_data_V0_ALS = imputation_data(smell_data_V0_ALS)
smell_data_V1_ALS = imputation_data(smell_data_V1_ALS)
# -> CTR
smell_data_V0_CTR = imputation_data(smell_data_V0_CTR)
smell_data_V1_CTR = imputation_data(smell_data_V1_CTR)

# make final data for analysis 
smell_data_V0_ALS_CTR = rbind(smell_data_V0_ALS[,2:13],
                              smell_data_V0_CTR[,2:13])
smell_data_V1_ALS_CTR = rbind(smell_data_V1_ALS[,2:13],
                              smell_data_V1_CTR[,2:13])
status_V0 = c(rep("ALS",nrow(smell_data_V0_ALS)),
                  rep("CTR", nrow(smell_data_V0_CTR)))
status_V1 = c(rep("ALS",nrow(smell_data_V1_ALS)),
                  rep("CTR", nrow(smell_data_V1_CTR)))