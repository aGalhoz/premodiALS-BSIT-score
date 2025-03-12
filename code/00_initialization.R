### Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(tidyr)
library(cowplot)
library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)
library(rstatix)
library(ggpubr)

### Directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# create directory for results
dir.create(file.path(getwd(),'results'), showWarnings = FALSE)
# create directory for plots
dir.create(file.path(getwd(),'plots'), showWarnings = FALSE)

### Collect data
# raw data with smells
BSIT <- read_delim("export-2025-02-25-PREMODIALS-AKDTR_CHUFR_HMCIL_KSSGCH_MRI_NIUSASSK_197 patients/BSIT.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

# meanings of raw data
BSIT_dic <- read_delim("export-2025-02-25-PREMODIALS-AKDTR_CHUFR_HMCIL_KSSGCH_MRI_NIUSASSK_197 patients/BSIT(C).csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# patient demographics
GeneralDocumentation <- read_delim("export-2025-02-25-PREMODIALS-AKDTR_CHUFR_HMCIL_KSSGCH_MRI_NIUSASSK_197 patients/GeneralDocumentation.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

# meanings of patient demographics
GeneralDocumentation_dic <- read_delim("export-2025-02-25-PREMODIALS-AKDTR_CHUFR_HMCIL_KSSGCH_MRI_NIUSASSK_197 patients/GeneralDocumentation(C).csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)


