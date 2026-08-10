
## This script is for taking chemical data from the Freshwater Research Lab, Flathead Lake Biological Station (converted to .csv) and splitting the data into different projects

#0. load packages----

library(tidyverse) 
library(dplyr)
library(stringr)

#scientific notation 

options(scipen = 100)

#read in data----

chemdata_2022 <- read.csv("C:\\Users\\13603\\Documents\\PhD_dell\\dissertation\\thesis\\GNP_includes_chp_3_paleo\\data_analysis\\LakeMcDonaldNutrientBudget\\0_data\\2022\\lab_data\\GNP_LM_tribs_BW_SM_4_28_2022_9_20_2022_complete.csv", header = T, na.strings = (""))

chemdata_2023 <- read.csv("C:\\Users\\13603\\Documents\\PhD_dell\\dissertation\\thesis\\GNP_includes_chp_3_paleo\\data_analysis\\LakeMcDonaldNutrientBudget\\0_data\\2023\\lab_data\\Bannerman_2023.csv", header = T, na.strings = (""))

head(chemdata_2022)
head(chemdata_2023)
dim(chemdata_2022)
dim(chemdata_2023)


#1. Clean up data----

# Remove detection limits and units

chemdata_2022 <- chemdata_2022[,c(1:3, 5:16)]
head(chemdata_2022)

chemdata_2023 <- chemdata_2023[,c(1:3, 5:16)]
head(chemdata_2023)

# Rename columns

colnames(chemdata_2022) <- c("lims", "id", "date",  "nh3", "chl", "doc", "no3no2", "pc", "pn", "pp", "srp", "tdn", "tdp", "tn", "tp")

head(chemdata_2022)

colnames(chemdata_2023) <- c("lims", "id", "date",  "nh3", "chl", "doc", "no3no2", "pc", "pn", "pp", "srp", "tdn", "tdp", "tn", "tp")

head(chemdata_2023)

# Remove unnecessary rows; units and mdls + other sites

chemmdl_2022 <- chemdata_2022[1:2,]
head(chemmdl_2022)
dim(chemdata_2022)

chemdata_2022 <- chemdata_2022[3:219,]
head(chemdata_2022)

chemmdl_2023 <- chemdata_2023[1:2,]
head(chemmdl_2023)
dim(chemdata_2023)

chemdata_2023 <- chemdata_2023[3:111,]
head(chemdata_2023)

chemdata_2023 <- subset(chemdata_2023, id == 'UMC_footbridge_A' | id == 'UMC_footbridge_B' | id == 'UMC_down_A' | id == 'UMC_down_B'| id == 'UMC_A' | id == 'UMC_B' | id == 'LMC_A' | id == 'LMC_B' | id == 'LM10_A' | id == 'LM10_B'| id == 'LM5i_A' | id == 'LM5i_B' | id == 'AG_A' | id == 'AG_B'| id == 'GN_A' | id == 'GN_B' |id == 'GS_A' | id == 'GS_B' |id == 'KLN_A' | id == 'KLN_B' |id == 'KLS_A' | id == 'KLS_B' |id == 'LN_A' | id == 'LN_B' | id == 'LS_A' | id == 'LS_B' | id == 'MLN_A' | id == 'MLN_B' | id == 'RS_A' | id == 'RS_B')


# Check data structure
str(chemdata_2022)
str(chemdata_2023)

# Convert concentration data from character to numeric objects
chemdata_2022 <- transform(chemdata_2022, nh3 = as.numeric(nh3))
chemdata_2022 <- transform(chemdata_2022, chl = as.numeric(chl))
chemdata_2022 <- transform(chemdata_2022, doc = as.numeric(doc))
chemdata_2022 <- transform(chemdata_2022, no3no2 = as.numeric(no3no2))
chemdata_2022 <- transform(chemdata_2022, pc = as.numeric(pc))
chemdata_2022 <- transform(chemdata_2022, pn = as.numeric(pn))
chemdata_2022 <- transform(chemdata_2022, pp = as.numeric(pp))
chemdata_2022 <- transform(chemdata_2022, srp = as.numeric(srp))
chemdata_2022 <- transform(chemdata_2022, tdn = as.numeric(tdn))
chemdata_2022 <- transform(chemdata_2022, tdp = as.numeric(tdp))
chemdata_2022 <- transform(chemdata_2022, tn = as.numeric(tn))
chemdata_2022 <- transform(chemdata_2022, tp = as.numeric(tp))

chemdata_2023 <- transform(chemdata_2023, nh3 = as.numeric(nh3))
chemdata_2023 <- transform(chemdata_2023, chl = as.numeric(chl))
chemdata_2023 <- transform(chemdata_2023, doc = as.numeric(doc))
chemdata_2023 <- transform(chemdata_2023, no3no2 = as.numeric(no3no2))
chemdata_2023 <- transform(chemdata_2023, pc = as.numeric(pc))
chemdata_2023 <- transform(chemdata_2023, pn = as.numeric(pn))
chemdata_2023 <- transform(chemdata_2023, pp = as.numeric(pp))
chemdata_2023 <- transform(chemdata_2023, srp = as.numeric(srp))
chemdata_2023 <- transform(chemdata_2023, tdn = as.numeric(tdn))
chemdata_2023 <- transform(chemdata_2023, tdp = as.numeric(tdp))
chemdata_2023 <- transform(chemdata_2023, tn = as.numeric(tn))
chemdata_2023 <- transform(chemdata_2023, tp = as.numeric(tp))

# Convert date from character to date
chemdata_2022 <- transform(chemdata_2022, date = as.Date(date, "%m/%d/%Y"))
chemdata_2023 <- transform(chemdata_2023, date = as.Date(date, "%m/%d/%Y"))

# Fix typos
chemdata_2022 <- chemdata_2022 %>% 
  mutate(id = str_replace(id, "LM_10_C", "LM10_C")) %>% 
  mutate(id = str_replace(id, "Spague", "Sprague"))
chemdata_2022


# Create new columns that specify site and replicate
chemdata_2022[c('site', 'rep')] <- str_split_fixed(chemdata_2022$id, '_', 2)
str(chemdata_2022)

chemdata_2023[c('site', 'rep')] <- str_split_fixed(chemdata_2023$id, '_', 2)
str(chemdata_2023)


# Convert pc, pn, and pp as a concentration (0.3 L filtered)

chemdata_2022 <- chemdata_2022 %>%
  mutate(pc= pc/.3)%>%
  mutate(pn= pn/.3)%>%
  mutate(pp= pp/.3)

chemdata_2023 <- chemdata_2023 %>%
  mutate(pc= pc/.3)%>%
  mutate(pn= pn/.3)%>%
  mutate(pp= pp/.3)

# Remove field ecology data plus Bowman and St Mary lake data
chemdata_2022<-chemdata_2022[!(chemdata_2022$site=="Field" | chemdata_2022$site=="BW10" | chemdata_2022$site=="BW5i" | chemdata_2022$site=="SM10" | chemdata_2022$site=="SM5i"),]

summary(chemdata_2022)
summary(chemdata_2023)

# Write files
write.table(chemdata_2022,file='C:\\Users\\13603\\Documents\\PhD_dell\\dissertation\\thesis\\GNP_includes_chp_3_paleo\\data_analysis\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2022.csv', sep = ",")

write.table(chemdata_2023,file='C:\\Users\\13603\\Documents\\PhD_dell\\dissertation\\thesis\\GNP_includes_chp_3_paleo\\data_analysis\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2023.csv', sep = ",")

