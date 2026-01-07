# WIP: November 2025
# 
# *****************************************

#TO DO: #CH note - should be possible to run all of these as a function with lapply
#Inputs:
#"data/sampling/CHI_Extract_Flagged_Eligible_Patients.parquet", # add where this created? sampling
# data_path,"sampling/excluded_practices.xlsx",# add where this created? sampling
#lookup_path,"practice_lookup.rds"  #created in '...\202526\syntax\sampling\create_final_practice_lookup.R'.

#Outputs:
# weights_path,"eligible_pats_by_gp.rds"
# weights_path,[scot,gpprac,gpcl,locality,hscp,hb,ca,ca_locality]_age_sex_population.rds" 

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

eligible <- read_parquet(paste0(sample_path,"CHI_Extract_Flagged_Eligible_Patients.parquet"),
                     col_select = c("upi_number","gp_prac_no","sex","age","age_eligible","non_scottish","loc_hscp_locality","loc_hscp2019name","loc_ca2019name"))

eligible <- eligible %>% 
  rename_with(tolower)%>% 
  mutate(patient_hscp_locality = paste(loc_hscp2019name,"HSCP -",loc_hscp_locality), #Create variable which combines loc_hscp2019name and loc_hscp_locality - Allows for easier identification for integration indicator tables.
         patient_hscp_locality = paste(loc_hscp2019name,"HSCP -",loc_hscp_locality), #Create variable which combines loc_ca2019name and loc_hscp_locality - Allows for easier identification for integration indicator tables.
         patient_ca_locality = paste(loc_ca2019name,"Local Authority -",loc_hscp_locality))
table(eligible$patient_ca_locality) 

eligible <- eligible %>% 
  mutate(patient_ca_locality = gsub("Na h-Eileanan Siar Local Authority","Na h-Eileanan Siar",patient_ca_locality))#Where patient_ca_locality contains "Na h-Eileanan Siar" drop "Local Authority" from description.
table(eligible$patient_ca_locality)
table(is.na(eligible$loc_hscp2019name),eligible$non_scottish)  #there are scottish addresses with no locality

#Describe exclusions:
exclusions <- eligible %>% #output this file?
  group_by() %>%
  summarise(total = n(),
            age_exclusions = sum(age_eligible == 0),
            non_scottish_exclusions = sum(non_scottish ==1))

eligible <- eligible %>%  
  filter(age_eligible == 1) %>% #Select 17 and over only. 
  filter(non_scottish == 0)  #Exclude Patients with Non Scottish Address. 

#GP practice check
excluded_practices <- read_xlsx(paste0(data_path,"sampling/excluded_practices.xlsx"))

length(eligible$gp_prac_no[eligible$gp_prac_no %in% excluded_practices$gp_prac_no])#check that we have excluded all excluded practices
sum(eligible$gp_prac_no == "") #check that there are no missing GP practice numbers 
length(unique(eligible$gp_prac_no))#Check of number of "eligible" practices

#Create eligible patients per GP file for use in Create_weight1.R
eligible_pats_by_gp <- eligible %>%
  group_by(gp_prac_no)%>%
  summarise(eligible_pats = n())

#check if the same as before
hist.file <- readRDS(paste0(weights_path,"eligible_pats_by_gp.rds"))
all.equal(hist.file,eligible_pats_by_gp)

saveRDS(eligible_pats_by_gp,paste0(weights_path,"eligible_pats_by_gp.rds"))

eligible <- eligible %>%
  mutate(age_band_2 = two_age_bands(age), #Add age groups
       age_band_3 = three_age_bands(age),
       age_band_6 = six_age_bands(age))

#match on GP Practice information
practice_lookup <- readRDS(paste0(lookup_path,"practice_lookup.rds"))
eligible <- eligible %>%
  left_join(practice_lookup,by = c("gp_prac_no"))

#2 age groups
gpprac_age_sex_population_2 <- eligible %>%
  group_by('report_area' = gp_prac_no,sex,'age_band' = age_band_2) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gpprac_age_sex_population_2.rds"))
all.equal(hist.file,gpprac_age_sex_population_2)
saveRDS(gpprac_age_sex_population_2,paste0(weights_path,"gpprac_age_sex_population_2.rds"))

gpcl_age_sex_population <- eligible %>%
  group_by('report_area' = practice_hscp_cluster,sex,'age_band' = age_band_2) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS("output/weights/gpcl_age_sex_population.rds")
all.equal(hist.file,gpcl_age_sex_population)
saveRDS(gpcl_age_sex_population,paste0(weights_path,"gpcl_age_sex_population.rds"))

locality_age_sex_population <- eligible %>%
  group_by('report_area' = patient_hscp_locality,sex,'age_band' = age_band_2) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_age_sex_population.rds"))
all.equal(hist.file,locality_age_sex_population)
saveRDS(locality_age_sex_population,paste0(weights_path,"locality_age_sex_population.rds"))

ca_locality_age_sex_population <- eligible %>%
  group_by('report_area' = patient_ca_locality,sex,'age_band' = age_band_2) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_age_sex_population.rds"))
all.equal(hist.file,ca_locality_age_sex_population)
saveRDS(ca_locality_age_sex_population,paste0(weights_path,"ca_locality_age_sex_population.rds"))
       
#3 age groups
table(eligible$practice_hscp_code,useNA = c("always"))
hscp_age_sex_population <- eligible %>%
  group_by('report_area' = practice_hscp_code,sex,'age_band' = age_band_3) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_age_sex_population.rds"))
all.equal(hist.file,hscp_age_sex_population)
saveRDS(hscp_age_sex_population,paste0(weights_path,"hscp_age_sex_population.rds"))

hb_age_sex_population <- eligible %>%
  group_by('report_area' = practice_board_code,sex,'age_band' = age_band_3) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hb_age_sex_population.rds"))
all.equal(hist.file,hb_age_sex_population)
saveRDS(hb_age_sex_population,paste0(weights_path,"hb_age_sex_population.rds"))

ca_age_sex_population <- eligible %>%
  group_by('report_area' = practice_council_code,sex,'age_band' = age_band_3) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_age_sex_population.rds"))
all.equal(hist.file,ca_age_sex_population)
saveRDS(ca_age_sex_population,paste0(weights_path,"ca_age_sex_population.rds"))

#6 age groups
scot_age_sex_population <- eligible %>%
  group_by('report_area' = "Scotland",sex,'age_band' = age_band_6) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"scot_age_sex_population.rds"))
all.equal(hist.file,scot_age_sex_population)
saveRDS(scot_age_sex_population,paste0(weights_path,"scot_age_sex_population.rds"))


