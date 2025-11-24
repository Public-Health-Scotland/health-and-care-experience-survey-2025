# Written by Catriona Haddow and Martin Leitch
# November 2025.
# 
# *****************************************

#TO DO: #CH note - should be possible to run all of these as a function with lapply
#Inputs: #UPDATE!
#"data/sampling/CHI_Extract_Flagged_Eligible_Patients.parquet",
#"data/sampling/excluded_practices_pre_sample",
#"lookups/Final_Practice_lookup.rds" 

#Outputs: #UPDATE!
#"output/weights/eligible_pats_by_gp.rds"
#"output/weights/gpprac_age_sex_population.rds" 
#"output/weights/gpcl_age_sex_population.rds"
#"output/weights/locality_age_sex_population.rds"
#"output/weights/hscp_age_sex_population.rds"
#"output/weights/hb_age_sex_population.rds"
#"output/weights/ca_age_sex_population.rds"
#"output/weights/scot_age_sex_population.rds"
#"output/weights/ca_locality_age_sex_population.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

# eligible_get_var_names <- read_parquet(paste0(sample_path,"CHI_Extract_Flagged_Eligible_Patients.parquet"), as_data_frame = FALSE) %>%
#    slice_head(n = 1) %>% 
#    compute()
# 
# ls(eligible_get_var_names)

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

#Select 17 and over only. 
table(eligible$age_eligible) #need to output this?

eligible <- eligible %>%  filter(age_eligible== 1)

#Exclude Patients with Non Scottish Address. 
table(eligible$non_scottish,useNA = c("always")) #need to output this?

eligible <- eligible %>% filter(non_scottish == 0)

#read in the list of excluded practices
excluded_practices <- read_xlsx(paste0(data_path,"sampling/excluded_practices.xlsx"))
excluded_practices <- excluded_practices %>%
  select(gp_prac_no) %>%
  mutate(excluded = 1)

#look at excluded practices
eligible <- eligible %>% 
  left_join(excluded_practices,by = c("gp_prac_no"))

#which practices have been excluded
table(eligible$gp_prac_no[eligible$excluded == 1]) #need to output this?
table(eligible$excluded,useNA = c("always"))

#drop excluded practices
eligible <- eligible %>% filter(is.na(excluded))

#Check of number of "eligible" practices
length(unique(eligible$gp_prac_no))

eligible$excluded <- NULL #drop unnecessary variables

#filter missing gp_prac_no
sum(eligible$gp_prac_no == "") # 0 
eligible <- eligible %>% filter(gp_prac_no != "")

#Create eligible patients per GP file for use in Create_weight1.R
eligible_pats_by_gp <- eligible %>%
  group_by(gp_prac_no)%>%
  summarise(eligible_pats = n())

#check if the same as before
hist.file <- readRDS(paste0(weights_path,"eligible_pats_by_gp.rds"))
identical(hist.file,eligible_pats_by_gp)

#save this to directory.
saveRDS(eligible_pats_by_gp,paste0(weights_path,"eligible_pats_by_gp.rds"))

eligible <- eligible %>%
  mutate(age_band_2 = two_age_bands(age), #Add age groups
       age_band_3 = three_age_bands(age),
       age_band_6 = six_age_bands(age))

#match on GP Practice information
practice_info <- readRDS(paste0(lookups_path,"Final_Practice_lookup.rds"))
eligible <- eligible %>%
  left_join(practice_info,by = c("gp_prac_no"))

#2 age groups
gpprac_age_sex_population_2 <- eligible %>%
  group_by('report_area' = gp_prac_no,sex,'age_band' = age_band_2) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gpprac_age_sex_population_2.rds"))
identical(hist.file,gpprac_age_sex_population_2)
saveRDS(gpprac_age_sex_population_2,paste0(weights_path,"gpprac_age_sex_population_2.rds"))

gpcl_age_sex_population <- eligible %>%
  group_by('report_area' = practice_hscp_cluster,sex,'age_band' = age_band_2) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS("output/weights/gpcl_age_sex_population.rds")
identical(hist.file,gpcl_age_sex_population)
saveRDS(gpcl_age_sex_population,paste0(weights_path,"gpcl_age_sex_population.rds"))

locality_age_sex_population <- eligible %>%
  group_by('report_area' = patient_hscp_locality,sex,'age_band' = age_band_2) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_age_sex_population.rds"))
identical(hist.file,locality_age_sex_population)
saveRDS(locality_age_sex_population,paste0(weights_path,"locality_age_sex_population.rds"))

ca_locality_age_sex_population <- eligible %>%
  group_by('report_area' = patient_ca_locality,sex,'age_band' = age_band_2) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_age_sex_population.rds"))
identical(hist.file,ca_locality_age_sex_population)
saveRDS(ca_locality_age_sex_population,paste0(weights_path,"ca_locality_age_sex_population.rds"))
       
#3 age groups
table(eligible$practice_hscp_code,useNA = c("always"))
hscp_age_sex_population <- eligible %>%
  group_by('report_area' = practice_hscp_code,sex,'age_band' = age_band_3) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_age_sex_population.rds"))
identical(hist.file,hscp_age_sex_population)
saveRDS(hscp_age_sex_population,paste0(weights_path,"hscp_age_sex_population.rds"))

hb_age_sex_population <- eligible %>%
  group_by('report_area' = practice_board_code,sex,'age_band' = age_band_3) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hb_age_sex_population.rds"))
identical(hist.file,hb_age_sex_population)
saveRDS(hb_age_sex_population,paste0(weights_path,"hb_age_sex_population.rds"))

ca_age_sex_population <- eligible %>%
  group_by('report_area' = practice_council_code,sex,'age_band' = age_band_3) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_age_sex_population.rds"))
identical(hist.file,ca_age_sex_population)
saveRDS(ca_age_sex_population,paste0(weights_path,"ca_age_sex_population.rds"))

#6 age groups
scot_age_sex_population <- eligible %>%
  group_by('report_area' = "Scotland",sex,'age_band' = age_band_6) %>%
  summarise(eligible_population = n(),.groups = 'drop')
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"scot_age_sex_population.rds"))
identical(hist.file,scot_age_sex_population)
saveRDS(scot_age_sex_population,paste0(weights_path,"scot_age_sex_population.rds"))


