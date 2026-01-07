# *****************************************
# January 2025 WIP.

#Purpose: Creates responses_with_categories file with age bands and sections. Calculates non-response weights (weight 2)

#TO DO:
#Check section definitions
#We now have in section categories Y, N, and NA - what impact does this have? this does have an impact - have reverted to Y and N only

#Inputs: 
#data/Results/data_Validated results.rds #created in 01.validation.R
#lookup_path,"patientID_info.rds" #created in 02.create_patient_info_files_from_sample_pop.R
#lookup_path,"practice_lookup.rds" #created in '...\202526\syntax\sampling\create_final_practice_lookup.R'.
#weights_path,weight1.rds" #created in 04.create_weight1
#weights_path,[scot,hb,hscp,gpcl,gpprac,ca,locality,ca_locality]_age_sex_population.rds" #created in 03.create_reference_files_from_eligible_pop
#lookup_path,pcis_rates_[2,3,6]_agebands.rds #created in 00.reformat_PCIS_rates

#Outputs: 
#analysis_output_path,"responses_with_categories.rds"
#weights_path,[nat,hb,hscp,gpcl,gp,ca,locality,ca_locality]_weights.rds

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#Read in responses
validated_results <- readRDS(paste0(data_path,"results/data_Validated_results.rds"))

#Add on patient information
patientID_info <- readRDS(paste0(lookup_path,"patientID_info.rds"))
validated_results <- validated_results %>% 
  left_join(patientID_info %>% select(patientid,sex,age,gp_prac_no,patient_hscp_locality,patient_ca_locality),by = c("patientid"))
rm(patientID_info)

#Add on GP practice information
practice_lookup <- readRDS(paste0(lookup_path,"practice_lookup.rds"))
validated_results <- validated_results %>% 
  left_join(practice_lookup %>% select(gp_prac_no,practice_hscp_cluster,practice_hscp_code,practice_board_code,practice_council_code), by = c("gp_prac_no"))
rm(practice_lookup)

validated_results <- validated_results %>% 
  mutate(age_band_2 = two_age_bands(age), #Add age groups
         age_band_3 = three_age_bands(age),
         age_band_6 = six_age_bands(age))

#Add markers to indicate which respondents are in each weighting population####

#s1  - GP users  (weight for questions 2-17b)
#s2  - OOH users  (weight for Questions 20a to 25)
#s3  - Social care users (weight for Questions 28a-31)
#s4  - Carers (weight for Questions 34a to 37e)
#s5  - Whole population (weight for Questions 1,19,27a,27b,27c,27d,27e,27f,27g,27h,33,38)
#s6  - Those in need of social care (weight for Questions 32a,32b,32c,32d,32e,32f,32g,32h,32i)

validated_results <- validated_results %>%
  mutate('s1' = case_when(q01 == "1" ~ "Y", TRUE ~ "N"),
         's2' = case_when(q19 == "1" ~ "Y", TRUE ~ "N"),
         's3' = case_when(q27a =="1"|q27b =="1"|q27c=="1"|q27d=="1"|q27e=="1"|q27f=="1" ~ "Y",TRUE ~ "N"),
         's4' = case_when(q33 %in% c("1","2","3","4","5") ~ "Y", TRUE ~ "N"),
         's5' = "Y",
         's6' = case_when(q27a =="1"|q27b =="1"|q27c=="1"|q27d=="1"|q27e=="1"|q27f=="1"|q27g=="1" ~ "Y",TRUE ~ "N"))

#Read in weight 1 and match on. This is used to estimate the population accounted for by the respondents
weight1 <- readRDS(paste0(weights_path,"weight1.rds"))
responses_with_categories <- left_join(validated_results,weight1,by = c("gp_prac_no"))
rm(weight1)

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"responses_with_categories.rds"))
all.equal(hist.file,responses_with_categories)
saveRDS(responses_with_categories,paste0(analysis_output_path,"responses_with_categories.rds"))

#Save out version for SG
#responses_with_categories_for_SG <- responses_with_categories %>%
#  select(-qh_psid, -patientid, -age, -patient_hscp_locality, -patient_ca_locality)

#saveRDS(responses_with_categories_for_SG,paste0(analysis_output_path,"responses_with_categories_for_SG.rds"))

rm(hist.file,responses_with_categories,validated_results)

#Calculate weight 2
#Aggregate responses by section sex / age group category
#Scot cat is the sex / age group category; S1_Wt2_Cat is split by whether in or not in that section's sex / age group category  
#     -calculate proportion in or not in each section category. This is either done using PTI info or the answer to the routing question
#     -calculate the respondent population for each section category
#Cat_population is the eligible population in this sex / age group category
#     -use the eligible population to estimate the population who are in / not in the population of interest.
#Weight 2 is the eligible population / respondent population

####Weights function for Section 1####
#: GP Use. Uses the number of people in the population using their GP Practice, estimated using the number of people consulting their GP or practice nurse from the 2012/13 Practice Team Information (PTI) Statistics

make_weights_s1 <- function(df) {
  weight2_s1 <- df %>%
    group_by(report_area,age_band,sex,s1) %>%
    summarise(respondent_pop =sum(gp_wt1),
              resp_by_section_cat = n(),.groups = "keep")%>%
    left_join(area_age_sex_population,by = c("report_area","age_band","sex")) %>%
    left_join(pcis_rates,by = c("age_band","sex","s1")) %>%
    mutate(cat_population = pcis_per * eligible_population,
           weight = cat_population/respondent_pop)%>%
    ungroup %>%
   select(report_area,age_band,sex,section_category = s1,respondent_pop,resp_by_section_cat,pcis_per,eligible_population,cat_population,weight)
  return(weight2_s1)
}

####Weights function for Sections  2 plus####
#Make weight function
make_weights <- function (x){responses_temp %>%
    mutate(section_category = get(x)) %>% 
    group_by(report_area,age_band,sex,section_category) %>%
    summarise(respondent_pop =sum(gp_wt1),
              resp_by_section_cat = n(),.groups = "keep")%>%
    group_by(report_area,age_band,sex) %>%
    mutate(resp = sum(resp_by_section_cat),
           cat_percent = resp_by_section_cat/resp) %>%
    left_join(area_age_sex_population,by = c("report_area","age_band","sex")) %>%
    mutate(cat_population = cat_percent * eligible_population,
           weight = cat_population/respondent_pop) %>%
    ungroup %>%
    select(-resp)
}

responses <- readRDS(paste0(analysis_output_path,"responses_with_categories.rds"))

#define vector for adding weights for sections 2 to 6
sections2to6<- c("s2","s3","s4","s5","s6")

#National weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"scot_age_sex_population.rds"))

#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_6_agebands.rds"))

#apply weight categories for this level of reporting
responses_temp <- responses %>%  mutate(report_area = "Scotland",age_band = age_band_6)

#run weight functions
nat_weights <- append(list(make_weights_s1(responses_temp)), #run function for section 1 weight
                       lapply(sections2to6,make_weights)) #run function to add weights for sections 2 to 6 
names(nat_weights) <- c("s1",sections2to6) #add appropriate names

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(weights_path,"nat_weights.rds"))
all.equal(hist.file,nat_weights)
saveRDS(nat_weights,paste0(weights_path,"nat_weights.rds"))

#NHS Board weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"hb_age_sex_population.rds"))

#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_3_agebands.rds"))

#apply weight categories for this level of reporting
responses_temp <- responses %>%  mutate(report_area = practice_board_code,age_band = age_band_3)

#run weight functions
hb_weights <- append(list(make_weights_s1(responses_temp)), #run function for section 1 weight
                       lapply(sections2to6,make_weights))#run function to add weights for sections 2 to 6 
names(hb_weights) <- c("s1",sections2to6) #add appropriate names

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(weights_path,"hb_weights.rds"))
all.equal(hist.file,hb_weights)
saveRDS(hb_weights,paste0(weights_path,"hb_weights.rds"))

#HSCP weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"hscp_age_sex_population.rds"))
#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_3_agebands.rds"))

#apply weight categories for this level of reporting
responses_temp <- responses %>% mutate(report_area = practice_hscp_code,age_band = age_band_3)

#run weight functions
hscp_weights <- append(list(make_weights_s1(responses_temp)), #run function for section 1 weight
                       lapply(sections2to6,make_weights))#run function to add weights for sections 2 to 6 
names(hscp_weights) <- c("s1",sections2to6) #add appropriate names

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_weights.rds"))
all.equal(hist.file,hscp_weights)
saveRDS(hscp_weights,paste0(weights_path,"hscp_weights.rds"))

#GP Cluster weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"gpcl_age_sex_population.rds"))

#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_2_agebands.rds"))

#apply weight categories for this level of reporting
table(responses$practice_hscp_cluster,useNA = c("always"))
responses_temp <- responses %>% mutate(report_area = practice_hscp_cluster,age_band = age_band_2)

#run weight functions
gpcl_weights <- append(list(make_weights_s1(responses_temp)), #run function for section 1 weight
                       lapply(sections2to6,make_weights))#run function to add weights for sections 2 to 6         
names(gpcl_weights) <- c("s1",sections2to6) #add appropriate names

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(weights_path,"gpcl_weights.rds"))
all.equal(hist.file,gpcl_weights)
saveRDS(gpcl_weights,paste0(weights_path,"gpcl_weights.rds"))

#GP weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"gpprac_age_sex_population_2.rds"))

#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_2_agebands.rds"))

#apply weight categories for this level of reporting
responses_temp <- responses %>% mutate(report_area = gp_prac_no,age_band = age_band_2)

#run weight functions
gp_weights <- append(list(make_weights_s1(responses_temp)), #run function for section 1 weight
                       lapply(sections2to6,make_weights))#run function to add weights for sections 2 to 6   
names(gp_weights) <- c("s1",sections2to6) #add appropriate names

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(weights_path,"gp_weights.rds"))
all.equal(hist.file,gp_weights)
saveRDS(gp_weights,paste0(weights_path,"gp_weights.rds"))

#CA weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"ca_age_sex_population.rds"))

#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_3_agebands.rds"))

#apply weight categories for this level of reporting
table(responses$practice_council_code,useNA = c("always"))
responses_temp <- responses %>% mutate(report_area = practice_council_code,age_band = age_band_3)

#run weight functions
ca_weights <- append(list(make_weights_s1(responses_temp)), #run function for section 1 weight
                     lapply(sections2to6,make_weights))#run function to add weights for sections 2 to 6  
names(ca_weights) <- c("s1",sections2to6) #add appropriate names

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_weights.rds"))
all.equal(hist.file,ca_weights)
saveRDS(ca_weights,paste0(weights_path,"ca_weights.rds"))

#Locality weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"locality_age_sex_population.rds"))

#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_2_agebands.rds"))

#apply weight categories for this level of reporting
table(responses$patient_hscp_locality,useNA = c("always"))
responses_temp <- responses %>% mutate(report_area = patient_hscp_locality,age_band = age_band_2)

#run weight functions
locality_weights <- append(list(make_weights_s1(responses_temp)), #run function for section 1 weight
                     lapply(sections2to6,make_weights))#run function to add weights for sections 2 to 6   
names(locality_weights) <- c("s1",sections2to6) #add appropriate names

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_weights.rds"))
all.equal(hist.file,locality_weights)
saveRDS(locality_weights,paste0(weights_path,"locality_weights.rds"))

#ca_Locality weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"ca_locality_age_sex_population.rds"))

#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_2_agebands.rds"))

#apply weight categories for this level of reporting
table(responses$patient_ca_locality,useNA = c("always"))
responses_temp <- responses %>% mutate(report_area = patient_ca_locality,age_band = age_band_2)

#run weight functions
ca_locality_weights <- append(list(make_weights_s1(responses_temp)), #run function for section 1 weight
                                 lapply(sections2to6,make_weights))#run function to add weights for sections 2 to 6   
names(ca_locality_weights) <- c("s1",sections2to6) #add appropriate names

#Save out, first checking if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_weights.rds"))
all.equal(hist.file,ca_locality_weights)
saveRDS(ca_locality_weights,paste0(weights_path,"ca_locality_weights.rds"))