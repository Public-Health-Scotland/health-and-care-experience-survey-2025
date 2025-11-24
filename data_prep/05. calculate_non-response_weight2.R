# Written by Catriona Haddow and Martin Leitch
# November 2025.

# *****************************************
#Purpose: Creates responses_with_categories file with age bands and sections. Calculates non-response weights (weight 2)

#TO DO:
#We now have in section categories Y, N, and NA - what impact does this have? this does have an impact - have reverted to Y and N only

#Inputs: #UDPATE!
#"data/Results/data_Validated results.rds"
#"lookups/patientID_info.rds"
#"lookups/Final_Practice_lookup.sav" (updated ML 30/01/24)
#paste0(weights_path,"weight1.rds"
#paste0(weights_path,"scot_age_sex_population.rds"
#paste0(lookup_path,"pcis_rates_6_agebands.rds"
#"output/weights/hb_age_sex_population.rds"
#"lookups/pti_rates_3_agebands.rds"
#"output/weights/hscp_age_sex_population.rds"
#"output/weights/gpcl_age_sex_population.rds"
#"lookups/pti_rates_2_agebands.rds"
#"output/weights/gpprac_age_sex_population_2.rds"
#"output/weights/ca_age_sex_population.rds"
#"output/weights/locality_age_sex_population.rds"
#"output/weights/ca_locality_age_sex_population.rds"))

#Outputs: #UDPATE!
#"output/analysis_output/responses_with_categories.rds"
#"output/weights/nat_s1_weights.rds - nat_s6_weights.rds"
#"output/weights/nat_s1_weights.csv - nat_s6_weights.csv"
#"output/weights/hb_s1_weights.rds - hb_s6_weights.rds"
#"output/weights/hb_s1_weights.csv - hb_s6_weights.csv"
#"output/weights/hscp_s1_weights.rds - hscp_s6_weights.rds"
#"output/weights/hscp_s1_weights.csv - hscp_s6_weights.csv"
#"output/weights/gpcl_s1_weights.rds - gpcl_s6_weights.rds"
#"output/weights/gpcl_s1_weights.csv - gpcl_s6_weights.csv"
#"output/weights/gp_s1_weights.rds - gp_s6_weights.rds"
#"output/weights/gp_s1_weights.csv - gp_s6_weights.csv"
#"output/weights/ca_s1_weights.rds - ca_s6_weights.rds"
#"output/weights/ca_s1_weights.csv - ca_s6_weights.csv"
#"output/weights/locality_s1_weights.rds - locality_s6_weights.rds"
#"output/weights/locality_s1_weights.csv - locality_s6_weights.csv"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#Read in responses
validated_results <- readRDS(paste0(data_path,"results/data_Validated_results.rds"))

#Add on patient information
patientID_info <- readRDS(paste0(lookup_path_202324,"patientID_info.rds"))
validated_results <- validated_results %>% 
  left_join(patientID_info %>% select(patientid,sex,age,gp_prac_no,patient_hscp_locality,patient_ca_locality),by = c("patientid"))
rm(patientID_info)

#Add on GP practice information
practice_lookup <- readRDS(paste0(lookup_path_202324,"Final_Practice_lookup.rds"))
validated_results <- validated_results %>% 
  left_join(practice_lookup %>% select(gp_prac_no,practice_hscp_cluster,practice_hscp_code,practice_board_code,practice_council_code), by = c("gp_prac_no"))
rm(practice_lookup)

validated_results <- validated_results %>% 
  mutate(age_band_2 = two_age_bands(age), #Add age groups
         age_band_3 = three_age_bands(age),
         age_band_6 = six_age_bands(age))

#Add markers to indicate which respondents are in each weighting population

#s1  - GP users  (weight for questions 2-17b)####
#s2  - OOH users  (weight for Questions 20a to 25)####
#s3  - Social care users (weight for Questions 28a-31)####
#s4  - Carers (weight for Questions 34a to 37e)####
#s5  - Whole population (weight for Questions 1,19,27a,27b,27c,27d,27e,27f,27g,27h,33,38)####
#s6  - Those in need of social care (weight for new Questions 32a,32b,32c,32d,32e,32f,32g,32h,32i)####

validated_results <- validated_results %>%
  mutate('s1' = case_when(q01 == 1 ~ "Y", TRUE ~ "N"),
         's2' = case_when(q19 == 1 ~ "Y", TRUE ~ "N"),
         's3' = case_when(q27a ==1|q27b ==1|q27c==1|q27d==1|q27e==1|q27f==1 ~ "Y",TRUE ~ "N"),
         's4' = case_when(q33 %in% c(1,2,3,4,5) ~ "Y", TRUE ~ "N"),
         's5' = "Y",
         's6' = case_when(q27a ==1|q27b ==1|q27c==1|q27d==1|q27e==1|q27f==1|q27g==1 ~ "Y",TRUE ~ "N"))

#Read in weight 1 and match on. This is used to estimate the population accounted for by the respondents
weight1 <- readRDS(paste0(weights_path_202324,"weight1.rds"))
responses_with_categories <- left_join(validated_results,weight1,by = c("gp_prac_no"))
rm(weight1)

#check if the same as before
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
    left_join(pti_rates,by = c("age_band","sex","s1")) %>%
    mutate(cat_population = pti_per * eligible_population,
           weight = cat_population/respondent_pop)%>%
    ungroup %>%
   select(report_area,age_band,sex,s1,respondent_pop,resp_by_section_cat,pti_per,eligible_population,cat_population,weight)
  return(weight2_s1)
}

####Weights function for Sections  2 plus####
#Make weight function
make_weights <- function(df, section_category) {
  Weight2 <- df %>%
    group_by(report_area,age_band,sex,{{section_category}}) %>%
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
  return(Weight2)
}

responses <- readRDS(paste0(analysis_output_path,"responses_with_categories.rds"))

#National weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path_202324,"scot_age_sex_population.rds"))

# #Read in the PTI information, updated using syntax 'reformat_PTI_rates.R'
pti_rates <- readRDS(paste0(lookup_path_202324,"pti_rates_6_agebands.rds"))

#Read in the PCIS information, prepared using script '00. reformat_PCIS_rates.R'
# pcis_rates <- readRDS(paste0(lookup_path,"pcis_rates_6_agebands.rds"))
#apply weight categories
responses_temp <- responses %>%  mutate(report_area = "Scotland",age_band = age_band_6)

nat_s1_weights <- make_weights_s1(responses_temp)
#check if the same as before
hist.file <- readRDS(paste0(weights_path_202324,"nat_s1_weights.rds"))
identical(hist.file,nat_s1_weights)
write.table(nat_s1_weights,paste0(weights_path,"nat_s1_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(nat_s1_weights,paste0(weights_path,"nat_s1_weights.rds"))

nat_s2_weights <- make_weights(responses_temp,s2)
#check if the same as before
hist.file <- readRDS(paste0(weights_path_202324,"nat_s2_weights.rds"))
identical(hist.file,nat_s2_weights)
write.table(nat_s2_weights,paste0(weights_path,"nat_s2_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(nat_s2_weights,paste0(weights_path,"nat_s2_weights.rds"))

nat_s3_weights <- make_weights(responses_temp,s3)
#check if the same as before
hist.file <- readRDS(paste0(weights_path_202324,"nat_s3_weights.rds"))
identical(hist.file,nat_s3_weights)
write.table(nat_s3_weights,paste0(weights_path,"nat_s3_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(nat_s3_weights,paste0(weights_path,"nat_s3_weights.rds"))

nat_s4_weights <- make_weights(responses_temp,s4)
#check if the same as before
hist.file <- readRDS(paste0(weights_path_202324,"nat_s4_weights.rds"))
identical(hist.file,nat_s4_weights)
write.table(nat_s4_weights,paste0(weights_path,"nat_s4_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(nat_s4_weights,paste0(weights_path,"nat_s4_weights.rds"))

nat_s5_weights <- make_weights(responses_temp,s5)
#check if the same as before
hist.file <- readRDS(paste0(weights_path_202324,"nat_s5_weights.rds"))
identical(hist.file,nat_s5_weights)
write.table(nat_s5_weights,paste0(weights_path,"nat_s5_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(nat_s5_weights,paste0(weights_path,"nat_s5_weights.rds"))

nat_s6_weights <- make_weights(responses_temp,s6)
#check if the same as before
hist.file <- readRDS(paste0(weights_path_202324,"nat_s6_weights.rds"))
identical(hist.file,nat_s6_weights)
write.table(nat_s6_weights,paste0(weights_path,"nat_s6_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(nat_s6_weights,paste0(weights_path,"nat_s6_weights.rds"))

rm(responses_temp,nat_s1_weights,nat_s2_weights,nat_s3_weights,nat_s4_weights,nat_s5_weights,
      nat_s6_weights)

#NHS Board weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path_202324,"hb_age_sex_population.rds"))
#Read in the PTI information, updated using syntax 'reformat_PTI_rates.R'
pti_rates <- readRDS(paste0(lookup_path_202324,"pti_rates_3_agebands.rds"))

#apply weight categories
responses_temp <- responses %>%  mutate(report_area = practice_board_code,age_band = age_band_3)

hb_s1_weights <- make_weights_s1(responses_temp)
#check if the same as before
hist.file <- readRDS(paste0(weights_path_202324,"hb_s1_weights.rds"))
identical(hist.file,hb_s1_weights)
write.table(hb_s1_weights,paste0(weights_path,"hb_s1_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hb_s1_weights,paste0(weights_path,"hb_s1_weights.rds"))

hb_s2_weights <- make_weights(responses_temp,s2)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hb_s2_weights.rds"))
identical(hist.file,hb_s2_weights)
write.table(hb_s2_weights,paste0(weights_path,"hb_s2_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hb_s2_weights,paste0(weights_path,"hb_s2_weights.rds"))

hb_s3_weights <- make_weights(responses_temp,s3)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hb_s3_weights.rds"))
identical(hist.file,hb_s3_weights)
write.table(hb_s3_weights,paste0(weights_path,"hb_s3_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hb_s3_weights,paste0(weights_path,"hb_s3_weights.rds"))

hb_s4_weights <- make_weights(responses_temp,s4)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hb_s4_weights.rds"))
identical(hist.file,hb_s4_weights)
write.table(hb_s4_weights,paste0(weights_path,"hb_s4_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hb_s4_weights,paste0(weights_path,"hb_s4_weights.rds"))

hb_s5_weights <- make_weights(responses_temp,s5)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hb_s5_weights.rds"))
identical(hist.file,hb_s5_weights)
write.table(hb_s5_weights,paste0(weights_path,"hb_s5_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hb_s5_weights,paste0(weights_path,"hb_s5_weights.rds"))

hb_s6_weights <- make_weights(responses_temp,s6)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hb_s6_weights.rds"))
identical(hist.file,hb_s6_weights)
write.table(hb_s6_weights,paste0(weights_path,"hb_s6_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hb_s6_weights,paste0(weights_path,"hb_s6_weights.rds"))

rm(responses_temp,hb_s1_weights,hb_s2_weights,hb_s3_weights,hb_s4_weights,hb_s5_weights,
   hb_s6_weights)

#HSCP weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"hscp_age_sex_population.rds"))
#Read in the PTI information, updated using syntax 'reformat_PTI_rates.R'
pti_rates <- readRDS(paste0(lookup_path,"pcis_rates_3_agebands.rds"))

#apply weight categories
responses_temp <- responses %>% mutate(report_area = practice_hscp_code,age_band = age_band_3)

hscp_s1_weights <- make_weights_s1(responses_temp)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_s1_weights.rds"))
identical(hist.file,hscp_s1_weights)
write.table(hscp_s1_weights,paste0(weights_path,"hscp_s1_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hscp_s1_weights,paste0(weights_path,"hscp_s1_weights.rds"))

hscp_s2_weights <- make_weights(responses_temp,s2)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_s2_weights.rds"))
identical(hist.file,hscp_s2_weights)
write.table(hscp_s2_weights,paste0(weights_path,"hscp_s2_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hscp_s2_weights,paste0(weights_path,"hscp_s2_weights.rds"))

hscp_s3_weights <- make_weights(responses_temp,s3)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_s3_weights.rds"))
identical(hist.file,hscp_s3_weights)
write.table(hscp_s3_weights,paste0(weights_path,"hscp_s3_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hscp_s3_weights,paste0(weights_path,"hscp_s3_weights.rds"))

hscp_s4_weights <- make_weights(responses_temp,s4)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_s4_weights.rds"))
identical(hist.file,hscp_s4_weights)
write.table(hscp_s4_weights,paste0(weights_path,"hscp_s4_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hscp_s4_weights,paste0(weights_path,"hscp_s4_weights.rds"))

hscp_s5_weights <- make_weights(responses_temp,s5)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_s5_weights.rds"))
identical(hist.file,hscp_s5_weights)
write.table(hscp_s5_weights,paste0(weights_path,"hscp_s5_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hscp_s5_weights,paste0(weights_path,"hscp_s5_weights.rds"))

hscp_s6_weights <- make_weights(responses_temp,s6)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"hscp_s6_weights.rds"))
identical(hist.file,hscp_s6_weights)
write.table(hscp_s6_weights,paste0(weights_path,"hscp_s6_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(hscp_s6_weights,paste0(weights_path,"hscp_s6_weights.rds"))

rm(responses_temp,hscp_s1_weights,hscp_s2_weights,hscp_s3_weights,hscp_s4_weights,hscp_s5_weights,
   hscp_s6_weights)

#GP Cluster weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"gpcl_age_sex_population.rds"))
#Read in the PTI information, updated using syntax 'reformat_PTI_rates.R'
pti_rates <- readRDS(paste0(lookup_path,"pcis_rates_2_agebands.rds"))

#apply weight categories
table(responses$practice_hscp_cluster,useNA = c("always"))
responses_temp <- responses %>% mutate(report_area = practice_hscp_cluster,age_band = age_band_2)
         
gpcl_s1_weights <- make_weights_s1(responses_temp)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gpcl_s1_weights.rds"))
identical(hist.file,gpcl_s1_weights)
write.table(gpcl_s1_weights,paste0(weights_path,"gpcl_s1_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gpcl_s1_weights,paste0(weights_path,"gpcl_s1_weights.rds"))

gpcl_s2_weights <- make_weights(responses_temp,s2)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gpcl_s2_weights.rds"))
identical(hist.file,gpcl_s2_weights)
write.table(gpcl_s2_weights,paste0(weights_path,"gpcl_s2_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gpcl_s2_weights,paste0(weights_path,"gpcl_s2_weights.rds"))

gpcl_s3_weights <- make_weights(responses_temp,s3)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gpcl_s3_weights.rds"))
identical(hist.file,gpcl_s3_weights)
write.table(gpcl_s3_weights,paste0(weights_path,"gpcl_s3_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gpcl_s3_weights,paste0(weights_path,"gpcl_s3_weights.rds"))

gpcl_s4_weights <- make_weights(responses_temp,s4)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gpcl_s4_weights.rds"))
identical(hist.file,gpcl_s4_weights)
write.table(gpcl_s4_weights,paste0(weights_path,"gpcl_s4_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gpcl_s4_weights,paste0(weights_path,"gpcl_s4_weights.rds"))

gpcl_s5_weights <- make_weights(responses_temp,s5)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gpcl_s5_weights.rds"))
identical(hist.file,gpcl_s5_weights)
write.table(gpcl_s5_weights,paste0(weights_path,"gpcl_s5_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gpcl_s5_weights,paste0(weights_path,"gpcl_s5_weights.rds"))

gpcl_s6_weights <- make_weights(responses_temp,s6)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gpcl_s6_weights.rds"))
identical(hist.file,gpcl_s6_weights)
write.table(gpcl_s6_weights,paste0(weights_path,"gpcl_s6_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gpcl_s6_weights,paste0(weights_path,"gpcl_s6_weights.rds"))

rm(responses_temp,gpcl_s1_weights,gpcl_s2_weights,gpcl_s3_weights,gpcl_s4_weights,gpcl_s5_weights,
   gpcl_s6_weights)

#GP weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"gpprac_age_sex_population_2.rds"))
#Read in the PTI information, updated using syntax 'reformat_PTI_rates.R'
pti_rates <- readRDS(paste0(lookup_path,"pcis_rates_2_agebands.rds"))

#apply weight categories
responses_temp <- responses %>% mutate(report_area = gp_prac_no,age_band = age_band_2)

gp_s1_weights <- make_weights_s1(responses_temp)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gp_s1_weights.rds"))
identical(hist.file,gp_s1_weights)
write.table(gp_s1_weights,paste0(weights_path,"gp_s1_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gp_s1_weights,paste0(weights_path,"gp_s1_weights.rds"))

gp_s2_weights <- make_weights(responses_temp,s2)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gp_s2_weights.rds"))
identical(hist.file,gp_s2_weights)
write.table(gp_s2_weights,paste0(weights_path,"gp_s2_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gp_s2_weights,paste0(weights_path,"gp_s2_weights.rds"))

gp_s3_weights <- make_weights(responses_temp,s3)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gp_s3_weights.rds"))
identical(hist.file,gp_s3_weights)
write.table(gp_s3_weights,paste0(weights_path,"gp_s3_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gp_s3_weights,paste0(weights_path,"gp_s3_weights.rds"))

gp_s4_weights <- make_weights(responses_temp,s4)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gp_s4_weights.rds"))
identical(hist.file,gp_s4_weights)
write.table(gp_s4_weights,paste0(weights_path,"gp_s4_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gp_s4_weights,paste0(weights_path,"gp_s4_weights.rds"))

gp_s5_weights <- make_weights(responses_temp,s5)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gp_s5_weights.rds"))
identical(hist.file,gp_s5_weights)
write.table(gp_s5_weights,paste0(weights_path,"gp_s5_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gp_s5_weights,paste0(weights_path,"gp_s5_weights.rds"))

gp_s6_weights <- make_weights(responses_temp,s6)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"gp_s6_weights.rds"))
identical(hist.file,gp_s6_weights)
write.table(gp_s6_weights,paste0(weights_path,"gp_s6_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(gp_s6_weights,paste0(weights_path,"gp_s6_weights.rds"))

rm(responses_temp,gp_s1_weights,gp_s2_weights,gp_s3_weights,gp_s4_weights,gp_s5_weights,
   gp_s6_weights)

#CA weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"ca_age_sex_population.rds"))
#Read in the PTI information, updated using syntax 'reformat_PTI_rates.R'
pti_rates <- readRDS(paste0(lookup_path,"pcis_rates_3_agebands.rds"))
ls(responses)
#apply weight categories

table(responses$practice_council_code,useNA = c("always"))
responses_temp <- responses %>% mutate(report_area = practice_council_code,age_band = age_band_3)

ca_s1_weights <- make_weights_s1(responses_temp)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_s1_weights.rds"))
identical(hist.file,ca_s1_weights)
write.table(ca_s1_weights,paste0(weights_path,"ca_s1_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_s1_weights,paste0(weights_path,"ca_s1_weights.rds"))

ca_s2_weights <- make_weights(responses_temp,s2)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_s2_weights.rds"))
identical(hist.file,ca_s2_weights)
write.table(ca_s2_weights,paste0(weights_path,"ca_s2_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_s2_weights,paste0(weights_path,"ca_s2_weights.rds"))

ca_s3_weights <- make_weights(responses_temp,s3)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_s3_weights.rds"))
identical(hist.file,ca_s3_weights)
write.table(ca_s3_weights,paste0(weights_path,"ca_s3_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_s3_weights,paste0(weights_path,"ca_s3_weights.rds"))

ca_s4_weights <- make_weights(responses_temp,s4)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_s4_weights.rds"))
identical(hist.file,ca_s4_weights)
write.table(ca_s4_weights,paste0(weights_path,"ca_s4_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_s4_weights,paste0(weights_path,"ca_s4_weights.rds"))

ca_s5_weights <- make_weights(responses_temp,s5)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_s5_weights.rds"))
identical(hist.file,ca_s5_weights)
write.table(ca_s5_weights,paste0(weights_path,"ca_s5_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_s5_weights,paste0(weights_path,"ca_s5_weights.rds"))

ca_s6_weights <- make_weights(responses_temp,s6)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_s6_weights.rds"))
identical(hist.file,ca_s6_weights)
write.table(ca_s6_weights,paste0(weights_path,"ca_s6_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_s6_weights,paste0(weights_path,"ca_s6_weights.rds"))

rm(responses_temp,ca_s1_weights,ca_s2_weights,ca_s3_weights,ca_s4_weights,ca_s5_weights,
   ca_s6_weights)

#Locality weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"locality_age_sex_population.rds"))
#Read in the PTI information, updated using syntax 'reformat_PTI_rates.R'
pti_rates <- readRDS(paste0(lookup_path,"pcis_rates_2_agebands.rds"))

#apply weight categories
table(responses$patient_hscp_locality,useNA = c("always"))
responses_temp <- responses %>% mutate(report_area = patient_hscp_locality,age_band = age_band_2)

locality_s1_weights <- make_weights_s1(responses_temp)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_s1_weights.rds"))
identical(hist.file,locality_s1_weights)
write.table(locality_s1_weights,paste0(weights_path,"locality_s1_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(locality_s1_weights,paste0(weights_path,"locality_s1_weights.rds"))

locality_s2_weights <- make_weights(responses_temp,s2)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_s2_weights.rds"))
identical(hist.file,locality_s2_weights)
write.table(locality_s2_weights,paste0(weights_path,"locality_s2_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(locality_s2_weights,paste0(weights_path,"locality_s2_weights.rds"))

locality_s3_weights <- make_weights(responses_temp,s3)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_s3_weights.rds"))
identical(hist.file,locality_s3_weights)
write.table(locality_s3_weights,paste0(weights_path,"locality_s3_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(locality_s3_weights,paste0(weights_path,"locality_s3_weights.rds"))

locality_s4_weights <- make_weights(responses_temp,s4)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_s4_weights.rds"))
identical(hist.file,locality_s4_weights)
write.table(locality_s4_weights,paste0(weights_path,"locality_s4_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(locality_s4_weights,paste0(weights_path,"locality_s4_weights.rds"))

locality_s5_weights <- make_weights(responses_temp,s5)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_s5_weights.rds"))
identical(hist.file,locality_s5_weights)
write.table(locality_s5_weights,paste0(weights_path,"locality_s5_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(locality_s5_weights,paste0(weights_path,"locality_s5_weights.rds"))

locality_s6_weights <- make_weights(responses_temp,s6)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"locality_s6_weights.rds"))
identical(hist.file,locality_s6_weights)
write.table(locality_s6_weights,paste0(weights_path,"locality_s6_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(locality_s6_weights,paste0(weights_path,"locality_s6_weights.rds"))

rm(responses_temp,locality_s1_weights,locality_s2_weights,locality_s3_weights,locality_s4_weights,locality_s5_weights,
   locality_s6_weights)

#ca_Locality weights####
#Read in the eligible population. Lookup file created in 'create reference files from eligible population.R'
area_age_sex_population <- readRDS(paste0(weights_path,"ca_locality_age_sex_population.rds"))
#Read in the PTI information, updated using syntax 'reformat_PTI_rates.R'
pti_rates <- readRDS(paste0(lookup_path,"pti_rates_2_agebands.rds"))

#apply weight categories
table(responses$patient_ca_locality,useNA = c("always"))
responses_temp <- responses %>% mutate(report_area = patient_ca_locality,age_band = age_band_2)

ca_locality_s1_weights <- make_weights_s1(responses_temp)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_s1_weights.rds"))
identical(hist.file,ca_locality_s1_weights)
write.table(ca_locality_s1_weights,paste0(weights_path,"ca_locality_s1_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_locality_s1_weights,paste0(weights_path,"ca_locality_s1_weights.rds"))

ca_locality_s2_weights <- make_weights(responses_temp,s2)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_s2_weights.rds"))
identical(hist.file,ca_locality_s2_weights)
write.table(ca_locality_s2_weights,paste0(weights_path,"ca_locality_s2_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_locality_s2_weights,paste0(weights_path,"ca_locality_s2_weights.rds"))

ca_locality_s3_weights <- make_weights(responses_temp,s3)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_s3_weights.rds"))
identical(hist.file,ca_locality_s3_weights)
write.table(ca_locality_s3_weights,paste0(weights_path,"ca_locality_s3_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_locality_s3_weights,paste0(weights_path,"ca_locality_s3_weights.rds"))

ca_locality_s4_weights <- make_weights(responses_temp,s4)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_s4_weights.rds"))
identical(hist.file,ca_locality_s4_weights)
write.table(ca_locality_s4_weights,paste0(weights_path,"ca_locality_s4_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_locality_s4_weights,paste0(weights_path,"ca_locality_s4_weights.rds"))

ca_locality_s5_weights <- make_weights(responses_temp,s5)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_s5_weights.rds"))
identical(hist.file,ca_locality_s5_weights)
write.table(ca_locality_s5_weights,paste0(weights_path,"ca_locality_s5_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_locality_s5_weights,paste0(weights_path,"ca_locality_s5_weights.rds"))

ca_locality_s6_weights <- make_weights(responses_temp,s6)
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"ca_locality_s6_weights.rds"))
identical(hist.file,ca_locality_s6_weights)
write.table(ca_locality_s6_weights,paste0(weights_path,"ca_locality_s6_weights.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(ca_locality_s6_weights,paste0(weights_path,"ca_locality_s6_weights.rds"))

rm(responses_temp,ca_locality_s1_weights,ca_locality_s2_weights,ca_locality_s3_weights,ca_locality_s4_weights,ca_locality_s5_weights,
   ca_locality_s6_weights)