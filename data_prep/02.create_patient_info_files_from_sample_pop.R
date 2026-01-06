# Written by Catriona Haddow and Martin Leitch
# November 2023.
# Adapted from 2021 code by Catriona Haddow
#
# *****************************************
#Purpose: Reads in base sample file and creates a cut down version without identifiers for use in lookups and to retain.

#Inputs:
#"data/sampling/Master Sample File/2023.12.01_master_HACE_list_post_mailout.parquet" #UPDATE!
#"data_Validated results.rds"

#Outputs:
# "lookups/patientID_info.rds"
# "output/sampling/sample_for_SG.rds"
# "output/sampling/sample_for_SG.csv"
# "output/weights/sample_size_by_gp.rds"
# "output/weights/sample_size_by_gp.csv"
        
source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in the sample file.
# This is all sampled patients including patients that have been identified to be removed from the mail out via the death checks. Note that death check flags are included for use in calculating response percentages
 # sample_get_var_names <- read_parquet(paste0(sample_path,"Master Sample File/2025.12.10_master_HACE_list_post_mailout.parquet"), as_data_frame = FALSE) %>%
 #   slice_head(n = 1) %>%
 #   collect()
#psid removed:  what was this? is it useful? 
#ls(sample_get_var_names)
sample <- read_parquet(paste0(sample_path,"Master Sample File/2025.12.10_master_HACE_list_post_mailout.parquet"),   
                   col_select = c("patientid","sex","age","gp_prac_no","hb2019","hb2019name","hscp2019","hscp2019name","ca2019","ca2019name",
                                  "simd2020v2_sc_quintile","simd2020v2_sc_decile","ur6_2022","ur6_2022_name","ur8_2022","ur8_2022_name",
                                  "loc_hscp2019name","loc_ca2019name","loc_hscp_locality","IQVIA_exclude","IQVIA_flagdate","reason","pre_survey_exclusion","primary_exclusion_source"))
sample <- sample %>% 
  rename_with(tolower)%>% 
  mutate(patient_hscp_locality = paste(loc_hscp2019name,"HSCP -",loc_hscp_locality), #Create variable which combines loc_hscp2019name and loc_hscp_locality - Allows for easier identification for integration indicator tables.
          patient_hscp_locality = paste(loc_hscp2019name,"HSCP -",loc_hscp_locality), #Create variable which combines loc_ca2019name and loc_hscp_locality - Allows for easier identification for integration indicator tables.
          patient_ca_locality = paste(loc_ca2019name,"Local Authority -",loc_hscp_locality))

sample <- sample %>%
  mutate(patient_ca_locality = gsub("Na h-Eileanan Siar Local Authority","Na h-Eileanan Siar",patient_ca_locality)) %>% #Where patient_ca_locality contains "Na h-Eileanan Siar" drop "Local Authority" from description.
  relocate(c(patient_hscp_locality,patient_ca_locality), .before = iqvia_exclude) %>% #relocate patient_hscp_locality and patient_ca_locality
  mutate(age_band_2 = two_age_bands(age),#Add age groups
         age_band_3 = three_age_bands(age),
         age_band_6 = six_age_bands(age))

#read in validated results to match in patientid(PHS), patientid_SG + response.code(flag) to filter out non-respondents for sg.
validated_results <- readRDS(paste0(data_path,"results/data_Validated_results.rds"))

validated_results <- validated_results %>%
  select(patientid, patientid_sg, qh_psid, responsecode) %>% #added qh_psid here
  mutate(flagnew = 1)

sample <- sample %>% 
  left_join(validated_results,by = c("patientid"))

table(sample$flagnew,useNA = c("always")) #check matches
rm(validated_results)

#removed line:  rename(qh_psid = psid) %>% #PSID to QH_PSID - to enable matching in script 6 Calculate_non-response_weight2.R
sample <- sample %>% 
  relocate(c(patientid_sg,responsecode), .before = sex) %>%  #relocate patientid qh_psid patientid_sg and response.code(flag)
  relocate(c(patientid,qh_psid), .before = sex) %>% #tidy file
  select(-flagnew)

#check if the same as before
hist.file <- readRDS(paste0(lookup_path,"patientID_info.rds"))
all.equal(hist.file,sample)
saveRDS(sample,paste0(lookup_path,"patientID_info.rds"))

#save, dropping age, patient identifiers and QH exclusion reasons.
sample_for_SG <- sample %>%
                 filter (responsecode == 1) %>% #filter only respondents to the survey
                 select(-responsecode, -patientid, -qh_psid, -age, -iqvia_exclude, -iqvia_flagdate, , -reason, 
                       -hb2019, -hb2019name, -hscp2019, -hscp2019name, -ca2019, -ca2019name, -loc_hscp2019name, -loc_ca2019name, -loc_hscp_locality,
                       -patient_hscp_locality, -patient_ca_locality,-pre_survey_exclusion,-primary_exclusion_source)

#check if the same as before
hist.file <- readRDS(paste0(output_path,"sampling/sample_for_SG.rds"))
all.equal(hist.file,sample_for_SG)
#Save out anonymised version of validated data for SG as rds
saveRDS(sample_for_SG,paste0(output_path,"sampling/sample_for_SG.rds"))

table(sample$reason,sample$iqvia_exclude) # CH - to output?

#calculate sample size by practice
sample_size_by_gp <- sample %>%
  group_by(gp_prac_no) %>%
  summarise(sample_pop = n())

#save this to directory.
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"sample_size_by_gp.rds"))
all.equal(hist.file,sample_size_by_gp)

saveRDS(sample_size_by_gp,paste0(weights_path,"sample_size_by_gp.rds"))
#write_excel_csv(sample_size_by_gp,paste0(weights_path,"sample_size_by_gp.csv")) #CH why write excel_csv? delete this?
