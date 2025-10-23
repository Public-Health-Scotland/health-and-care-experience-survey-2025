# Name of file: 01.create_responses_longer.R
# 
# Original author(s): Catriona Haddow 
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Read in responses, restructure to be longer, add weights
# 
# Approximate run time: 2 min
# 
# Approximate memory usage: 7 GiB

#To do:
#Add analysis of other levels, eg localities. Can this be dynamic depending on SG / PHS?
#Does the list sizes part belong here?
#ML CHECK GIT ACCESS####

#Inputs:
#analysis_output_path,"responses_with_categories.rds"
#weights_path,"weights_vars.rds"
#lookup_path,"question_lookup.rds"

#Outputs: #UPDATE!
#"output/analysis_output/responses_longer.rds"
#"output/temp/forms_completed_list.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in results data####
responses <- readRDS(paste0(analysis_output_path,"responses_with_categories.rds"))

responses <- responses %>% mutate(scotland = "Scotland") #add new variable for reporting at national level
table(responses$q09a,useNA = c("always"))
#add in variables to get 'tick all that apply' totals

responses <- responses %>%
  rowwise() %>%
  mutate(q09 = max(q09a,q09b,q09c,q09d,q09e,q09f),
         q14 = max(q14a,q14b,q14c,q14d,q14e,q14f),
         q20 = max(q20a,q20b,q20c,q20d,q20e,q20f),
         q27 = max(q27a,q27b,q27c,q27d,q27e,q27f,q27g,q27h),
         q28 = max(q28a,q28b,q28c,q28d),
         q32 = max(q32a,q32b,q32c,q32d,q32e,q32f,q32g,q32h,q32i),
         q34 = max(q34a,q34b,q34c,q34d,q34e,q34f),
         q35 = max(q35a,q35b,q35c,q35d,q35e,q35f,q35g,q35h), #ML added q35h 15/2/24
         q39 = max(q39a,q39b,q39c,q39d,q39e,q39f,q39g,q39h,q39i,q39j,q39k))%>%
  mutate(across(q09a:q09f,~replace(., q09 != 1,NA)),
         across(q14a:q14f,~replace(., q14 != 1,NA)),
         across(q20a:q20f,~replace(., q20 != 1,NA)),
         across(q27a:q27h,~replace(., q27 != 1,NA)),
         across(q28a:q28d,~replace(., q28 != 1,NA)),
         across(q32a:q32i,~replace(., q32 != 1,NA)),
         across(q34a:q34f,~replace(., q34 != 1,NA)),
         across(q35a:q35h,~replace(., q35 != 1,NA)), #ML changed 'q35a:q35g' to 'q35a:q35h' 15/2/24
         across(q39a:q39k,~replace(., q39 != 1,NA))) %>% 
  ungroup()

#read in weights data####
weights_vars <- readRDS(paste0(weights_path,"weights_vars.rds"))

#joining by all id variables prevents duplication - that is, creation of patientid_sg.x for example
responses <- responses %>% 
  left_join(select(weights_vars,-eligible_pats,-gp_wt1), by = c("qh_psid","patientid","patientid_sg"))

#pivot longer####
responses_longer <- responses %>%
  pivot_longer(all_of(questions),names_to = "question", values_to = "response_option")%>%
  select("patientid",all_of(report_areas),ends_with('_wt'),question,response_option,eligible_pats) %>% 
  mutate(response_option = as.character(response_option))

#read in lookup to get weight category
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds"))

responses_longer <- responses_longer %>% 
  left_join(select(question_lookup,question,response_option,weight),by = c("question","response_option"))

#select weight for each question####
responses_longer <- responses_longer%>%
  mutate('nat_wt' = case_when(weight == "T1_Wt_Final" ~ nat_s1_wt,
                              weight == "T2_Wt_Final" ~ nat_s2_wt,
                              weight == "T3_Wt_Final" ~ nat_s3_wt,
                              weight == "T4_Wt_Final" ~ nat_s4_wt,
                              weight == "T5_Wt_Final" ~ nat_s5_wt,
                              weight == "T6_Wt_Final" ~ nat_s6_wt,
                              weight == "No_Weight" ~ 1),
         'hb_wt' = case_when(weight == "T1_Wt_Final" ~ hb_s1_wt,
                             weight == "T2_Wt_Final" ~ hb_s2_wt,
                             weight == "T3_Wt_Final" ~ hb_s3_wt,
                             weight == "T4_Wt_Final" ~ hb_s4_wt,
                             weight == "T5_Wt_Final" ~ hb_s5_wt,
                             weight == "T6_Wt_Final" ~ hb_s6_wt,
                             weight == "No_Weight" ~ 1),
         'hscp_wt' = case_when(weight == "T1_Wt_Final" ~ hscp_s1_wt,
                               weight == "T2_Wt_Final" ~ hscp_s2_wt,
                               weight == "T3_Wt_Final" ~ hscp_s3_wt,
                               weight == "T4_Wt_Final" ~ hscp_s4_wt,
                               weight == "T5_Wt_Final" ~ hscp_s5_wt,
                               weight == "T6_Wt_Final" ~ hscp_s6_wt,
                               weight == "No_Weight" ~ 1),
         'gpcl_wt' = case_when(weight == "T1_Wt_Final" ~ gpcl_s1_wt,
                               weight == "T2_Wt_Final" ~ gpcl_s2_wt,
                               weight == "T3_Wt_Final" ~ gpcl_s3_wt,
                               weight == "T4_Wt_Final" ~ gpcl_s4_wt,
                               weight == "T5_Wt_Final" ~ gpcl_s5_wt,
                               weight == "T6_Wt_Final" ~ gpcl_s6_wt,
                               weight == "No_Weight" ~ 1),
         'gp_wt' = case_when(weight == "T1_Wt_Final" ~ gp_s1_wt,
                             weight == "T2_Wt_Final" ~ gp_s2_wt,
                             weight == "T3_Wt_Final" ~ gp_s3_wt,
                             weight == "T4_Wt_Final" ~ gp_s4_wt,
                             weight == "T5_Wt_Final" ~ gp_s5_wt,
                             weight == "T6_Wt_Final" ~ gp_s6_wt,
                             weight == "No_Weight" ~ 1))

responses_longer <- responses_longer %>% 
  select("patientid",all_of(report_areas),all_of(report_area_wt),question,response_option,eligible_pats)

#check if the same as before, then save new file
hist.file <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))
identical(hist.file,responses_longer)
file.remove(paste0(analysis_output_path,"responses_longer.rds")) # remove existing file #ML 15/02/24 Changed 'lookup_path,"question_lookup_pnn.rds"' to 'analysis_output_path,"responses_longer.rds"'
saveRDS(responses_longer, paste0(analysis_output_path,"responses_longer.rds"))

#calculate response rate = completed form count / sample size ####
forms_completed_list <- lapply(report_areas, function(x) {
  x <- responses %>% group_by_at(x) %>% summarise(forms_completed = n())})

forms_completed_list <- lapply(seq_along(report_areas), function(x) {
  forms_completed_list[[x]][3] <- names(forms_completed_list[[x]])[1]
  names(forms_completed_list[[x]])[1] <- "report_area"
  names(forms_completed_list[[x]])[3] <- "level"
  forms_completed_list[[x]]
})

forms_completed_list <- bind_rows(forms_completed_list)
forms_completed_list$level <- str_replace_all(forms_completed_list$level, setNames(report_areas_output, report_areas))

#check if the same as before, then save new file
hist.file <- readRDS(paste0(analysis_output_path,"forms_completed_list.rds"))
identical(hist.file,forms_completed_list)
file.remove(paste0(analysis_output_path,"forms_completed_list.rds"))
saveRDS(forms_completed_list, paste0(analysis_output_path,"forms_completed_list.rds"))




