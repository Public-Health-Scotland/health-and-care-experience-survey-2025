# *****************************************
# January 2025 WIP.
# Name of file: 01.create_responses_longer.R
# Description of content:  Read in responses, restructure to be longer, add weights.
# Also counts responses for use in dashboard
# 
# Approximate run time: 2 min
# 
# Approximate memory usage: 7 GiB

#Inputs:
#analysis_output_path,"responses_with_categories.rds" #created in 05.calculate_non_response_weight2.R
#weights_path,"weights_vars.rds" #created in 06.calculate_final_weights.R
#lookup_path,"question_lookup.rds" #created in script 00.create_question_lookup

#Outputs:
#analysis_output_path,"responses_longer.rds"
#analysis_output_path,forms_completed_list.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in results data####
responses <- readRDS(paste0(analysis_output_path,"responses_with_categories.rds"))
responses <- responses %>% mutate(scotland = "Scotland") #add new variable for reporting at national level

#read in weights data####
weights_vars <- readRDS(paste0(weights_path,"weights_vars.rds"))

#joining by all id variables prevents duplication - that is, creation of patientid_sg.x for example
responses <- responses %>% 
  left_join(select(weights_vars,-eligible_pats,-gp_wt1), by = c("qh_psid","patientid","patientid_sg"))

#pivot longer####
responses_longer <- responses %>%
  pivot_longer(all_of(questions),names_to = "question", values_to = "response_code")%>%
  select("patientid",all_of(report_areas),ends_with('_wt'),question,response_code,eligible_pats) %>% 
  mutate(response_code = as.character(response_code))

#read in lookup to get weight category
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds"))

responses_longer <- responses_longer %>% 
  left_join(select(question_lookup,question,response_code,response_text_analysis,weight),by = c("question","response_code"))

#select weight for each question####
responses_longer <- responses_longer%>%
  mutate('nat_wt' = case_when(weight == "T1_Wt_Final" ~ nat_s1_wt,
                              weight == "T2_Wt_Final" ~ nat_s2_wt,
                              weight == "T3_Wt_Final" ~ nat_s3_wt,
                              weight == "T4_Wt_Final" ~ nat_s4_wt,
                              weight == "T5_Wt_Final" ~ nat_s5_wt,
                              weight == "T6_Wt_Final" ~ nat_s6_wt,
                              weight == "No Weight" ~ 1),
         'hb_wt' = case_when(weight == "T1_Wt_Final" ~ hb_s1_wt,
                             weight == "T2_Wt_Final" ~ hb_s2_wt,
                             weight == "T3_Wt_Final" ~ hb_s3_wt,
                             weight == "T4_Wt_Final" ~ hb_s4_wt,
                             weight == "T5_Wt_Final" ~ hb_s5_wt,
                             weight == "T6_Wt_Final" ~ hb_s6_wt,
                             weight == "No Weight" ~ 1),
         'hscp_wt' = case_when(weight == "T1_Wt_Final" ~ hscp_s1_wt,
                               weight == "T2_Wt_Final" ~ hscp_s2_wt,
                               weight == "T3_Wt_Final" ~ hscp_s3_wt,
                               weight == "T4_Wt_Final" ~ hscp_s4_wt,
                               weight == "T5_Wt_Final" ~ hscp_s5_wt,
                               weight == "T6_Wt_Final" ~ hscp_s6_wt,
                               weight == "No Weight" ~ 1),
         'gpcl_wt' = case_when(weight == "T1_Wt_Final" ~ gpcl_s1_wt,
                               weight == "T2_Wt_Final" ~ gpcl_s2_wt,
                               weight == "T3_Wt_Final" ~ gpcl_s3_wt,
                               weight == "T4_Wt_Final" ~ gpcl_s4_wt,
                               weight == "T5_Wt_Final" ~ gpcl_s5_wt,
                               weight == "T6_Wt_Final" ~ gpcl_s6_wt,
                               weight == "No Weight" ~ 1),
         'gp_wt' = case_when(weight == "T1_Wt_Final" ~ gp_s1_wt,
                             weight == "T2_Wt_Final" ~ gp_s2_wt,
                             weight == "T3_Wt_Final" ~ gp_s3_wt,
                             weight == "T4_Wt_Final" ~ gp_s4_wt,
                             weight == "T5_Wt_Final" ~ gp_s5_wt,
                             weight == "T6_Wt_Final" ~ gp_s6_wt,
                             weight == "No Weight" ~ 1))

responses_longer <- responses_longer %>% 
  select("patientid",all_of(report_areas),all_of(report_area_wt),question,response_code,response_text_analysis,eligible_pats)

#check if the same as before, then save new file
hist.file <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))
all.equal(hist.file,responses_longer)
saveRDS(responses_longer, paste0(analysis_output_path,"responses_longer.rds"))

responses_counts <- responses_longer %>% 
  tabyl(question, response_code) 
write.table(responses_counts,paste0(analysis_output_path,"responses_counts.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)

#calculate response rate = completed form count / sample size ####
forms_completed_list <- lapply(report_areas, function(x) {
  x <- responses %>% group_by_at(x) %>% summarise(forms_completed = n())})

forms_completed_list <- lapply(seq_along(report_areas), function(x) {
  forms_completed_list[[x]][3] <- names(forms_completed_list[[x]])[1]
  names(forms_completed_list[[x]])[1] <- "report_area"
  names(forms_completed_list[[x]])[3] <- "level"
  forms_completed_list[[x]]
})

forms_completed_list <- bind_rows(forms_completed_list) %>% 
  mutate(level = str_replace_all(level, setNames(report_areas_output, report_areas)))

#check if the same as before, then save new file
hist.file <- readRDS(paste0(analysis_output_path,"forms_completed_list.rds"))
all.equal(hist.file,forms_completed_list)
saveRDS(forms_completed_list, paste0(analysis_output_path,"forms_completed_list.rds"))




