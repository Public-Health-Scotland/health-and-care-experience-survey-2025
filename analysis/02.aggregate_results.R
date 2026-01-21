# Name of file: 02.aggregate_results.R
# 
# Description of content:  Read in responses, response rates and output analyses at all levels of reporting.
# 
# Approximate run time: 1 hour
# 
# Approximate memory usage: 1 GiB
# 
# *****************************************

#Inputs: 
#"lookups/information_questions.rds" - created in script 00.create_question_lookup
#"lookups/information_questions_tata.rds" - created in script 00.create_question_lookup
#"output/analysis_output/responses_longer.rds" - created in script 01.create_responses_longer
#"lookups/question_lookup_info.rds"- created in script 00.create_question_lookup
# lookup_path,"practice_lookup.rds"  #created in '...\202526\syntax\sampling\create_final_practice_lookup.R'.
#"analysis_output_path,"sample_size_net_of_pse.rds" - script 02.create_patient_info_files_from_sample
#"output/temp/forms_completed_list.rds" - created in script 01.create_responses_longer
#"historical_data_path,"info_questions_sg.rds"

#Question 6 is wrong - shouldn't have a Don't know option at this stage

#Outputs: 
#analysis_output_path,"agg_output.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in results longer data####
responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))

question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) 
question_lookup_short <- question_lookup %>% distinct(question,response_text_analysis)#read in lookup for info questions

#Make responses_longer a list so that lapply can be used
responses.list <- split(responses_longer, responses_longer$question)
responses.list <- setNames(responses.list,questions)

#this is needed for those questions with 1 positive response per practice
options(survey.lonely.psu="remove")

#define function to aggregate questions and add on CIs
aggregate_f <- function(report_areas,wt,x) {
  responses.list[[x]] %>% filter(!is.na(response_text_analysis)) %>% 
    as_survey_design(id = 1,strata = gp_prac_no, fpc = eligible_pats,weights = {{wt}}) %>% 
    group_by(question,{{report_areas}},response_text_analysis) %>%
    summarise(mean = survey_mean(na.rm = TRUE, vartype = c("ci"),deff = TRUE),
              n_response = sum(!is.na(response_code)),
              n_wgt_response = sum({{wt}}),.groups = "keep") %>% 
    rename(wgt_percent = mean,wgt_percent_low = mean_low,wgt_percent_upp = mean_upp) %>% 
    group_by(question,{{report_areas}}) %>%
    mutate(n_includedresponses = sum(n_response,na.rm = TRUE),
           n_wgt_includedresponses = sum(n_wgt_response,na.rm = TRUE),
           percent = n_response/n_includedresponses)}

#run at each level####
nat_agg  <- lapply(seq_along(questions), function(x) {
  aggregate_f(scotland,nat_wt,x)})
nat_agg <- bind_rows(nat_agg) %>% mutate(level = "Scotland") %>% rename("report_area" = "scotland")

hb_agg <- lapply(seq_along(questions), function(x) {
  aggregate_f(practice_board_code,hb_wt,x)}) 
hb_agg <- bind_rows(hb_agg) %>% mutate(level = "Health Board") %>% rename("report_area" = "practice_board_code")

hscp_agg <- lapply(seq_along(questions), function(x) {
  aggregate_f(practice_hscp_code,hscp_wt,x)}) 
hscp_agg <- bind_rows(hscp_agg) %>% mutate(level = "HSCP")%>% rename("report_area" = "practice_hscp_code")

gpcl_agg <- lapply(seq_along(questions), function(x) {
  aggregate_f(practice_hscp_cluster,gpcl_wt,x)}) 
gpcl_agg <- bind_rows(gpcl_agg) %>% mutate(level = "GPCL")%>% rename("report_area" = "practice_hscp_cluster")

gp_agg <- lapply(seq_along(questions), function(x) {
  aggregate_f(gp_prac_no,gp_wt,x)}) 
gp_agg <- bind_rows(gp_agg) %>% mutate(level = "GP")%>% rename("report_area" = "gp_prac_no")

agg_output <- bind_rows(nat_agg,hb_agg,hscp_agg,gpcl_agg,gp_agg)
agg_output <- agg_output %>%
  #Code to deal with 'tick all that apply' questions.Removes the "No" response to the "tick all that apply" questions
  filter(!(substr(question,1,3) %in% information_questions_tata & response_text_analysis == "No"))

#add on report_area names
#read in Practice lookup
practice_lookup <- readRDS(paste0(lookup_path,"practice_lookup.rds"))

agg_output <- agg_output  %>% 
  left_join(distinct(practice_lookup,gp_prac_no,practice_name_letter),by = c("report_area" = "gp_prac_no")) %>% 
  left_join(distinct(practice_lookup,practice_board_code,practice_board_name),by = c("report_area" = "practice_board_code"))%>% 
  left_join(distinct(practice_lookup,practice_hscp_code,practice_hscp_name),by = c("report_area" = "practice_hscp_code"))%>% 
  mutate(report_area_name = case_when(level == "GP" ~ paste0(practice_name_letter," (",report_area,")"),
                                      level == "Health Board" ~ gsub(" and "," & ",practice_board_name),
                                      level == "HSCP" ~ practice_hscp_name,
                                      level %in% c("Scotland","GPCL")~ report_area, TRUE ~ "Error")) %>% 
  select(-practice_name_letter,-practice_board_name,-practice_hscp_name)

sum(agg_output$report_area_name == "Error") #check. Should be 0

#add on completed forms and sample size####
#read in sample size, list completed, 
sample_size <- readRDS(paste0(analysis_output_path,"sample_size_net_of_pse.rds"))
forms_completed_list <- readRDS(paste0(analysis_output_path,"forms_completed_list.rds"))

agg_output<- agg_output %>% 
  left_join(forms_completed_list, by = c("level","report_area")) %>% 
  left_join(sample_size, by = c("level","report_area")) %>% 
  mutate(response_rate_perc = forms_completed / net_sample_pop * 100)

saveRDS(agg_output,paste0(analysis_output_path,"agg_output.rds"))



