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
  mutate(Response_Rate_perc = forms_completed / net_sample_pop * 100)

saveRDS(agg_output,paste0(analysis_output_path,"agg_output.rds"))

####add on historical data####
info_questions_historical <- readRDS(paste0(analysis_output_path_202324,"info_questions_sg.rds"))
ls(info_questions_historical,sorted = FALSE)
table(info_questions_historical$Level)

#take out GP and GP Cluster data, and remove unnecessary variables
info_questions_historical <- info_questions_historical %>% 
  filter(!Level %in% c("GP","GPCL")) %>% 
  mutate(in_historical_file = 1) %>% 
  select(-Forms_completed,-N_IncludedResponses,-sample_size,
         -SurveySection,-Question_text,-Response_Rate_perc, -sample_size,-SurveySection  ) %>% 
  rename_with(tolower) 

pnn_questions_historical <- readRDS(paste0(analysis_output_path_202324,"pnn_output_sg.rds"))
pnn_nat_historical <- readRDS(paste0(analysis_output_path_202324,"nat_pnn.rds"))
ls(pnn_questions_historical,sorted = FALSE)
#take out GP and GP Cluster data, remove unnecessary variables and pivot wider
pnn_questions_historical <- pnn_questions_historical %>% 
  filter(!level %in% c("GP","GPCL")) %>% 
  mutate(in_historical_file = 1) %>% 
  select(question,report_area_name_2024,report_area,level,starts_with("wgt_p")) 

%>% 
  
  #rename so that can use in pivot longer
  rename_with(.fn = ~ str_replace_all(., "_percent", "_percent-"))


pnn_questions_historical_l <- pnn_questions_historical %>% 
  pivot_longer(cols= c(wgt_percentpositive,wgt_percentneutral,wgt_percentnegative),
                        names_to = c("response_text"),values_to = c("wgt_percent_response")) %>% 
                cols= c(wgt_percentpositive,wgt_percentneutral,wgt_percentnegative),
names_to = c("response_text"),values_to = c("wgt_percent_response")

#join info_output and info_questions_historical
info_output_full <- info_output %>% 
  left_join(info_questions_historical,by = c("question_2022","response_option" = "responseoption","level","report_area_name"= "report_area"),suffix=c("2024","hist")) %>%
  rename("question_2024"="question")

#info_output is intended to be a full version of the data
info_output_full <- info_output_full %>% 
  select(-in_historical_file,-response_texthist) %>%  #ch removed responseoption. Not sure why it was there
  rename("report_area_code"= "report_area")

hist.file <- readRDS(paste0(analysis_output_path,"info_output_full.rds"))
identical(info_output_full,hist.file)
saveRDS(info_output_full, paste0(analysis_output_path,"info_output_full.rds"))
