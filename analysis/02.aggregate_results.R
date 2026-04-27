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
  responses.list[[x]] %>% filter(!is.na(response_text_analysis) & response_text_analysis != "Exclude") %>% 
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

# #ML 24-04-26 to avoid having to recreate again and save 1.5 <- 2 hours.
# #nat_agg
# saveRDS(nat_agg, paste0(analysis_output_path,"nat_agg.rds"))
# write_csv(nat_agg, paste0(analysis_output_path,"nat_agg_",today(),".csv"))
# 
# #hb_agg
# saveRDS(hb_agg, paste0(analysis_output_path,"hb_agg.rds"))
# write_csv(hb_agg, paste0(analysis_output_path,"hb_agg_",today(),".csv"))
# 
# #hscp_agg
# saveRDS(hscp_agg, paste0(analysis_output_path,"hscp_agg.rds"))
# write_csv(hscp_agg, paste0(analysis_output_path,"hscp_agg_",today(),".csv"))
# 
# #gpcl_agg
# saveRDS(gpcl_agg, paste0(analysis_output_path,"gpcl_agg.rds"))
# write_csv(gpcl_agg, paste0(analysis_output_path,"gpcl_agg_",today(),".csv"))
# 
# #gp_agg
# saveRDS(gp_agg, paste0(analysis_output_path,"gp_agg.rds"))
# write_csv(gp_agg, paste0(analysis_output_path,"gp_agg_",today(),".csv"))

agg_output <- bind_rows(nat_agg,hb_agg,hscp_agg,gpcl_agg,gp_agg)
agg_output <- agg_output %>%
  #Code to deal with 'tick all that apply' questions.Removes the "No" response to the "tick all that apply" questions
  filter(!(substr(question,1,3) %in% information_questions_tata & response_text_analysis == "No"))

#ML added start
#Expand to ensure all possible combinations of report area, question and response option are present in the output.####
#Create an lookup table of the required rows, then use to create a master table to ensure all possible option combinations exist
question_lookup_short <- readRDS(paste0(lookup_path,"question_lookup.rds")) %>% 
  distinct(question,response_text_analysis) %>% 
  #Removes the "No" response to the "tick all that apply" questions
  filter(!(substr(question,1,3) %in% information_questions_tata & response_text_analysis == "No")) %>% 
  filter(response_text_analysis != "Exclude")

report_area_combinations <- nrow(agg_output %>% ungroup() %>%  distinct(report_area)) #how many times do we need to repeat each question * response_option
idx <- rep(1:nrow(question_lookup_short), report_area_combinations)
expand.table <- question_lookup_short[idx,]
expand.table <- expand.table %>%
  mutate(report_area = rep(unique(agg_output$report_area),times = 1,each = nrow(question_lookup_short))) %>% 
  left_join(agg_output %>% ungroup() %>% distinct(report_area,level),by = c("report_area"))

agg_output_expanded <- expand.table %>% 
  left_join(agg_output,by = c("level","report_area","question","response_text_analysis"))
#ML added end

#read in Practice lookup
practice_lookup <- readRDS(paste0(lookup_path,"practice_lookup.rds"))

#add on report_area names
agg_output_expanded <- agg_output_expanded  %>% 
  left_join(distinct(practice_lookup,gp_prac_no,practice_name_letter),by = c("report_area" = "gp_prac_no")) %>% 
  left_join(distinct(practice_lookup,practice_board_code,practice_board_name),by = c("report_area" = "practice_board_code"))%>% 
  left_join(distinct(practice_lookup,practice_hscp_code,practice_hscp_name),by = c("report_area" = "practice_hscp_code"))%>% 
  mutate(report_area_name = case_when(level == "GP" ~ paste0(practice_name_letter," (",report_area,")"),
                                      level == "Health Board" ~ gsub(" and "," & ",practice_board_name),
                                      level == "HSCP" ~ practice_hscp_name,
                                      level %in% c("Scotland","GPCL")~ report_area, TRUE ~ "Error")) %>% 
  select(-practice_name_letter,-practice_board_name,-practice_hscp_name)

sum(agg_output_expanded$report_area_name == "Error") #check. Should be 0

#tidy to fill in the gaps where expand has left NAs
agg_output_expanded <- agg_output_expanded %>% 
  mutate(across(c("n_response", "n_wgt_response","percent", "wgt_percent"), ~ replace_na(., 0))) %>% 
  group_by(level,report_area,question) %>% 
  mutate(n_includedresponses_alt =  sum(n_response),
         n_wgt_includedresponses_alt = sum(n_wgt_response)) %>% 
  mutate(n_includedresponses= coalesce(n_includedresponses,n_includedresponses_alt),
         n_wgt_includedresponses = coalesce(n_wgt_includedresponses,n_wgt_includedresponses_alt)) %>% 
  select(-matches("alt"))

#check historical file output and save out
hist.file = readRDS(paste0(analysis_output_path,"agg_output.rds"))
all.equal(agg_output_expanded,hist.file)
saveRDS(agg_output_expanded,paste0(analysis_output_path,"agg_output.rds"))




# #add on report_area names
# #read in Practice lookup
# practice_lookup <- readRDS(paste0(lookup_path,"practice_lookup.rds"))
# 
# agg_output <- agg_output  %>% 
#   left_join(distinct(practice_lookup,gp_prac_no,practice_name_letter),by = c("report_area" = "gp_prac_no")) %>% 
#   left_join(distinct(practice_lookup,practice_board_code,practice_board_name),by = c("report_area" = "practice_board_code"))%>% 
#   left_join(distinct(practice_lookup,practice_hscp_code,practice_hscp_name),by = c("report_area" = "practice_hscp_code"))%>% 
#   mutate(report_area_name = case_when(level == "GP" ~ paste0(practice_name_letter," (",report_area,")"),
#                                       level == "Health Board" ~ gsub(" and "," & ",practice_board_name),
#                                       level == "HSCP" ~ practice_hscp_name,
#                                       level %in% c("Scotland","GPCL")~ report_area, TRUE ~ "Error")) %>% 
#   select(-practice_name_letter,-practice_board_name,-practice_hscp_name)
# 
# sum(agg_output$report_area_name == "Error") #check. Should be 0
# 
# #add on completed forms and sample size#### #CH - remove this section
# #read in sample size, list completed, 
# # sample_size <- readRDS(paste0(analysis_output_path,"sample_size_net_of_pse.rds"))
# # forms_completed_list <- readRDS(paste0(analysis_output_path,"forms_completed_list.rds"))
# # 
# # agg_output<- agg_output %>% 
# #   left_join(forms_completed_list, by = c("level","report_area")) %>% 
# #   left_join(sample_size, by = c("level","report_area")) %>% 
# #   mutate(response_rate_perc = forms_completed / net_sample_pop * 100)
# 
# #check historical file output
# hist.file = readRDS(paste0(analysis_output_path,"agg_output.rds"))
# all_equal(agg_output,hist.file)
# saveRDS(agg_output,paste0(analysis_output_path,"agg_output.rds"))



