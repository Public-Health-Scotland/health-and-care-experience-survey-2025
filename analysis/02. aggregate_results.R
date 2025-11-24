# Name of file: 02.analyse_information_questions.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Analyse information questions. Read in responses, response rates and output analyses at all levels of reporting.
# 
# Approximate run time: 3 min
# 
# Approximate memory usage: 1 GiB
# 
# *****************************************

#Inputs: 
#"lookups/information_questions.rds" - created in script 00.create_question_lookup
#"lookups/information_questions_tata.rds" - created in script 00.create_question_lookup
#"output/analysis_output/responses_longer.rds" - created in script 01.create_responses_longer
#"lookups/question_lookup_info.rds"- created in script 00.create_question_lookup
#"lookups/Final_Practice_lookup.rds"  - created as part of preparation work
#"output/temp/output/temp/sample_size_list_net_of_deaths.rds" - ???
#"output/temp/forms_completed_list.rds"
#"historical_data_path,"info_questions_sg.rds"

#Outputs: 
##"output/analysis_output/info_questions_sg.rds"
##"output/analysis_output/info_questions_sg.xlsx"
##"output/analysis_output/info_output_full.rds")

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in results longer data####
responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))

#CPES_lookup <- readRDS("/conf/bss/CPES/2024/Data/Lookup/question_lookup.rds")
question_lookup_info <- readRDS(paste0(lookup_path_202324,"question_lookup_info.rds")) #read in lookup for info questions
question_lookup_pnn <- readRDS(paste0(lookup_path_202324,"question_lookup_pnn.rds")) #read in lookup for pnn questions
question_lookup <- question_lookup_info %>% 
  filter(question != "q38") %>%  #this means this will be dealt with as a PNN only. Not sure how to do both
  bind_rows(question_lookup_pnn)

#match question lookup onto responses longer to add on response_text_analysis only. Deal with NA
responses_longer <- responses_longer %>% 
  left_join(question_lookup %>% select(question,response_option,response_text),by = c("question","response_option")) 

hist.file <- readRDS(paste0(weights_path_202324,"weights_vars.rds"))
#Need to keep eligible patients for the calculation of the fpc?
responses_wider_nat <- responses_longer %>% 
  pivot_wider(id_cols = c(patientid,all_of(report_areas)),names_from = question,values_from = c(response_text,nat_wt))

#define survey design objects
#Not sure how to deal with different weights for each section. May need to adopt a questions by question approach, and loop through picking up the appropriate weight variable each time. Or at least section by section.
#Or leave the data long
options(survey.lonely.psu="remove")
surveydesign_nat<-svydesign(ids=~1, weights=~nat_wt, data=responses_wider_nat)

#define function to calculate CIs using survey package
get_survey_CIs <- function(x,survey_design_object,report_areas) {
  survey_design_object <- update(survey_design_object, analysis_var=factor(get(x)), grouping_var=factor(get(report_areas)))
  as.data.frame(confint(svyby(~analysis_var, by=~grouping_var, survey_design_object,svymean, na.rm=TRUE, deff="replace", keep.var=TRUE))) %>% 
    rename(wgt_percent_low = `2.5 %`,
           wgt_percent_upp = `97.5 %`) %>% 
    mutate("question" = x,
           divider = str_locate(rownames(.),":"),
           divider2 = if_else(str_detect(rownames(.),"[[:punct:]]{3}") == T,
                              str_locate(rownames(.),"[[:punct:]]{3}")-1,
                              nchar(rownames(.))),#get variable values from row names
           report_area = substr(rownames(.),1,divider-1),
           response_text_analysis = substr(rownames(.),divider+13,divider2)) %>% 
    select(report_area, question, response_text_analysis, wgt_percent_low,wgt_percent_upp)%>% 
    remove_rownames(.)}

#define aggregate_responses function
aggregate_responses <- function(report_areas,weights) {
  responses_longer %>% 
    filter(!is.na(response_text_analysis)) %>% 
    group_by("report_area" = {{report_areas}},question,response_text_analysis) %>%
    summarise(n_response = n(),
              n_wgt_response = sum({{weights}},na.rm = TRUE),.groups = "keep")}

#run at each level####
nat_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_nat,"scotland")) %>%  bind_rows() %>% mutate(level = "Scotland")
nat <- aggregate_responses(scotland,nat_wt) 
nat <- nat_cis %>%   left_join(nat,by = c("report_area","question","response_text_analysis"))


#add on completed forms and sample size####
#read in sample size, list completed, 
sample_size_list <- readRDS(paste0(output_path,"sampling/sample_size_list_net_of_deaths.rds"))
forms_completed_list <- readRDS(paste0(analysis_output_path,"forms_completed_list.rds"))

info_output <- info_output %>% 
  left_join(forms_completed_list, by = c("level","report_area")) %>% 
  left_join(sample_size_list, by = c("level","report_area")) %>% 
  mutate(Response_Rate_perc = forms_completed / sample_pop * 100)

####add on historical data####
info_questions_historical <- readRDS(paste0(historical_data_path,"info_questions_sg.rds"))
ls(info_questions_historical,sorted = FALSE)
table(info_questions_historical$Level)

#take out GP and GP Cluster data, and remove unnecessary variables
info_questions_historical <- info_questions_historical %>% 
                            filter(!Level %in% c("GP","GPCL")) %>% 
                            mutate(in_historical_file = 1) %>% 
                            select(-Forms_completed,-N_IncludedResponses,-sample_size,
                                  -SurveySection,-Question_text,-Response_Rate_perc, -sample_size,-SurveySection  ) %>% 
                            rename_with(tolower) 

#Read in info_output_all_historic_q10.rds and bind onto info_questions_historical.
q10_hist <- readRDS(paste0(output_path ,"analysis_output/Q10 INFO reconfig/info_output_all_historic_q10.rds"))
common_col_names <- intersect(names(info_questions_historical),names(q10_hist))
info_questions_historical <- rbind(info_questions_historical, q10_hist) 

#Is anything like this needed for 2024?
#remove q12 "Community Link worker" from info_questions_historical and update response_option for "Another Healthcare worker" from 7 to 6 in order to match onto info_output
# info_questions_historical <- subset(info_questions_historical, info_questions_historical$response_text != "Community Link Worker")
# info_questions_historical$response_option [info_questions_historical$question_2020 == "q12"
#                                           & info_questions_historical$response_text == "Another healthcare professional" 
#                                           & info_questions_historical$response_option == "7" ] <- "6"
# info_questions_historical$response_text[info_questions_historical$response_text == "Another healthcare professional"] <- "Another Healthcare professional"

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

#Rename variables for consistency with previous files. Is this still needed?
oldnames <- c("level","report_area_name","question_2024","question_text","surveysection","response_option","response_text2024",
              "n_includedresponses","n_response", "wgt_percent_response","n_response_2022","wgt_percent_response_2022","n_response_2020","wgt_percent_response_2020",
              "n_response_2018","wgt_percent_response_2018","n_response_2016","wgt_percent_response_2016",
              "n_response_2014","wgt_percent_response_2014","forms_completed","sample_pop","Response_Rate_perc")
newnames <- c("Level","Report_Area","Question_2024","Question_text","SurveySection","ResponseOption","Response_text",
              "N_IncludedResponses","N_Response_2024", "Wgt_Percent_Response_2024","N_Response_2022", "Wgt_Percent_Response_2022","N_Response_2020","Wgt_Percent_Response_2020",
              "N_Response_2018","Wgt_Percent_Response_2018","N_Response_2016","Wgt_Percent_Response_2016",
              "N_Response_2014","Wgt_Percent_Response_2014","Forms_completed","sample_size","Response_Rate_perc")

info_questions <- info_output_full %>% 
  rename_with(~newnames, .cols = all_of(oldnames))%>% 
  select(all_of(newnames))

#check if the same as before
hist.file_sg <- readRDS(paste0(analysis_output_path,"/info_questions_sg.rds"))
hist.file_sg <- hist.file_sg %>% arrange(Level,Report_Area,Question_2024,ResponseOption)

info_questions <- info_questions %>% arrange(Level,Report_Area,Question_2024,ResponseOption)
identical(info_questions,hist.file_sg)
#file.remove(paste0(analysis_output_path,"info_questions_sg.rds"))
saveRDS(info_questions, paste0(analysis_output_path,"/info_questions_sg.rds"))

write.xlsx(info_questions, paste0(analysis_output_path,"info_questions_sg.xlsx"))
