# Name of file: 03.analyse_pnn_questions.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Reads in PNN output. Run significance tests. Reads in historical data. Outputs analyses at all levels of reporting.
# 
# Approximate run time: 35 min
# 
# Approximate memory usage: 1 GiB
# 
# *****************************************

#Inputs: 
#"lookups/Final_Practice_lookup.rds" - created as part of preparation work
#"output/temp/output/temp/sample_size_list_net_of_deaths.rds" - created as part of preparation work
#"output/temp/forms_completed_list.rds" -  - created in script 01.create_responses_longer
#"lookups/question_lookup_pnn.rds" - created in script 00.create_question_lookup
#"output/temp/nat_pnn.rds"- created in script 03.analyse_pnn_questions
#"output/temp/hb_pnn.rds"- created in script 03.analyse_pnn_questions
#"output/temp/hscp_pnn.rds"- created in script 03.analyse_pnn_questions
#"output/temp/gpcl_pnn.rds"- created in script 03.analyse_pnn_questions
#"output/temp/gp_pnn.rds"- created in script 03.analyse_pnn_questions
#"output/temp/pnn_questions_historical.rds" - created in 2022

#Outputs: 
#"output/analysis_output/pnn_output_sg.rds"
#"output/analysis_output/pnn_output_sg.xlsx"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#Note: 28/04/2022 updated after production of file for SG to test use of significance tests,and to remove the tableau output. 
# Any changes to SG output file were checked for and found to be related to attributes only.

###Read in files
#read in Practice lookup, sample size, list completed,question_lookup_pnn
# practice_lookup <- readRDS(paste0(lookup_path,"Final_Practice_lookup.rds"))
# sample_size_list <- readRDS(paste0(output_path,"sampling/sample_size_list_net_of_deaths.rds"))
# forms_completed_list <- readRDS(paste0(analysis_output_path,"forms_completed_list.rds"))
question_lookup_pnn <- readRDS(paste0(lookup_path,"question_lookup_pnn.rds"))
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds"))
#read in pnn files

nat_pnn <- readRDS(paste0(analysis_output_path,"nat_pnn.rds"))
hb_pnn <- readRDS(paste0(analysis_output_path,"hb_pnn.rds"))
hscp_pnn <- readRDS(paste0(analysis_output_path,"hscp_pnn.rds"))
gpcl_pnn <- readRDS(paste0(analysis_output_path,"gpcl_pnn.rds"))
gp_pnn <- readRDS(paste0(analysis_output_path,"gp_pnn.rds"))

####significance text flags for comparison to scotland

pnn_output_comp <- bind_rows(hb_pnn,hscp_pnn,gpcl_pnn,gp_pnn)
pnn_output_comp <- left_join(pnn_output_comp,select(nat_pnn,question,"nat_wgt_percentpositive" = wgt_percentpositive,
                                                    "nat_wgt_percentpositive_low" = wgt_percentpositive_low,
                                                    "nat_wgt_percentpositive_upp" = wgt_percentpositive_upp,
                                                    "nat_n_includedresponses" = n_includedresponses,
                                                    "nat_wgt_percentpositive_deff" = wgt_percentpositive_deff), by = c("question"))


#flag to indicate whether the 2024 local level result is significantly higher or lower than Scotland result.
pnn_output_comp <- pnn_output_comp %>%
  mutate(p_scot=nat_wgt_percentpositive/100,
         n_scot=nat_n_includedresponses,
         p2=wgt_percentpositive/100,
         n2=n_includedresponses,
         scot_serror=sqrt((wgt_percentpositive_deff*(p2*(1-p2))/n2)+ (nat_wgt_percentpositive_deff*(p_scot*(1-p_scot))/n_scot)),
         scot_z_score=(p2-p_scot)/scot_serror, 
         scot_sigf = if_else(is.na(scot_z_score),1,
                             if_else(scot_z_score>qnorm(0.025,0,1) & scot_z_score<qnorm(0.975,0,1),0,1)),
         scot_var=round(100*(p2-p_scot),0.1),
         scot_flag = if_else(scot_sigf==1 & scot_var < 0,"Statistically lower than Scotland",  
                             if_else(scot_sigf==1 & scot_var > 0, "Statistically higher than Scotland",
                                     "Not statistically different from Scotland")))               


responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))

responses_longer <- responses_longer %>% 
  left_join(question_lookup %>% select(question,response_option,response_text_analysis),by = c("question","response_option")) %>% 
  mutate(response_text_analysis = case_match(response_text_analysis,"Exclude" ~ NA,.default = response_text_analysis))

responses_wider <- responses_longer %>% 
  pivot_wider(id_cols = c(patientid,all_of(report_areas_output),all_of(report_area_wt),no_wt,cancer_group_smr06),names_from = question,values_from = response_text_analysis)

responses_wider <- responses_longer %>% 
  pivot_wider(id_cols = c(patientid,all_of(report_areas_output),all_of(report_area_wt),no_wt),names_from = question,values_from = response_text_analysis)

#define survey design objects
options(survey.lonely.psu="remove")
surveydesign_nat<-svydesign(ids=~1, weights=~nat_wt, data=responses_wider)
surveydesign_nett<-svydesign(ids=~1, weights=~nett_wt, data=responses_wider)
surveydesign_netr<-svydesign(ids=~1, weights=~netr_wt, data=responses_wider)
surveydesign_hbt<-svydesign(ids=~1, weights=~hbt_wt, data=responses_wider)
surveydesign_hbr<-svydesign(ids=~1, weights=~hbr_wt, data=responses_wider)
surveydesign_hbr2<-svydesign(ids=~1, weights=~hbr2_wt, data=responses_wider)
surveydesign_cc<-svydesign(ids=~1, weights=~no_wt, data=responses_wider)

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




#Replicate national results 
nat_pnn <- left_join(nat_pnn,select(nat_pnn,question,"nat_wgt_percentpositive" = wgt_percentpositive,
                                    "nat_wgt_percentpositive_low" = wgt_percentpositive_low,
                                    "nat_wgt_percentpositive_upp" = wgt_percentpositive_upp,
                                    "nat_n_includedresponses" = n_includedresponses), by = c("question"))

nat_pnn <- nat_pnn %>%
  mutate(scot_flag = "Comparison not applicable")

pnn_output <-  bind_rows(pnn_output_comp,nat_pnn)

rm(gp_pnn,gpcl_pnn,hb_pnn,hscp_pnn,nat_pnn)

####add the question information####
pnn_output <- pnn_output %>% 
  left_join(distinct(question_lookup_pnn,question,question_text,question_2022,surveysection),by = c("question"))

#add on report_area names
pnn_output <- pnn_output %>% 
  left_join(distinct(practice_lookup,gp_prac_no,practice_name_letter),by = c("report_area" = "gp_prac_no")) %>% 
  left_join(distinct(practice_lookup,practice_board_code,practice_board_name),by = c("report_area" = "practice_board_code")) %>% 
  left_join(distinct(practice_lookup,practice_hscp_code,practice_hscp_name),by = c("report_area" = "practice_hscp_code")) %>% 
  mutate(report_area_name = case_when(level == "GP" ~ paste0(practice_name_letter," (",report_area,")"),
                                      level == "Health Board" ~ gsub(" and "," & ",practice_board_name),
                                      level == "HSCP" ~ practice_hscp_name,
                                      level %in% c("Scotland","GPCL")~ report_area, TRUE ~ "Error")) %>% 
  select(-practice_name_letter,-practice_board_name,-practice_hscp_name)

sum(pnn_output$report_area_name == "Error") #check. Should be 0

#add on completed forms and sample size
pnn_output <- pnn_output %>% 
  left_join(forms_completed_list, by = c("level","report_area")) %>% 
  left_join(sample_size_list, by = c("level","report_area")) %>% 
  mutate(Response_Rate_perc = forms_completed / sample_pop * 100)

####add on historical data####
pnn_output_historical <- readRDS(paste0(historical_data_path,"pnn_output_sg.rds"))
pnn_output_historical <- pnn_output_historical %>% 
  select(question,level,report_area_name,report_area,n_includedresponses,wgt_percentpositive,
         wgt_percentpositive_2020,wgt_percentpositive_2018,wgt_percentpositive_2016,wgt_percentpositive_2014,wgt_percentpositive_deff) %>% 
  rename(question_2022 = question,wgt_percentpositive_2022 = wgt_percentpositive,n_includedresponses_2022 = n_includedresponses,wgt_percentpositive_deff_2022 = wgt_percentpositive_deff)

ls(pnn_output_historical)
pnn_output_full <- pnn_output %>% 
  left_join(pnn_output_historical,by = c("question_2022","level","report_area"= "report_area"),suffix=c("_2024","")) 
#ML 04/04/24 Updated '"report_area_name"= "report_area"' to '"report_area"= "report_area"'
ls(pnn_output_full)

#ML 16/04/24 Replace the wgt_percentpositive_2022 values for levels GP and GPCL with NA - not eligible for significance testing against 2022.
pnn_output_full$wgt_percentpositive_2022[pnn_output_full$level == "GPCL"] <- NA
pnn_output_full$wgt_percentpositive_2022[pnn_output_full$level == "GP"] <- NA

####significance text flags for comparison to 2022
#flag to indicate whether the 2024 result is significantly higher or lower than 2022 result.

pnn_output_full <- pnn_output_full %>%
  mutate(p_2022=wgt_percentpositive_2022/100,
         n_2022=n_includedresponses_2022,
         p2=wgt_percentpositive/100,
         n2=n_includedresponses,
         time_serror=sqrt((wgt_percentpositive_deff*(p2*(1-p2))/n2)+ (wgt_percentpositive_deff_2022*(p_2022*(1-p_2022))/n_2022)),
         time_z_score=(p2-p_2022)/time_serror, 
         time_sigf = if_else(time_z_score>qnorm(0.025,0,1) & time_z_score <qnorm(0.975,0,1),0,1),
         time_var=round(100*(p2-p_2022),0.1),
         time_flag = if_else(is.na(wgt_percentpositive_2022),"Not tested against 2022",
                             if_else(time_sigf==1 & time_var < 0,"Statistically lower than 2022",  
                                     if_else(time_sigf==1 & time_var > 0, "Statistically higher than 2022",
                                             "Not statistically different from 2022"))))  

table(pnn_output_full$time_flag)
table(pnn_output_full$level,pnn_output_full$time_flag) # Extra check added 16/04/24 by ML

pnn_output_sg <- pnn_output_full %>% select("level","report_area","report_area_name_2024","question","question_text","surveysection",
                                            "n_includedresponses","n_positive","n_neutral","n_negative",
                                            "n_wgt_includedresponses","n_wgt_positive","n_wgt_neutral","n_wgt_negative",
                                            "wgt_percentpositive","wgt_percentpositive_low","wgt_percentpositive_upp",
                                            "wgt_percentpositive_deff","wgt_percentneutral","wgt_percentnegative",
                                            "wgt_percentpositive_2022","wgt_percentpositive_2020","wgt_percentpositive_2018","wgt_percentpositive_2016",
                                            "wgt_percentpositive_2014","scot_flag","time_flag",
                                            "nat_n_includedresponses","nat_wgt_percentpositive",
                                            "forms_completed","sample_pop","Response_Rate_perc")
#ML 04/04/24 Updated "report_area_name" to "report_area_name_2024"

#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"pnn_output_sg.rds"))
identical(hist.file,pnn_output_sg)

file.remove(paste0(analysis_output_path,"pnn_output_sg.rds"))
saveRDS(pnn_output_sg, paste0(analysis_output_path,"pnn_output_sg.rds"))
file.remove(paste0(analysis_output_path,"pnn_output_sg.xlsx"))
write.xlsx(pnn_output_sg, paste0(analysis_output_path,"pnn_output_sg.xlsx"))

#remove unnecessary files
pnn_subfiles <- dir(path =analysis_output_path,pattern='_pnn.rds')
pnn_subfiles <- paste0(analysis_output_path,pnn_subfiles)
file.remove(pnn_subfiles)





