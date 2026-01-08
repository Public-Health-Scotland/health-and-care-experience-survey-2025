# Name of file: 02.aggregate_results.R
# 
# Description of content:  Read in responses, response rates and output analyses at all levels of reporting.
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
# lookup_path,"practice_lookup.rds"  #created in '...\202526\syntax\sampling\create_final_practice_lookup.R'.
#"analysis_output_path,"sample_size_net_of_pse.rds" - script 02.create_patient_info_files_from_sample
#"output/temp/forms_completed_list.rds" - created in script 01.create_responses_longer
#"historical_data_path,"info_questions_sg.rds"

#Outputs: 
##"output/analysis_output/info_questions_sg.rds"
##"output/analysis_output/info_questions_sg.xlsx"
##"output/analysis_output/info_output_full.rds")

#to do: there is something wrong with the coding of NA responses - see question 4 response option 3

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in results longer data####
responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))

question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) 
question_lookup_short <- question_lookup %>% distinct(question,response_text_analysis)#read in lookup for info questions

# survey_design_l<-responses_longer %>% 
#     as_survey_design(
#     strata = gp_prac_no, 
#     fpc = eligible_pats,
#     weights = nat_wt)

aggregate_f <- function(report_areas,wt) {
     responses_longer <- responses_longer %>%
        group_by("report_area" = {{report_areas}},question,response_text_analysis)%>%
        summarise(n_response = sum(!is.na(response_code)),
                  n_wgt_response = sum({{wt}}),.groups = "keep")}

  
#define the aggregate function.####
# aggregate_f <- function(report_areas,wt) {
#   survey_design_l<-responses_longer %>% 
#     filter(!is.na(response_text_analysis)) %>% 
#     as_survey_design(
#       strata = gp_prac_no, 
#       fpc = eligible_pats,
#       weights = {{wt}})
#   responses_longer <- survey_design_l %>%
#     group_by("report_area" = {{report_areas}},question,response_text_analysis)%>%
#     summarise(n_response = n(),
#               survey_prop(na.rm = TRUE,vartype = c("ci"),level = 0.95,prop_method = c("logit")),
#               survey_mean(na.rm = TRUE,vartype = c("ci"),level = 0.95,prop_method = c("logit")),
#               n_wgt_response = sum({{weights}},na.rm = TRUE), .groups = "keep")%>% 
#        rename(wgt_percent = coef)
# }

#define the expand table function.####
#Create an index of the required rows, then use to create a master table to ensure all possible option combinations exist
expand_table <- function(df) {
  idx <- rep(1:nrow(question_lookup), length(unique(df$report_area)))
  expand.table <-  question_lookup[idx,]
  expand.table <- expand.table %>%
    mutate(report_area = rep(unique(df$report_area),nrow(question_lookup)))%>%
    left_join(df,by = c("report_area","question","response_text_analysis"))
}

#run at each level####
df <- aggregate_f(scotland,nat_wt)
nat_agg <- expand_table(df) %>% mutate(level = "Scotland")

df <- aggregate_f(practice_board_code,hb_wt)
hb_agg <- expand_table(df)%>% mutate(level = "Health Board")

df <- aggregate_f(practice_hscp_code,hscp_wt)
hscp_agg <- expand_table(df)%>% mutate(level = "HSCP")

df <- aggregate_f(practice_hscp_cluster,gpcl_wt)
gpcl_agg <- expand_table(df)%>% mutate(level = "GPCL")

df <- aggregate_f(gp_prac_no,gp_wt)
gp_agg <- expand_table(df)%>% mutate(level = "GP")

agg_output <- bind_rows(nat_agg,hb_agg,hscp_agg,gpcl_agg,gp_agg)
agg_output <- agg_output %>%
  group_by(report_area,question) %>%
  mutate(n_includedresponses = sum(n_response,na.rm = TRUE),
         n_wgt_includedresponses = sum(n_wgt_response,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent_response = n_response / n_includedresponses * 100,
         wgt_percent_response= n_wgt_response / n_wgt_includedresponses * 100) %>% 
#Code to deal with 'tick all that apply' questions.Removes the "No" response to the "tick all that apply" questions
  filter(!(substr(question,1,3) %in% information_questions_tata & response_text_analysis == "No"))

result <- prop.test(agg_output$n_wgt_response, agg_output$n_wgt_includedresponses, conf.level = 0.95)
print(result$conf.int)

#add basic CI calculations here....####
#this is not correct but added to allow progress
survey_design_nat<-  as_survey_design(responses_longer %>% 
                                        filter(!is.na(response_text_analysis)), 
      strata = gp_prac_no, 
      fpc = eligible_pats,
      weights = nat_wt)

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

get_survey_CIs()
agg_output <- agg_output %>%
  mutate(confint(wgt_percent_response,level = 0.95))

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
