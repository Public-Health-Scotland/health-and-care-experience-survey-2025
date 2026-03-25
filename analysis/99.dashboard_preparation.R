# *****************************************
# Name of file: 99.dashboard_preparation.R
# Description of content:  Prepares data for use in R shiny dashboard
# 
# Approximate run time: 5 mins
# 
# Approximate memory usage: 1 GiB
# 
# *****************************************
#Inputs: 
#lookup_path,"question_lookup.rds"
#analysis_output_path,"agg_output_full.rds" - created in 03.add_historical_data
#analysis_output_path,"responses_with_categories.rds" - created in 
#lookup_path,"practice_lookup.rds" - created in ...

#Outputs: 
# dashboard_path,"data_by_area.rds"
# dashboard_path,"rr_data.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#MAIN REPORTING DASHBOARD PREP####
# 1. Read in data and make adjustments for use in dashboard --------------------

question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) 

#Create response rate data###
#calculate response rate = completed form count / sample size ####
responses <- readRDS(paste0(analysis_output_path,"responses_with_categories.rds"))
responses <- responses %>% mutate(scotland = "Scotland") #add new variable for reporting at national level

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

sample_size_net_of_pse <- readRDS(paste0(analysis_output_path,"sample_size_net_of_pse.rds")) 

rr_data <- forms_completed_list %>% 
  left_join(sample_size_net_of_pse, by = c("level","report_area")) %>% 
  mutate(response_rate_perc = forms_completed / net_sample_pop * 100)


#Step 2. Create data by area file
agg_output_full <- readRDS(paste0(analysis_output_path,"agg_output_full.rds")) 

#Suppress questions with less than 20 responses.
agg_output_full <- agg_output_full %>% 
  mutate(across(.cols = matches("2026") & !matches("n_included"), ~ case_when(n_includedresponses_2026 < 20 ~ NA,TRUE ~ .)))

data_by_area <- readRDS(paste0(analysis_output_path,"agg_output_full.rds")) %>% 
  select(-c("topic","question_text")) %>%
  left_join(question_lookup %>% distinct(question,question_text_dashboard,topic), by = c("question")) %>% 
  left_join(rr_data, by = c("level","report_area")) %>% 
  filter(topic != "About You") %>%
  mutate(topic = str_replace(topic,"practice","Practice"),
         report_area_name = case_when(level == "HSCP" ~ paste0(report_area_name," ",level),
                                      level == "GPCL" ~ paste0(report_area_name," Cluster"),
                                      TRUE ~ report_area_name),
         response_code = case_when(response_text_analysis == "Postive" ~ "1", #add codes for PNN to ensure correct sorting
                                   response_text_analysis == "Neutral" ~ "2",
                                   response_text_analysis == "Negative" ~ "3", TRUE ~ response_code),
         response_text_dashboard = response_text_analysis,
         question = case_when(substr(question,1,3) %in% information_questions_tata ~ substr(question,1,3), TRUE ~ question),
         question = paste0(toupper(substr(question,1,1)),substr(question,2,nchar(question))),
         question_text_dashboard = paste(question, question_text_dashboard, sep = ": "),

         #response_text_dashboard2 = paste(response_code,"-",response_text_dashboard), don't know what this is for
         #question_type_dashboard = if_else(question_type == "info", "(Information question)","(Detailed experience question)"),
         #question_text_dashboard = paste(question_text, "", question_type_dashboard),
         #format numerical columns
         across(.cols = matches("perc"),.fns = ~ .*100),   # percentages to be shown out of 100
         across(.cols = matches("upp|low"),.fns = ~ round(.x, 2)), # rounding function for CIs
         across(.cols = !matches("upp|low") & matches("perc"),.fns = ~ round(.x, 1)),   # rounding function for other percentages
         across(.cols = c(n_includedresponses_2026),#Format to have numeric comma in position (where appropriate)
                .fns = ~ prettyNum(.x, big.mark = ",", scientific = FALSE))) %>% 
  select(-c("response_text_analysis"))

data_by_area <- data_by_area %>% 
  #Create warning message 
  mutate(warning = if_else(level == "GP" & (forms_completed <100 | response_rate_perc <= 14.99),"WARNING: There were either fewer than 100 responses from patients registered with this
                           practice, or the response rate was less than 15%.  Please treat results with caution as they may not be representative","None")) %>% 
  arrange(question,response_code,level,report_area_name)

data_by_area$question_text_wrapped <- sapply(data_by_area$question_text_dashboard, 
                                             FUN = function(question_text_dashboard) {paste(strwrap(question_text_dashboard, width = 50), collapse = "<br>")})

data_by_area <- data_by_area %>% 
  mutate(ci_2026 = if_else(!is.na(wgt_percent_2026),paste0("(", round(wgt_percent_low_2026, 2)," - ",round(wgt_percent_upp_2026, 2),")"),""),
         ci_2024 = if_else(!is.na(wgt_percent_2024),paste0("(", round(wgt_percent_low_2024, 2)," - ",round(wgt_percent_upp_2024, 2),")"),""),
         ci_2022 = if_else(!is.na(wgt_percent_2022),paste0("(", round(wgt_percent_low_2022, 2)," - ",round(wgt_percent_upp_2022, 2),")"),""),
         ci_2020 = if_else(!is.na(wgt_percent_2020),paste0("(", round(wgt_percent_low_2020, 2)," - ",round(wgt_percent_upp_2020, 2),")"),""),
         ci_2018 = if_else(!is.na(wgt_percent_2018),paste0("(", round(wgt_percent_low_2018, 2)," - ",round(wgt_percent_upp_2018, 2),")"),"")) %>% 
  ungroup()

saveRDS(data_by_area, paste0(dashboard_path,"data_by_area.rds"))

rr_data <- rr_data %>% 
  left_join(data_by_area %>% distinct(report_area,report_area_name,warning), by = c("report_area")) %>% 
  mutate(across(.cols = matches("perc"),.fns = ~ round(.x, 1)), #format percentage
         across(.cols = c(forms_completed,net_sample_pop),#Format to have numeric comma in position (where appropriate)
                .fns = ~ prettyNum(.x, big.mark = ",", scientific = FALSE)))

saveRDS(rr_data, paste0(dashboard_path,"rr_data.rds"))

summary_questions_gp <- c("Q13","Q03","Q12b","Q16b","Q16d","Q16l")
summary_questions_other <- c("Q13","Q03","Q25","Q30e","Q30g","Q31","Q30e","Q37e","Q37a","Q37c")

data_by_area_summary <- data_by_area %>%  
  filter((level %in% c("Scotland","Health Board","HSCP") & question %in% summary_questions_other) |
           (level %in% c("GP","GPCL") & question %in% summary_questions_gp)) %>% 
  select(question,question_text_dashboard,question_text_wrapped,topic,level,report_area,report_area_name,n_includedresponses_2026,response_text_dashboard,wgt_percent_2026,warning) %>% 
  pivot_wider(names_from = response_text_dashboard, values_from = wgt_percent_2026) 

pp_data <- data_by_area %>% ungroup() %>% 
  filter(level == "Health Board" & 
           question %in% c(summary_questions_other,summary_questions_gp) &
           response_text_dashboard == "Positive") %>% 
  select("positive_hb_name" = report_area_name,question,"Positive_HB" = wgt_percent_2026)  %>% 
  left_join(data_by_area  %>%
              filter(level == "Scotland" &
                     question %in% c(summary_questions_other,summary_questions_gp) &
                     response_text_dashboard == "Positive") %>% 
              select(question,"Positive_Scotland" = wgt_percent_2026), by = c("question")) %>% 
  ungroup()

pp_data <- pp_data %>% ungroup()

#read in Practice lookup
practice_lookup <- readRDS(paste0(lookup_path,"practice_lookup.rds")) 

data_by_area_summary <- data_by_area_summary %>% 
  left_join(practice_lookup %>% distinct(gp_prac_no,gp_practice_board_name = practice_board_name),by = c("report_area" = "gp_prac_no")) %>% 
  left_join(practice_lookup %>% distinct(practice_hscp_cluster,gpcl_practice_board_name = practice_board_name),by = c("report_area" = "practice_hscp_cluster")) %>% 
  left_join(practice_lookup %>% distinct(practice_hscp_code,hscp_practice_board_name = practice_board_name),by = c("report_area" = "practice_hscp_code")) %>% 
  mutate("positive_hb_name" = case_when(level == "GP" ~ gp_practice_board_name,
                                        level == "GPCL" ~ gpcl_practice_board_name,
                                        level == "HSCP" ~ hscp_practice_board_name,
                                        level == "Health Board" ~ report_area_name,
                                        level == "Scotland" ~ "Not applicable")) %>% 
  select(-matches("practice_board_name"))

data_by_area_summary <- data_by_area_summary %>% 
  left_join(pp_data, by = c("positive_hb_name","question"))

saveRDS(pp_data, paste0(dashboard_path,"pp_data.rds"))

saveRDS(data_by_area_summary, paste0(dashboard_path,"data_by_area_summary.rds"))
