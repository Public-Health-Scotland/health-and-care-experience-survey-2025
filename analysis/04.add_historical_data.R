# *****************************************
# January 2025 WIP.
# Name of file: 04.add_historical_data.R
# Description of content:  Reads in current and links to historical aggregate data and outputs analyses at all levels of reporting.
# 
# Approximate run time: 35 min
# 
# Approximate memory usage: 1 GiB
# 
# *****************************************

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#recode 'no change'?
#Questions to check Q6 2024 - need to explicitly exclude exclude option, Q6 2018 has multiple mapped response options
#add question text too

####add on historical data####
agg_output <- readRDS(paste0(analysis_output_path,"agg_output.rds")) %>% 
  rename_with(.fn = ~ paste0(.,"_2026"),  .cols = where(is.numeric))   #rename to identify as being from 202526

#PNN columns need mapping by response_text_analysis, whereas info questions need to be mapped by response_code.

#create version of lookup which allows mapping by response_text_analysis
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) 
question_lookup_pnn <- question_lookup %>% 
  filter(question %in% percent_positive_questions) %>% 
  distinct(question,question_text,question_2024,question_2022,question_2020,question_2018)

#split out pnn questions
agg_output_pnn <- agg_output %>% filter(question %in% percent_positive_questions) %>% 
  left_join(question_lookup_pnn,by = c("question"))

#map on 202324 pnn data 
agg_output_pnn_202324 <- readRDS(paste0(analysis_output_path_all_years,"agg_output_pnn_202324.rds"))
agg_output_pnn <- agg_output_pnn %>% 
  left_join(agg_output_pnn_202324, by = c("level","report_area","question_2024","response_text_analysis" = "response_text_analysis_2024"))

#map on 202122 pnn data 
agg_output_pnn_202122 <- readRDS(paste0(analysis_output_path_all_years,"agg_output_pnn_202122.rds"))
agg_output_pnn <- agg_output_pnn %>% 
  left_join(agg_output_pnn_202122, by = c("level","report_area","question_2022","response_text_analysis" = "response_text_analysis_2022"))

#split out infon questions
agg_output_info <- agg_output %>% filter(question %in% information_questions) %>% 
  left_join(question_lookup,by = c("question","response_text_analysis"))

#map on 202324 info data 
agg_output_info_202324 <- readRDS(paste0(analysis_output_path_all_years,"agg_output_info_202324.rds"))
agg_output_info <- agg_output_info %>% 
  left_join(agg_output_info_202324, by = c("level","report_area","question_2024","response_code_2024"))

#map on 202122 info data 
agg_output_info_202122 <- readRDS(paste0(analysis_output_path_all_years,"agg_output_info_202122.rds"))
agg_output_info <- agg_output_info %>% 
  left_join(agg_output_info_202122, by = c("level","report_area","question_2022","response_code_2022"))

#bring PNN and aggregate questions back together
agg_output_full <- agg_output_info %>% 
  bind_rows(agg_output_pnn)

#map on 201920 data. Here, response code is within the response_text analysis field, so no need to split into pnn/info
agg_output_201920 <- readRDS(paste0(analysis_output_path_all_years,"agg_output_201920.rds"))
agg_output_full <- agg_output_full %>% 
  left_join(agg_output_201920, by = c("level","report_area","question_2020","response_text_analysis" = "response_text_analysis_2020"))

#map on 201718 data 
agg_output_201718 <- readRDS(paste0(analysis_output_path_all_years,"agg_output_201718.rds"))
agg_output_full <- agg_output_full %>% 
  left_join(agg_output_201718, by = c("level","report_area","question_2018","response_text_analysis" = "response_text_analysis_2018"))

ls(agg_output_full)

#re-order and keep only required variables
agg_output_full <- agg_output_full %>% 
  select(level,report_area,report_area_name,forms_completed_2026,net_sample_pop_2026,"response_rate_perc_2026" = Response_Rate_perc_2026,
         topic,question,question_text,response_code,
         n_includedresponses_2026,n_response_2026,percent_2026,
         n_wgt_includedresponses_2026,n_wgt_response_2026,wgt_percent_2026,wgt_percent_low_2026,wgt_percent_upp_2026,
         wgt_percent_2024,wgt_percent_low_2024,wgt_percent_upp_2024,
         wgt_percent_2022,wgt_percent_low_2022,wgt_percent_upp_2022,
         wgt_percent_2020,wgt_percent_low_2020,wgt_percent_upp_2020,
         wgt_percent_2018,wgt_percent_low_2018,wgt_percent_upp_2018)
  
hist.file <- readRDS(paste0(analysis_output_path,"agg_output_full.rds"))
identical(agg_output_full,hist.file)
saveRDS(agg_output_full, paste0(analysis_output_path,"agg_output_full.rds"))





