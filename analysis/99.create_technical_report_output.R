# WIP: November 2025

#Technical report output.
#Purpose: Create output for the HACE Technical Output.

#To do
#Check practicelistsize
#Check dates

#Inputs:
#lookup_path,"patientID_info.rds" created in 02.create_patient_info_files_from_sample_pop.R
#lookup_path,"practice_lookup.rds" #created in '...\202526\syntax\sampling\create_final_practice_lookup.R'.
#data_path, results/data_Validated_results.rds #created in 01.validation.R
#sample_path,"Master Sample File/2025.12.10_master_HACE_list_post_mailout.parquet" # add where this created? sampling

#Outputs:
#output_path,"/technical_report/technical_report_populated_tables_",today(),".xlsx"
#output_path,"technical_report/sample_removals_note_",today(),".xlsx"

#Process:
#Obtain tidied up results file data_Validated results.rds
#THEN
#Match on the patient file which contains all necessary reference info (sample_patientID_info,"lookups/Final_Practice_lookup.rdss")
#THEN
#Deduct those patients identified in the pre-survey check and day one of the survey mail out

# *****************************************
source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#Read in list on selected patients for surveying####
patient_info <-read_rds(paste0(lookup_path,"patientID_info.rds")) #file created in 02.create_patient_info_files_from_sample

#Match on GP Practice information####
practice_info <- read_rds(paste0(lookup_path,"practice_lookup.rds")) 
patient_info <- left_join(patient_info,practice_info,by = c("gp_prac_no"))
sum(is.na(patient_info$gp_prac_no)) #this to check if lookup is successful and file complete
rm(practice_info)

#Read in validated results to obtain survey respondents and response method####
validated_data <-readRDS(paste0(data_path,"results/data_Validated_results.rds"))
validated_data <- validated_data %>%
  select(qh_psid,patientid,patientid_sg,responsecode,responsesubcode)

#Match validated_data onto patient_info####
patient_info <- left_join(patient_info,validated_data,by = c("patientid","patientid_sg","responsecode","qh_psid"))
iqvia_flagdates <- patient_info %>% 
  group_by(iqvia_flagdate) %>% 
  summarise(count = n())

table(patient_info$iqvia_flagdate,is.na(patient_info$iqvia_flagdate),useNA = c("always"))
rm(validated_data)

#Deduct those patients identified in the pre-survey checks of 15,20,21/10/25 and day one of the survey mail out (22/10/25).####

patient_info <- patient_info %>% 
   filter(iqvia_flagdate > as.Date("2025-10-22") |
            is.na(iqvia_flagdate))

#Table 6 Response rate by submission method####

submission_response_rate <- patient_info %>% 
  filter(responsecode == 1) %>% 
  group_by("Method" = case_when(responsesubcode == "L" ~ "Language Line",
                                responsesubcode == "O" ~ "Online",
                                responsesubcode == "P" ~ "Post",
                                responsesubcode == "T" ~ "Telephone",TRUE ~ "Error")) %>% 
  summarise(count = n()) %>% 
  mutate(total = sum(count)) %>% 
  bind_rows(patient_info %>% filter(responsecode == 1) %>% 
            group_by("Method" = "Total") %>% #Add Total line
            summarise(count = n(),
            total = n())) %>% 
  mutate(percent = count/total)
  
#Table 7 Response rate by practice list size from CHI####

patient_info <- patient_info %>%
  mutate(practice_population = case_when(practicelistsize_chi>=0 & practicelistsize_chi<=2499~  "< 2,500",
                                         practicelistsize_chi>=2500 & practicelistsize_chi<=4999~  "2,500 to 4,999",
                                         practicelistsize_chi>=5000 & practicelistsize_chi<=7499~  "5,000 to 7,499",
                                         practicelistsize_chi>=7500 & practicelistsize_chi<=9999~  "7,500 to 9,999",
                                         practicelistsize_chi>=10000~  "10,000 +",
                                         TRUE~  "Not known")) %>% 
  mutate(practice_population = factor(practice_population, 
                      levels = c('< 2,500', '2,500 to 4,999', '5,000 to 7,499', '7,500 to 9,999', '10,000 +'))
)

table(patient_info$practice_population,useNA = c("always"))
practice_pop_response_rate <- patient_info %>%
  mutate(record = 1) %>% 
  group_by(practice_population) %>% 
  summarise("Total_number_of_forms_sent_out" = sum(record),
            "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
  mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out) %>% 
  bind_rows(patient_info %>%
              mutate(record = 1) %>% 
              group_by(practice_population = "Total") %>% 
              summarise("Total_number_of_forms_sent_out" = sum(record),
                        "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
              mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out))

#Table 8 Response rate by Health Board (not published)####

HB_Response_rate <- patient_info %>%
  mutate(record = 1) %>% 
  group_by(practice_board_name) %>% 
  summarise("Total_number_of_forms_sent_out" = sum(record),
            "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
  mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out) %>% 
  bind_rows(patient_info %>%
            mutate(record = 1) %>% 
            group_by(practice_board_name = "Total") %>% 
              summarise("Total_number_of_forms_sent_out" = sum(record),
                        "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
              mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out))

#Table 8 Response rate by Health and Social Care Partnership ####
  
HSCP_Response_rate <- patient_info %>%
  mutate(record = 1) %>% 
  group_by(practice_hscp_name) %>% 
  summarise("Total_number_of_forms_sent_out" = sum(record),
            "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
  mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out) %>% 
  bind_rows(patient_info %>%
              mutate(record = 1) %>% 
              group_by(practice_hscp_name = "Total") %>% 
              summarise("Total_number_of_forms_sent_out" = sum(record),
                        "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
              mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out))

#Table 9 Response rate by deprivation quintile ####

SIMD_Response_rate <- patient_info %>%
  mutate(record = 1) %>% 
  group_by(simd2020v2_sc_quintile = as.character(simd2020v2_sc_quintile)) %>% 
  summarise("Total_number_of_forms_sent_out" = sum(record),
            "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
  mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out) %>% 
  bind_rows(patient_info %>%
              mutate(record = 1) %>% 
              group_by(simd2020v2_sc_quintile = "Total") %>% 
              summarise("Total_number_of_forms_sent_out" = sum(record),
                        "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
              mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out))

#Table 10 Response rate by urban / rural location####

UR6_Response_rate <- patient_info %>%
  mutate(record = 1) %>% 
  group_by(ur6_2022_name) %>% 
  summarise("Total_number_of_forms_sent_out" = sum(record),
            "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
  mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out) %>% 
  bind_rows(patient_info %>%
              mutate(record = 1) %>% 
              group_by(ur6_2022_name = "Total") %>% 
              summarise("Total_number_of_forms_sent_out" = sum(record),
                        "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
              mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out))

#Table 11 Response rate by age group####

Age_Response_rate <- patient_info %>%
  mutate(record = 1) %>% 
  group_by(age_band_6) %>% 
  summarise("Total_number_of_forms_sent_out" = sum(record),
            "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
  mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out) %>% 
  bind_rows(patient_info %>%
              mutate(record = 1) %>% 
              group_by(age_band_6 = "Total") %>% 
              summarise("Total_number_of_forms_sent_out" = sum(record),
                        "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
              mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out))

#Table 11 Response rate by age group and submission method#### CH - is this useful? Maybe more interesting if it's the proportion by method within age group

Age_Method_Response_rate <- patient_info %>%
  mutate(record = 1) %>% 
  group_by(age_band_6,"Method" = responsesubcode) %>% 
  summarise("Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
  left_join(Age_Response_rate %>% select(age_band_6,Total_number_of_forms_sent_out), by = c("age_band_6"))%>% 
  mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out)
  
#Table 12 Response rate by sex####

Sex_Response_rate <- patient_info %>%
  mutate(sex_desc = case_when(sex==1 ~ "Male",sex==2 ~ "Female"),record = 1) %>% 
  group_by(sex_desc) %>% 
  summarise("Total_number_of_forms_sent_out" = sum(record),
            "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
  mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out) %>% 
  bind_rows(patient_info %>%
              mutate(record = 1) %>% 
              group_by(sex_desc = "Total") %>% 
              summarise("Total_number_of_forms_sent_out" = sum(record),
                        "Number_of_Responses" = sum(record[responsecode == 1],na.rm = T)) %>% 
              mutate(response_rate = Number_of_Responses / Total_number_of_forms_sent_out))

#Pick up reporting template and populate, save outfile as xlsx####

template <- loadWorkbook(paste0(output_path, "technical_report/technical_report_tables_template.xlsx"))

run_date <-paste0("Table last updated: ",format(Sys.Date(), format ="%d %B %Y"))#Add Date Tables Run

writeData(template, "Submission Method", submission_response_rate, startCol = 2, startRow = 6)
writeData(template, "Practice List Size", practice_pop_response_rate, startCol = 2, startRow = 6)
writeData(template, "Board", HB_Response_rate, startCol = 2, startRow = 6)
writeData(template, "HSCP", HSCP_Response_rate, startCol = 2, startRow = 6)
writeData(template, "SIMD", SIMD_Response_rate, startCol = 2, startRow = 6)
writeData(template, "UR6", UR6_Response_rate, startCol = 3, startRow = 6)
writeData(template, "Sex", Sex_Response_rate, startCol = 2, startRow = 6)
writeData(template, "Age", Age_Response_rate, startCol = 2, startRow = 6)

writeData(template, "Submission Method", run_date, startCol = 1, startRow = 14)
writeData(template, "Practice List Size", run_date, startCol = 1, startRow = 17)
writeData(template, "Board", run_date, startCol = 1, startRow = 26)
writeData(template, "HSCP", run_date, startCol = 1, startRow = 42)
writeData(template, "SIMD", run_date, startCol = 1, startRow = 19)
writeData(template, "UR6", run_date, startCol = 1, startRow = 18)
writeData(template, "Sex", run_date, startCol = 1, startRow = 14)
writeData(template, "Age", run_date, startCol = 1, startRow = 17)

saveWorkbook(template,paste0(output_path,"/technical_report/technical_report_populated_tables_",today(),".xlsx"), overwrite =TRUE)

#Technical report:
#Count of removals for summary template####
#Technical report output - removals note:

#Total No Sampled 
#No Removed Pre Survey and Day 1 Mail Out (Deaths)
#No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)
#Total No Sent out 
#Total Deaths Day 2 (Rest of Initial Mail Out)
#Total No of Removals Sent to QH (after Day 2)

sample <- read_parquet(paste0(sample_path,"Master Sample File/2025.12.10_master_HACE_list_post_mailout.parquet"), 
                        col_select = c("IQVIA_flagdate","nhscr_date","NHSCR_reason","chili_date","chili_reason",
                                       "IQVIA_exclude","pre_survey_exclusion","reason","primary_exclusion_source"))
sample <- sample %>% 
  rename_with(tolower)

#CH - do these need to go into an output file?
table(master_list$iqvia_flagdate)
table(master_list$nhscr_reason)
table(master_list$chili_date)
table(master_list$chili_reason)
table(master_list$pre_survey_exclusion)
table(master_list$reason)
table(master_list$primary_exclusion_source)
table(master_list$iqvia_flagdate,master_list$reason)

#Total No Sampled
Line1 <- sample %>%
  group_by("Mailout Stage" = "Total No Sampled") %>% 
  summarise(count = n())

#No Removed Pre Survey and Day 1 Mail Out (Deaths)
Line2 <- sample %>%
  filter(iqvia_flagdate <= as.Date("2025-10-22") & reason == "Death") %>% 
  group_by("Mailout Stage" = "No Removed Pre Survey and Day 1 Mail Out (Deaths)") %>% 
  summarise(count = n())

#No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)
Line3 <- sample %>%
  filter(iqvia_flagdate <= as.Date("2025-10-22") & reason != "Death")%>% 
  group_by("Mailout Stage" = "No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)") %>% 
  summarise(count = n())

#Total No Sent out
Line4 <- sample %>%
  filter(iqvia_flagdate > as.Date("2025-10-22") | is.na(iqvia_flagdate))%>% 
  group_by("Mailout Stage" = "Total No Sent out on day 1 of mail out (22/10/25)") %>% 
  summarise(count = n())

#Line5 Total exclusions (Rest of Initial Mail Out)
Line5 <- sample %>%
  filter(iqvia_flagdate %in% c("2025-10-23","2025-10-24","2025-10-27"))  %>% 
  group_by("Mailout Stage" = "Total Exclusions (Rest of Initial Mail Out phase)") %>% 
  summarise(count = n())

#Line6 Total No of Removals Sent to QH (as part of the reminder mail out)
Line6 <- sample %>%
  filter(iqvia_flagdate > c("2025-10-27")) %>% 
  group_by("Mailout Stage" = "Total No of Removals Sent to QH (as part of reminder mail out phase)") %>% 
  summarise(count = n())

removals <-bind_rows(Line1,Line2,Line3,Line4,Line5,Line6)

#Pick up reporting template and populate, save outfile as xlsx####

template <- loadWorkbook(paste0(output_path,"technical_report/removals_template.xlsx"))
writeData(template, "removals_summary", removals, startCol = 2, startRow = 9)
saveWorkbook(template,paste0(output_path,"technical_report/sample_removals_note_",today(),".xlsx"), overwrite =TRUE)
