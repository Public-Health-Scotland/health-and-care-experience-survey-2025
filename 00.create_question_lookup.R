# Name of file: 00.create_question_lookup.R
# 
# Description of content:  Create metadata for each question in HACE survey
# 
# Approximate run time: <1 min
# 
# Approximate memory usage: 1.26 GiB

#Inputs: lookup_path,"HACE_2025_question_mapping.xlsx"

#Outputs:
#"lookups/question_lookup_info.rds"
#"lookups/question_lookup_pnn.rds"
#"lookups/question_lookup.rds"
#"lookups/questions.rds"
#"lookups/information_questions.rds"
#"lookups/percent_positive_questions.rds"
#"lookups/information_questions_tata.rds"

#CH comments. 
#What are the different sheets in the lookup document?
#What does Comparability = Tableau mean?
#Similarly previous scripts used surveysection - now topic 
#Should we be weighting the chronic pain question this year? Presently it isn't weighted
#Filter out where iref is '-'?
#How to deal with Q38  In general, how well do you feel that you are able to look after your own health? In mapping document as PPN question, but preeviously treated as both

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")

#Read in document
question_mapping <- read_xlsx(paste0(lookup_path,"HACE_2025_question_mapping.xlsx"),
                              sheet = "HACE 2025-26",na = "", trim_ws = TRUE)

question_mapping <- question_mapping %>%
  mutate(across(everything(), as.character))%>%
  rename_with(tolower) %>% 
  filter(!is.na(question))

information_questions_tata <- unique(substr(question_mapping$question,1,3)[question_mapping$`tick all that apply (tata)` == "Y"])

question_lookup <- question_mapping %>%
  filter(!iref == "-") %>% 
  mutate(question_2024 = if_else(`comparability` %in% c("Dashboard","Commentary"),`quest. no. prev year`,""),
         response_code_2024 = if_else(`comparability` %in% c("Dashboard","Commentary"),`response option…2023-24`,""),
         question_2022 = if_else(`comparability` %in% c("Dashboard","Commentary"),`compare to…2021-22`,""),
         response_code_2022 = if_else(`comparability` %in% c("Dashboard","Commentary"),`response option…2021-22`,""),
         question_2020 = if_else(`comparability` %in% c("Dashboard","Commentary"),`compare to…2019-20`,""),
         response_code_2020 = if_else(`comparability` %in% c("Dashboard","Commentary"),`response option…2019-20`,""),
         question_2018 = if_else(`comparability` %in% c("Dashboard","Commentary"),`compare to…2017-18`,""),
         response_code_2018 = if_else(`comparability` %in% c("Dashboard","Commentary"),`response option…2017-18`,""))%>%
  #recoding to deal with tata type questions, can't have identical responses text
  mutate(response_text_analysis = case_when(substr(question,1,3) %in% information_questions_tata & response_text == "No" ~ "No", TRUE ~ response_text_dashboard)) %>% 
  #recoding to deal with non-dashboard questions, can't have empty text
  mutate(response_text_analysis = case_when(response_text_analysis == "-" ~ response_text, TRUE ~ response_text_analysis)) %>% 
  #recoding to deal with PPN questions
  mutate(response_text_analysis = case_when(question_type == "Percent positive" & grepl("positive",processing) == TRUE ~ "Positive",
                                            question_type == "Percent positive" & grepl("negative",processing) == TRUE ~ "Negative",
                                            question_type == "Percent positive" & grepl("neutral",processing) == TRUE ~ "Neutral", 
                                            question_type == "Percent positive" & grepl("exclude",processing) == TRUE ~ "Exclude",
                                            TRUE ~ response_text_analysis))%>%
  select(iref,question,question_text,weight,response_code,response_text_analysis,topic,question_2024,response_code_2024,
         question_2022,response_code_2022,question_2020,response_code_2020,question_2018,response_code_2018)

#check if the same as before, then save
hist.file <- readRDS(paste0(lookup_path,"question_lookup.rds"))
all.equal(hist.file,question_lookup)
saveRDS(question_lookup, paste0(lookup_path,"question_lookup.rds"))

#create vectors of percent positive / information questions
percent_positive_questions <- unique(question_mapping$question[question_mapping$question_type == "Percent positive"])
information_questions <- unique(question_mapping$question[question_mapping$question_type == "Information"])
questions <- question_mapping$question[(question_mapping$question_type %in% c("Percent positive","Information"))]

#check if the same as before
hist.file <- readRDS(paste0(lookup_path,"questions.rds"))
all.equal(hist.file,questions)
hist.file <- readRDS(paste0(lookup_path,"information_questions.rds"))
all.equal(hist.file,information_questions)
hist.file <- readRDS(paste0(lookup_path,"percent_positive_questions.rds"))
all.equal(hist.file,percent_positive_questions)
hist.file <- readRDS(paste0(lookup_path,"information_questions_tata.rds"))
all.equal(hist.file,information_questions_tata)

saveRDS(questions, paste0(lookup_path,"questions.rds"))
saveRDS(information_questions, paste0(lookup_path,"information_questions.rds"))
saveRDS(percent_positive_questions, paste0(lookup_path,"percent_positive_questions.rds"))
saveRDS(information_questions_tata, paste0(lookup_path,"information_questions_tata.rds"))
