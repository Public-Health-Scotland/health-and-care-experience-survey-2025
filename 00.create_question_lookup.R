# Name of file: 00.create_question_lookup.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Create metadata for each question in HACE survey
# 
# Approximate run time: <1 min
# 
# Approximate memory usage: 1.26 GiB

#Inputs: "lookup_path,"ASDHD - Health and Care Experience Survey - 2025 - 2026_v1.xlsx"

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
#Previous scripts used response_option - now response_code so we will need to re-write to adjust
#Similarly previous scripts used surveysection - now topic 
#Should we be weighting the chronic pain question this year? Presently it isn't weighted
#Filter out where iref is '-'?

#This section is for checking purposes - to be deleted
information_questions_tata_2324 <- readRDS(paste0(lookup_path_202324,"information_questions_tata.rds"))
question_lookup_info_2324 <- readRDS(paste0(lookup_path_202324,"question_lookup_info.rds"))
question_lookup_pnn_2324 <- readRDS(paste0(lookup_path_202324,"question_lookup_pnn.rds"))
question_lookup_2324 <- readRDS(paste0(lookup_path_202324,"question_lookup.rds"))
questions_2324 <- readRDS(paste0(lookup_path_202324,"questions.rds"))
information_questions_2324 <- readRDS(paste0(lookup_path_202324,"information_questions.rds"))
percent_positive_questions_2324 <- readRDS(paste0(lookup_path_202324,"percent_positive_questions.rds"))
                                        
#"lookups/question_lookup_info.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")

#Read in document

question_mapping <- read_xlsx(paste0(lookup_path,"HACE_2025_question_mapping.xlsx"),
                              sheet = "HACE 2025-26",na = "", trim_ws = TRUE)

question_mapping <- question_mapping %>%
  mutate(across(everything(), as.character))%>%
  rename_with(tolower) %>% 
  filter(!is.na(question))

table(question_mapping$comparability)

question_lookup_info <- question_mapping %>%
  filter(question_type == "Information" | question == "q38")%>% #- Q38 is In general, how well do you feel ... treated as both ppn and info 
  mutate(question_2024 = if_else(`comparability` %in% c("Dashboard","Commentary"),`quest. no. prev year`,""))%>%
  mutate(response_text = str_replace(response_text,", please write in:",""),
         response_text = str_replace(response_text,"^ ","")) %>% 
  select(question,question_text,weight,response_code,response_text,topic,question_2024)

# #Update q41 weight from "No_Weight" to "T5_Wt_Final" to weight up response to question (new weight "T5_Wt_Final" is equal to q38 weight)
# question_lookup_info <- question_lookup_info %>%
#   mutate(weight = if_else(question == "q41" & weight == "No_Weight","T5_Wt_Final",weight))

information_questions_tata <- unique(substr(question_mapping$question,1,3)[question_mapping$`tick all that apply (tata)` == "Y"])

#check if the same as before, then save
hist.file <- readRDS(paste0(lookup_path,"question_lookup_info.rds"))
all.equal(hist.file,question_lookup_info)
saveRDS(question_lookup_info, paste0(lookup_path,"question_lookup_info.rds"))

question_lookup_pnn <- question_mapping %>%
  filter(question_type == "Percent positive")%>%
  mutate(question_2024 = if_else(`comparability` %in% c("Dashboard","Commentary"),`quest. no. prev year`,""))%>%
  mutate(response_text_analysis = if_else(grepl("positive",processing) == TRUE, "Positive",if_else(grepl("negative",processing) == TRUE,"Negative","Neutral")))%>%
  select(question,question_text,weight,response_code,response_text,response_text_analysis,topic,question_2024)

#check if the same as before, then save
hist.file <- readRDS(paste0(lookup_path,"question_lookup_pnn.rds"))
all.equal(hist.file,question_lookup_pnn)
saveRDS(question_lookup_pnn, paste0(lookup_path,"question_lookup_pnn.rds"))

#Create combined lookup to analyse all questions together in aggregate results script.
question_lookup <- question_lookup_info %>% 
  filter(question != "q38") %>% 
  bind_rows(question_lookup_pnn) %>% 
  mutate(response_text_analysis = case_when(is.na(response_text_analysis)~response_text,TRUE ~ response_text_analysis)) # for aggregate_results.R
#check if the same as before
hist.file <- readRDS(paste0(lookup_path,"question_lookup.rds"))
all.equal(hist.file,question_lookup)
saveRDS(question_lookup, paste0(lookup_path,"question_lookup.rds"))

#create vectors of percent positive / information questions
percent_positive_questions <- unique(question_lookup_pnn$question)
information_questions <- unique(question_lookup_info$question)
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
