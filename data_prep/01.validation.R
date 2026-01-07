## Written by Catriona Haddow and Martin Leitch
# WIP: November 2025
# *****************************************

###################################################################################

#Purpose: Reads in patient level data for HACE 2025 Survey and applies and checks validation rules

#Inputs: "HA23_Data_FINAL_V1.xlsx"  #UPDATE!

#Outputs: #UPDATE!
#"Final_unrouted_data.Rds"
#"anonymised_unvalidated_response_data_for_SG.rds"
#"anonymised_unvalidated_response_data_for_SG.csv"
#"rules_summary.xlsx"
#"data_Validated results.rds"
#"data_Validated results.xlsx"
#"anonymised_data_Validated results for SG.rds"
#"anonymised_data_Validated results for SG.csv"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#Step 1: Read in contractor data#### 
#opening final set of unrouted results received from contractor.
#CH can we get file creation time?
contractor_data <- read.xlsx(paste0(data_path,"Results from Contractor/HA25_Interim_v1.xlsx"), sheet = "HA25_DATA")

#Rename Variables as necessary: 
contractor_data <- contractor_data %>% 
  rename_with(tolower)%>% 
  rename_with(.fn = ~ paste0("q", .),   .cols = matches("^\\d")) %>%      # Rename columns which start with a digit - these are the questions
  rename(qh_psid = participant.id,#pnumber last year
         responsereportingdatetime = response.date.time,
         responsecode = response.code,
         responsesubcode = response.sub.code) %>% #need response sub code too?
  mutate(responsereportingdatetime = as.Date(responsereportingdatetime, origin = "1899-12-30"), #reformat excel data
         qh_psid = paste0("P", qh_psid),#Update PSID to include P
         patientid = as.character(patientid)) #are there others this needs to apply to?

#variables for file summary
summary_file_name <-"HA25_Interim_v1.xls"
summary_duplicates <- sum(duplicated(contractor_data$patientid))
summary_variables <- sapply(contractor_data, class)#Check classification of each column
response_codes <- tabyl(contractor_data,responsecode,responsesubcode)
summary_record_count <- nrow(contractor_data) 

#add patientid_SG
index <- c(rep(1:nrow(contractor_data)))
contractor_data <- contractor_data %>% 
  mutate(patientid_sg = paste0("Pat",str_pad(index,6,c("left"),pad = "0"))) %>% 
  relocate(patientid_sg, .after = patientid)

#check if the same as before
hist.file <- readRDS(paste0(data_path,"Results from Contractor/interim_unrouted_data.rds"))
all.equal(hist.file,contractor_data) 
#Save out reformatted data
saveRDS(contractor_data, file=paste0(data_path,"Results from Contractor/interim_unrouted_data.rds") )

#Create anonymised version of unvalidated data as received from QH for SG:
SGFile <- contractor_data %>% 
  select(-c(qh_psid,patientid)) #Remove PSID(QH patient identifier) & PatientID (PHS patient identifier)

#check if the same as before
hist.file <- readRDS(paste0(data_path,"Results from Contractor/anonymised_unvalidated_response_data_for_SG.rds"))
all.equal(hist.file,SGFile)
#Save out anonymised version of unvalidated data for SG
saveRDS(SGFile, file=paste0(data_path,"Results from Contractor/anonymised_unvalidated_response_data_for_SG.rds"))
#write_excel_csv(SGFile, "data/Results from Contractor/Final data/anonymised_unvalidated_response_data_for_SG.csv") 

#Step 2: Read in reformatted responses####
contractor_data <- readRDS(paste0(data_path,"Results from Contractor/interim_unrouted_data.rds"))

contractor_data <- contractor_data %>% 
  select(-c(q18,q26)) #Drop the comments columns which are all blank

#Outputs the frequencies of all the question responses
pre_validation_freq <- apply(contractor_data[questions], MARGIN=2, table)
questions_in_data <- names(contractor_data)[startsWith(names(contractor_data), "q")]
questions_in_lookup_not_data <- questions[!questions %in% questions_in_data] #this should be empty
questions_in_data_not_lookup <- questions_in_data[!questions_in_data %in% questions] #this should have only 'other' questions

#Step 3: Apply validation rules####

#Rule 1: FOR QH ONLY: ####
#'Tick one box only’ questions: if respondent selects more than one box, then question is cleared. The majority of questions are this type, 
#so it’s easier to list the questions that this does not apply to: Q11, Q20, Q29, Q30, Q36, Q37 and Q40.

#===
#Rule 2: When did you last contact the GP Practice named on the enclosed letter?####
rule_table <- data.frame("rule" = c("Rule 2a"), #Set up rule table
                         "rule_label" = c("If Q1 is blank, and Q2 is not blank – set Q1 to 1"),
                         "value" = sum(is.na(contractor_data$q01) & !is.na(contractor_data$q02),na.rm = TRUE))

Rule02a_pre <- lapply("q02", crosstabs_f,"q01")  #Frequencies before implementing rule

contractor_data <- contractor_data %>% 
  mutate(q01 = if_else(is.na(q01) & !is.na(q02),1,q01))# implement rule

Rule02a_post <- lapply("q02", crosstabs_f,"q01") #Check frequencies after implementing rule

## b > If Q1 <> 1 and Q2 to Q17 are not all blank – set Q2 to Q17 to blank.#### Note that NA needs to be explicit, as NA <>!= 1 
q2toq17 <- subset_qs(2,17)

rule_table <- rule_table %>%
  add_row("rule" = c("Rule 2b"),
          "rule_label" = c("If Q1 <> 1 and Q2 to Q17 are not all blank – set Q2 to Q17 to blank"),
          "value" = sum(ifelse((contractor_data$q01 !=1|is.na(contractor_data$q01)) & rowAny(contractor_data[q2toq17]),1,0),na.rm = TRUE))

Rule02b_pre <- lapply(q2toq17, crosstabs_f,"q01")  #Frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q2toq17),~ case_when(q01 != 1 ~ NA,is.na(q01) ~ NA,TRUE ~ .)))

Rule02b_post <- lapply(q2toq17, crosstabs_f,"q01")  #Frequencies after implementing rule

#===
#Rule 3: The last time you needed an appointment with your general practice, what kind of appointment…?####
#If Q7 = 7 (or blank) and Q8 to Q9 are not all blank – set Q7 to Q9 to blank.
q8toq9 <-subset_qs(8,9)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 3"),
          "rule_label" = c("If Q7 = 7 (or blank) and Q8 to Q9 are not all blank – set Q8 to Q9 to blank"),
          "value" = sum(ifelse(contractor_data$q07 %in% c(7,NA) & rowAny(contractor_data[q8toq9]),1,0)))

Rule03_pre <- lapply(q8toq9, crosstabs_f,"q07")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q8toq9),~ case_when(q07 %in% c(7,NA) ~ NA,TRUE ~ .)))

Rule03_post <- lapply(q8toq9, crosstabs_f,"q07")  #Check frequencies after implementing rule

#Rule 4: The last time you needed to see or speak to a doctor or nurse from your GP Practice quite urgently, how long did you have to wait? ####
## 4a If Q10 = 1, 2 or 4 and Q11 is not blank – set Q11 to blank.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 4a"),
          "rule_label" = c("If Q10 = 1, 2 or 4 or blank and Q11 is not blank – set Q11 to blank."),
          "value" = sum(ifelse(contractor_data$q10 %in% c(1,2,4) & !is.na(contractor_data$q11),1,0)))

Rule04a_pre <- lapply("q10", crosstabs_f,"q11")   #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q11= case_when(q10 %in% c(1,2,4) ~ NA,TRUE ~ q11))

Rule04a_post <- lapply("q10", crosstabs_f,"q11")   #Check frequencies after implementing rule

## 4b > If Q10 is blank and Q11 is not blank – set Q10 to 3.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 4b"),
          "rule_label" = c("If Q10 is blank and Q11 is not blank – set Q10 to 3."),
          "value" = sum(ifelse(is.na(contractor_data$q10) & !is.na(contractor_data$q11),1,0)))

Rule04b_pre <- lapply("q10", crosstabs_f,"q11")   #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q10= case_when(is.na(q10) & !is.na(q11) ~ 3,TRUE ~ q10))

Rule04b_post <- lapply("q10", crosstabs_f,"q11")   #Check frequencies after implementing rule

#Rule 5: the last time you received treatment or advice at your General Practice in the last 12 months. What was it for? ####

q14atoq14e <-c("q14a","q14b","q14c","q14d","q14e")
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 5a"),
          "rule_label" = c("If Q14f = 1 and any of Q14a to Q14e = 1 – set Q14f to blank."),
          "value" = sum(if_else(contractor_data$q14f ==1 & rowAny(contractor_data[q14atoq14e]),1,0),na.rm = TRUE))

Rule05a_pre <- lapply(q14atoq14e, crosstabs_f,"q14f")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q14f = case_when(rowAny(contractor_data[q14atoq14e]) ~ NA,TRUE ~ q14f))

Rule05a_post <- lapply(q14atoq14e, crosstabs_f,"q14f")  #Check frequencies after implementing rule

##b > If Q14f = 1 and Q15 to Q17 are not all blank – set Q15 to Q17 to blank. 

q15toq17 <-subset_qs(15,17)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 5b"),
          "rule_label" = c("If Q14f = 1 and Q15 to Q17 are not all blank – set Q15 to Q17 to blank."),
          "value" = sum(if_else(contractor_data$q14f ==1 & rowAny(contractor_data[q15toq17]),1,0),na.rm = TRUE))

Rule05b_pre <- lapply(q15toq17, crosstabs_f,"q14f")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q15toq17),~ case_when(q14f == 1 ~ NA,TRUE ~ .)))

Rule05b_post <- lapply(q15toq17, crosstabs_f,"q14f")  #Check frequencies after implementing rule

## c > If none of Q14a-e = 1 then set Q15 to Q17 to blank.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 5c"),
          "rule_label" = c("If none of Q14a-e = 1 then set Q15 to Q17 to blank."),
          "value" = sum(if_else(rowAll(contractor_data[q14atoq14e]) & rowAny(contractor_data[q15toq17]),1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #add temporary helper variable to dataset
  mutate(q14atoq14e_any = case_when(if_any(all_of(q14atoq14e)) == 1 ~ 1,TRUE ~ 0))

Rule05c_pre <- lapply(q15toq17, crosstabs_f,"q14atoq14e_any")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q15toq17),~ case_when(q14atoq14e_any == 0 ~ NA,TRUE ~ .)))

Rule05c_post <- lapply(q15toq17, crosstabs_f,"q14atoq14e_any")  #Check frequencies after implementing rule

contractor_data <- contractor_data %>% select(-q14atoq14e_any) #drop helper variable
  
#Rule 6: In the past 12 months, have you contacted an NHS service when you wanted to see a healthcare professional, but your General Practice was closed? ####
## a > If Q19 = 2 and Q20 to Q25 are not all blank – set Q20 to Q25 to blank.

q20toq25 <- subset_qs(20,25)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 6a"),
          "rule_label" = c("If Q19 = 2 and Q20 to Q25 are not all blank – set Q20 to Q25 to blank."),
          "value" = sum(ifelse(contractor_data$q19 ==2 & rowAny(contractor_data[q20toq25]),1,0),na.rm = TRUE))

Rule06a_pre <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q20toq25),~ case_when(q19 == 2 ~ NA,TRUE ~ .)))

Rule06a_post <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies after implementing rule

## 6b > If Q19 is blank and Q20 to Q25 are not all blank – set Q19 to 1.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 6b"),
          "rule_label" = c("If Q19 is blank and Q20 to Q25 are not all blank – set Q19 to 1."),
          "value" = sum(ifelse(is.na(contractor_data$q19) & rowAny(contractor_data[q20toq25]),1,0),na.rm = TRUE))

Rule06b_pre <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q19 = case_when(is.na(contractor_data$q19) & rowAny(contractor_data[q20toq25]) ~ 1, TRUE ~ q19))

Rule06b_post <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies after implementing rule

#Rule 7: In the past 12 months, have you had any help or support with everyday living?####
#Rule 7: If you are not receiving all the help and care services for everyday living that you feel you need, which options describe your situation? Please tick all that apply. NEW 

## a > If Q27g = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27g to blank.

q27atoq27f <- c("q27a","q27b","q27c","q27d","q27e","q27f")

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 7a"),
          "rule_label" = c("If Q27g = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27g to blank."),
          "value" = sum(ifelse(contractor_data$q27g ==1 & rowAny(contractor_data[q27atoq27f]),1,0),na.rm = TRUE))

Rule07a_pre <- lapply(q27atoq27f, crosstabs_f,"q27g")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q27g = if_else(rowAny(contractor_data[q27atoq27f]),NA,q27g))

Rule07a_post <- lapply(q27atoq27f, crosstabs_f,"q27g")  #Check frequencies after implementing rule

## b > If Q27h = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27h to blank.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 7b"),
          "rule_label" = c("If Q27h = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27h to blank."),
          "value" = sum(ifelse(contractor_data$q27h ==1 & rowAny(contractor_data[q27atoq27f]),1,0),na.rm = TRUE))

Rule07b_pre <- lapply(q27atoq27f, crosstabs_f,"q27h")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule: 
  mutate(q27h = if_else(rowAny(contractor_data[q27atoq27f]),NA,q27h))

Rule07b_post <- lapply(q27atoq27f, crosstabs_f,"q27h")  #Check frequencies after implementing rule

## c > If Q27g = 1 and Q27h = 1 – set Q27h to blank.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 7c"),
          "rule_label" = c("If Q27g = 1 and Q27h = 1 – set Q27h to blank."),
          "value" = sum(ifelse(contractor_data$q27g ==1 & contractor_data$q27h == 1,1,0),na.rm = TRUE))

Rule07c_pre <- lapply("q27g", crosstabs_f,"q27h")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q27h = case_when(q27g == 1 ~ NA,TRUE ~ q27h))

Rule07c_post <- lapply("q27g", crosstabs_f,"q27h")  #Check frequencies after implementing rule

## d > If Q27g = 1 – set Q28 to Q31 to blank.

q28toq31 <- subset_qs(28,31)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 7d"),
          "rule_label" = "If Q27g = 1 – set Q28 to Q31 to blank.",
          "value" = sum(if_else(contractor_data$q27g ==1 & rowAny(contractor_data[q28toq31]),1,0),na.rm = TRUE))

Rule07d_pre <- lapply(q28toq31, crosstabs_f,"q27g")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q28toq31),~ case_when(q27g ==1 ~ NA,TRUE ~ .)))

Rule07d_post <- lapply(q28toq31, crosstabs_f,"q27g")  #Check frequencies after implementing rule

## e > If Q27h = 1 – set Q28 to Q32 to blank.
q28toq32 <- subset_qs(28,32)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 7e"),
          "rule_label" = "If Q27h = 1 – set Q28 to Q32 to blank.",
          "value" = sum(if_else(contractor_data$q27h ==1 & rowAny(contractor_data[q28toq32]),1,0),na.rm = TRUE))

Rule07e_pre <- lapply(q28toq32, crosstabs_f,"q27h")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q28toq32),~ case_when(q27h ==1 ~ NA,TRUE ~ .)))

Rule07e_post <- lapply(q28toq32, crosstabs_f,"q27h")  #Check frequencies after implementing rule

## f > If Q27a to Q27h are all blank (and Q28 to Q32 are not all blank) - set Q28 to Q32 to blank
#This is, if respondent hasn't completed this routing question, blank rest of section.

q27atoq27h <- c("q27a","q27b","q27c","q27d","q27e","q27f","q27g","q27h")
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 7f"),
          "rule_label" = "If Q27a to Q27h are all blank (and Q28 to Q32 are not all blank) - set Q28 to Q32 to blank",
          "value" = sum(ifelse((rowAll(contractor_data[q27atoq27h])
                               & rowAny(contractor_data[q28toq32])),1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #add temporary helper variable to dataset
  mutate(q27atoq27h_any = case_when(if_any(all_of(q27atoq27h)) == 1 ~ 1,TRUE ~ 0))

Rule07f_pre <- lapply(q28toq32, crosstabs_f,"q27atoq27h_any")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule
  mutate(across(all_of(q28toq32), ~replace(., rowAll(contractor_data[q27atoq27h]), NA)))

Rule07f_post <- lapply(q28toq32, crosstabs_f,"q27atoq27h_any")  #Check frequencies after implementing rule

contractor_data <- contractor_data %>% select(-q27atoq27h_any) #drop helper variable

#Rule 8: Do you look after, or give any regular help or support, to …..?####
# > If Q33 not in (1,2,3,4,5) and Q34 to Q37 are not all blank – set Q34 to Q37 to blank.

q34toq37 <-subset_qs(34,37)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 8"),
          "rule_label" = "If Q33 not in (1,2,3,4,5) and Q34 to Q37 are not all blank – set Q34 to Q37 to blank",
          "value" = sum(ifelse(contractor_data$q33  %in% c(6,NA) & rowAny(contractor_data[q34toq37]),1,0),na.rm = TRUE))

Rule08_pre <- lapply(q34toq37, crosstabs_f,"q33")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q34toq37),~ case_when(q33 %in% c(6,NA) ~ NA,TRUE ~ .)))

Rule08_post <- lapply(q34toq37, crosstabs_f,"q33")  #Check frequencies after implementing rule

#Rule 9. Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more? 
#If Q39 <> 1 (this is equivalent to 2 or NA) (and Q40 & Q41 are not all blank ) - set Q40 and Q41 to blank.  
q40toq41 <-subset_qs(40,41)
q40 <-subset_qs(40,40)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 9"),
          "rule_label" = "If Q39 <> 1 (and Q40 & Q41 are not all blank ) - set Q40 and Q41 to blank",
          "value" = sum(ifelse(contractor_data$q39 %in% c(2,NA) & rowAny(contractor_data[q40toq41]),1,0),na.rm = TRUE))

Rule09_pre <- lapply(q40toq41, crosstabs_f,"q39")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q40toq41),~ case_when(q39 %in% c(2,NA) ~ NA,TRUE ~ .)))

Rule09_post <- lapply(q40toq41, crosstabs_f,"q39")  #Check frequencies after implementing rule

#Rule 9.	Do you have any of the following?####
##a > If all of Q39a to Q32k are blank, and Q32jOther has text, set Q32j = 1 
# Q39. Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more? 
#   
#   If they do not tick yes to this question, clear Q40 & Q41. 
# 
# If Q39 <> 1 (and Q40 & Q41 are not all blank ) - set Q40 and Q41 to blank.  
# q34toq37 <-subset_qs(34,37)
# 
# rule_table <- rule_table %>% 
#   add_row("rule" = c("Rule 10a"),
#           "rule_label" = "If all of Q39a to Q32k are blank, and Q32jOther has text, set Q32j = 1",
#           "value" = sum(ifelse(rowAll(contractor_data[q39]) & !is.na(contractor_data$q39jOther),1,0),na.rm = TRUE))
# 
# contractor_data <- contractor_data %>% #Add helper variable
#   mutate(q39jOther_text = case_when(is.na(contractor_data$q39jOther) ~ "No text",TRUE ~ "Text"))
# 
# Rule10a_pre <- lapply(q39, crosstabs_f,"q39jOther_text")  #Check frequencies before implementing rule
# 
# contractor_data <- contractor_data %>% #Implement rule:
#   mutate(q39j= case_when(rowAll(contractor_data[q39]) & !is.na(contractor_data$q39jOther) ~ 1, TRUE ~ q39j))
# 
# Rule10a_post <- lapply(q39, crosstabs_f,"q39jOther_text")  #Check frequencies after implementing rule

# contractor_data <- contractor_data %>%  select(-q39jOther_text)#Drop helper variable
# 
# ##b > If any of Q39a to Q39j = 1 and Q39k = 1 – set Q39k to blank. 
# q39atoq39j <- q39[q39 != "q39k"]
# rule_table <- rule_table %>% 
#   add_row("rule" = c("Rule 10b"),
#           "rule_label" = "If any of Q39a to Q39j = 1 and Q39k = 1 – set Q39k to blank",
#           "value" = sum(if_else(rowAny(contractor_data[q39atoq39j]) & contractor_data$q39k == 1,1,0),na.rm = TRUE))
# 
# Rule10b_pre <- lapply(q39atoq39j, crosstabs_f,"q39k")  #Check frequencies before implementing rule
# 
# contractor_data <- contractor_data %>% #Implement rule:
#   mutate(q39k= case_when(rowAny(contractor_data[q39atoq39j]) ~ NA, TRUE ~ q39k))
# 
# Rule10b_post <- lapply(q39atoq39j, crosstabs_f,"q39k")  #Check frequencies after implementing rule
# 
# #Rule 11: Which of the following best describes your sexual orientation? #### 
# ##> If Q42 is blank and Q42 text box (q42Other) is not blank then q42 = 4.
# rule_table <- rule_table %>% 
#   add_row("rule" = c("Rule 11"),
#           "rule_label" = "If Q42 is blank and Q42 text box (q42Other) is not blank then q42 = 4",
#           "value" = sum(if_else(is.na(contractor_data$q42) & !is.na(contractor_data$q42Other),1,0),na.rm = TRUE))
# 
# contractor_data <- contractor_data %>% #Add helper variable
#   mutate(q42Other_text = case_when(is.na(contractor_data$q42Other) ~ "No text",TRUE ~ "Text"))
# 
# Rule11_pre <- lapply("q42", crosstabs_f,"q42Other_text")  #Check frequencies before implementing rule
# 
# contractor_data <- contractor_data %>% #Implement rule:
#   mutate(q42= case_when(is.na(contractor_data$q42) & !is.na(contractor_data$q42Other) ~ 4, TRUE ~ q42))
# 
# Rule11_post <- lapply("q42", crosstabs_f,"q42Other_text")  #Check frequencies after implementing rule
# 
# contractor_data <- contractor_data %>% select(-q42Other_text) #Drop helper variable
# 
# #===
# #Rule 12: What best describes your ethnic group? Please tick one box only#### 
# ##> If Q43 is blank and Q43 text box (q43Other) is not blank then q43 = 6.
# 
# rule_table <- rule_table %>% 
#   add_row("rule" = c("Rule 12"),
#           "rule_label" = "If Q43 is blank and Q43 text box (q43Other) is not blank then q43 = 6",
#           "value" = sum(if_else(is.na(contractor_data$q43) & !is.na(contractor_data$q43Other),1,0),na.rm = TRUE))
# 
# contractor_data <- contractor_data %>% #Add helper variable
#   mutate(q43Other_text = case_when(is.na(contractor_data$q43Other) ~ "No text",TRUE ~ "Text"))
# 
# Rule12_pre <- lapply("q43", crosstabs_f,"q43Other_text")  #Check frequencies before implementing rule
# 
# contractor_data <- contractor_data %>% #Implement rule:
#   mutate(q43= case_when(is.na(contractor_data$q43) & !is.na(contractor_data$q43Other) ~ 6, TRUE ~ q43))
# 
# Rule12_post <- lapply("q43", crosstabs_f,"q43Other_text")  #Check frequencies after implementing rule
# 
# contractor_data <- contractor_data %>% select(-q43Other_text) #Drop helper variable
# 
# # #Rule 13: What religion, religious denomination or body do you belong to? #### 
# ##> If Q44 is blank and Q44 text box (q44Other) is not blank then q44 = 11.
# rule_table <- rule_table %>% 
#   add_row("rule" = c("Rule 13"),
#           "rule_label" = "If Q44 is blank and Q44 text box (q44Other) is not blank then q44 = 11",
#           "value" = sum(if_else(is.na(contractor_data$q44) & !is.na(contractor_data$q44Other),1,0),na.rm = TRUE))
# 
# contractor_data <- contractor_data %>% #Add helper variable
#   mutate(q44Other_text = case_when(is.na(contractor_data$q44Other) ~ "No text",TRUE ~ "Text"))
# 
# Rule13_pre <- lapply("q44", crosstabs_f,"q44Other_text")  #Check frequencies before implementing rule
# 
# contractor_data <- contractor_data %>% #Implement rule:
#   mutate(q44= case_when(is.na(contractor_data$q44) & !is.na(contractor_data$q44Other) ~ 11, TRUE ~ q44))
# 
# Rule13_post <- lapply("q44", crosstabs_f,"q44Other_text")  #Check frequencies after implementing rule
# 
# contractor_data <- contractor_data %>% select(-q44Other_text) #Drop helper variable

#This outputs the frequencies of all the question responses
post_validation_freq <- apply(contractor_data[questions], MARGIN=2, table)

#TATA Rule: Apply Tick all that apply processing rule. ####
#Add in variables to get 'tick all that apply' totals 
#Blanks in TATA variables should be set to "No" (0) unless none of the response options were ticked, in which case all response options should be set to NA.
information_questions_tata
contractor_data <- contractor_data %>%
  mutate(q08 = case_when(rowAny(across(all_of(subset_qs(8,8))))~ 1, TRUE ~ 0), #if any of the q9 questions are not zero or NA, then q09 (total) = 1, else it is 0
         q14 = case_when(rowAny(across(all_of(subset_qs(14,14)))) ~ 1,TRUE ~ NA), 
         q20 = case_when(rowAny(across(all_of(subset_qs(20,20)))) ~ 1,TRUE ~ NA), 
         q27 = case_when(rowAny(across(all_of(subset_qs(27,27)))) ~ 1,TRUE ~ NA), 
         q28 = case_when(rowAny(across(all_of(subset_qs(28,28)))) ~ 1,TRUE ~ NA),
         q32 = case_when(rowAny(across(all_of(subset_qs(32,32)))) ~ 1,TRUE ~ NA), 
         q34 = case_when(rowAny(across(all_of(subset_qs(34,34)))) ~ 1,TRUE ~ NA), 
         q35 = case_when(rowAny(across(all_of(subset_qs(35,35)))) ~ 1,TRUE ~ NA),
         q39 = case_when(rowAny(across(all_of(subset_qs(39,39)))) ~ 1,TRUE ~ NA)) %>% 
  mutate(across(all_of(subset_qs(9,9)), function(x) if_else(q09 == 1, replace_na(x,0),x)), #if q09 (total) is 1, recode all of the q9 questions from NA to 0
         across(all_of(subset_qs(14,14)), function(x) if_else(q14 == 1, replace_na(x,0),x)),
         across(all_of(subset_qs(20,20)), function(x) if_else(q20 == 1, replace_na(x,0),x)),
         across(all_of(subset_qs(27,27)), function(x) if_else(q27 == 1, replace_na(x,0),x)),
         across(all_of(subset_qs(28,28)), function(x) if_else(q28 == 1, replace_na(x,0),x)),
         across(all_of(subset_qs(32,32)), function(x) if_else(q32 == 1, replace_na(x,0),x)),
         across(all_of(subset_qs(34,34)), function(x) if_else(q34 == 1, replace_na(x,0),x)),
         across(all_of(subset_qs(35,35)), function(x) if_else(q35 == 1, replace_na(x,0),x)),
         across(all_of(subset_qs(39,39)), function(x) if_else(q39 == 1, replace_na(x,0),x)))

#Create rule summary####

# Get list of Rules objects"
rule_list <- sort(c(ls(pattern = "^Rule.*pre"),ls(pattern = "^Rule.*post")))

write_out_list_f <- function(x) {  # function to write list of tables to single excel sheet
  curr_row <- 1
for(i in seq_along(x)) {
  writeData(template, deparse(substitute(x)),names(x)[i], startCol = 1, startRow = curr_row)
  writeData(template, deparse(substitute(x)),x[[i]], startCol = 1, startRow = curr_row+1)
  curr_row <- curr_row + nrow(x[[i]]) + 2
}
}

###########################################################################################################################################
#Complete template####
template <- loadWorkbook(paste0(analysis_output_path,"file_overview_template.xlsx"))
for (sheet in rule_list) {
  addWorksheet(template, sheet)}

# for (sheet in rule_list) {   #this doesn't work! there is a longform below
#   write_out_list_f(sheet)}
#lapply(rule_list,write_out_list_f) #this doesn't work! there is a longform below
write_out_list_f(post_validation_freq)
write_out_list_f(pre_validation_freq)
write_out_list_f(Rule02a_post)
write_out_list_f(Rule02a_pre)
write_out_list_f(Rule02b_post)
write_out_list_f(Rule02b_pre)
write_out_list_f(Rule03_post)
write_out_list_f(Rule03_pre)
write_out_list_f(Rule04a_post)
write_out_list_f(Rule04a_pre)
write_out_list_f(Rule04b_post)
write_out_list_f(Rule04b_pre)
write_out_list_f(Rule05a_post)
write_out_list_f(Rule05a_pre)
write_out_list_f(Rule05b_post)
write_out_list_f(Rule05b_pre)
write_out_list_f(Rule05c_post)
write_out_list_f(Rule05c_pre)
write_out_list_f(Rule06a_post)
write_out_list_f(Rule06a_pre)
write_out_list_f(Rule06b_post)
write_out_list_f(Rule06b_pre)
write_out_list_f(Rule07a_post)
write_out_list_f(Rule07a_pre)
write_out_list_f(Rule07b_post)
write_out_list_f(Rule07b_pre)
write_out_list_f(Rule07c_post)
write_out_list_f(Rule07c_pre)
write_out_list_f(Rule07d_post)
write_out_list_f(Rule07d_pre)
write_out_list_f(Rule07e_post)
write_out_list_f(Rule07e_pre)
write_out_list_f(Rule07f_post)
write_out_list_f(Rule07f_pre)
write_out_list_f(Rule08_post)
write_out_list_f(Rule08_pre)
write_out_list_f(Rule09_post)
write_out_list_f(Rule09_pre)
# write_out_list_f(Rule10a_post)
# write_out_list_f(Rule10a_pre)
# write_out_list_f(Rule10b_post)
# write_out_list_f(Rule10b_pre)
# write_out_list_f(Rule11_post)
# write_out_list_f(Rule11_pre)
# write_out_list_f(Rule12_post)
# write_out_list_f(Rule12_pre)
# write_out_list_f(Rule13_post)
# write_out_list_f(Rule13_pre)

writeData(template, "summary", today(), startCol = 2, startRow = 4, colNames = FALSE)
writeData(template, "summary", summary_file_name, startCol = 2, startRow = 5, colNames = FALSE)
writeData(template, "summary", summary_record_count, startCol = 2, startRow = 8, colNames = FALSE)
writeData(template, "summary", summary_duplicates, startCol = 2, startRow = 10, colNames = FALSE)
writeData(template, "summary", response_codes, startCol = 2, startRow = 12, colNames = TRUE)
writeData(template,"rules_summary",rule_table, startCol = 1, startRow = 2, colNames = TRUE)
saveWorkbook(template, paste0(analysis_output_path,"populated_file_overview_",today(),".xlsx"), overwrite =TRUE)

#check if the same as before
hist.file <- readRDS(paste0(data_path,"results/data_Validated_results.rds"))
all.equal(hist.file,contractor_data)

#Save outfile####
saveRDS(contractor_data, file=paste0(data_path,"results/data_Validated_results.rds"))
#write.xlsx(contractor_data, file="data/results/data_Validated_results.xlsx")

#Create and save out anonymised version of validated results for SG:
# SGFile_Validated_Prov <- contractor_data %>% 
#   select(-c(qh_psid,patientid)) #Remove PSID(QH patient identifier) & PatientID (PHS patient identifier)
# saveRDS(SGFile_Validated_Prov, file="data/Results/anonymised_data_Validated_results_for_SG.rds")
# write_csv(SGFile_Validated_Prov, "data/Results/anonymised_data_Validated_results_for_SG.csv") 