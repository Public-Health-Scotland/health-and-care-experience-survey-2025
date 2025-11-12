## Written by Catriona Haddow and Martin Leitch
# November 2023.
# Adapted from 2021 code by Catriona Haddow and Martin Leitch
# WIP: November 2025
# *****************************************

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")
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

#Step 1: Read in contractor data#### 
#opening final set of unrouted results received from contractor.
#CH can we get file creation time?
contractor_data <- read.xlsx(paste0(historical_data_path,"Results from Contractor/Final data/HA23_Data_FINAL_V1.xlsx"), sheet = "RESPONSES")
summary_file_name <-"HA23_Data_FINAL_V1.xls"

#variables for file summary
summary_duplicates <- sum(duplicated(contractor_data$PatientID))
summary_record_count <- nrow(contractor_data) 
summary_variables <- sapply(contractor_data, class)#Check classification of each column

#Rename Variables as necessary: 
contractor_data <- contractor_data %>% 
  rename_with(tolower)%>% 
  rename(qh_psid = pnumber) %>% 
  mutate(responsereportingdatetime = as.Date(responsereportingdatetime, origin = "1899-12-30"), #reformat excel data
         qh_psid = paste0("P", qh_psid),#Update PSID to include P
         patientid = as.character(patientid)) #are there others this needs to apply to?

#add patientid_SG
index <- c(rep(1:nrow(contractor_data)))
contractor_data <- contractor_data %>% 
  mutate(patientid_sg = paste0("Pat",str_pad(index,6,c("left"),pad = "0"))) %>% 
  relocate(patientid_sg, .after = patientid)

#check if the same as before
hist.file <- readRDS(paste0(historical_data_path,"Results from Contractor/Final data/Final_unrouted_data.rds"))
identical(hist.file,contractor_data) #TRUE (06/02/24)

#Save out reformatted data
#saveRDS(contractor_data, file="data/Results from Contractor/Final data/Final_unrouted_data.rds") 

#Create anonymised version of unvalidated data as received from QH for SG:
SGFile <- contractor_data %>% 
  select(-c(qh_psid,patientid)) #Remove PSID(QH patient identifier) & PatientID (PHS patient identifier)

#check if the same as before
hist.file <- readRDS(paste0(historical_data_path,"Results from Contractor/Final data/anonymised_unvalidated_response_data_for_SG.rds"))
all.equal(hist.file,SGFile)
#Save out anonymised version of unvalidated data for SG
#saveRDS(SGFile, file="data/Results from Contractor/Final data/anonymised_unvalidated_response_data_for_SG.rds") 

#Save out anonymised version of unvalidated data for SG as csv.
#write_excel_csv(SGFile, "data/Results from Contractor/Final data/anonymised_unvalidated_response_data_for_SG.csv") 

#remove SGFile from Environment window
rm(SGFile)

#Step 2: Read in reformatted final results####

contractor_data <- readRDS(paste0(data_path_202324,"Results from Contractor/Final data/Final_unrouted_data.rds"))

#This outputs the frequencies of all the question responses
pre_validation_freq <- apply(contractor_data[questions], MARGIN=2, table)

#Step 3: Apply validation rules####

#Rule 1: FOR QH ONLY: ####
#'Tick one box only’ questions: if respondent selects more than one box, then question is cleared. The majority of questions are this type, 
#'so it’s easier to list the questions that this does not apply to: Q11, Q20, Q29, Q30, Q36, Q37 and Q40.

#===
#Rule 2: When did you last contact the GP Practice named on the enclosed letter?####
#Set up rule table
rule_table <- data.frame("rule" = c("Rule 2a"),
                         "rule_label" = c("If Q1 is blank, and Q2 is not blank – set Q1 to 1"),
                         "value" = sum(is.na(contractor_data$q01) & !is.na(contractor_data$q02),na.rm = TRUE))

Rule2a_pre <- tabyl(contractor_data,q01,q02) #Check frequencies before implementing rule

contractor_data <- contractor_data %>% 
  mutate(q01 = if_else(is.na(q01) & !is.na(q02),1,q01))# implement rule

Rule2a_post <- tabyl(contractor_data,q01,q02) #Check frequencies after implementing rule

## b > If Q1 <> 1 and Q2 to Q17 are not all blank – set Q2 to Q17 to blank.#### Note that NA needs to be explicit, as NA <>!= 1 
q2toq17 <- subset_qs(2,17)

rule_table <- rule_table %>%
  add_row("rule" = c("Rule 2b"),
          "rule_label" = c("If Q1 <> 1 and Q2 to Q17 are not all blank – set Q2 to Q17 to blank"),
          "value" = sum(ifelse((contractor_data$q01 !=1|is.na(contractor_data$q01)) & rowAny(contractor_data[q2toq17]),1,0),na.rm = TRUE))

#Frequencies before implementing rule
Rule2b_pre <- lapply(q2toq17, crosstabs_f,"q01")  
names(Rule2b_pre) <- q2toq17

#Implement rule:
contractor_data <- contractor_data %>% 
  mutate(across(all_of(q2toq17),~ case_when(q01 != 1 ~ NA,is.na(q01) ~ NA,TRUE ~ .)))

#Frequencies after implementing rule
Rule2b_post <- lapply(q2toq17, crosstabs_f,"q01")  
names(Rule2b_post) <- q2toq17

#===
#Rule 3: The last time you needed an appointment with your general practice, what kind of appointment…?####
#If Q6 = 7 (or blank) and Q7 to Q9 are not all blank – set Q7 to Q9 to blank.
q7toq9 <-subset_qs(7,9)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 3"),
          "rule_label" = c("If Q6 = 7 (or blank) and Q7 to Q9 are not all blank – set Q7 to Q9 to blank"),
          "value" = sum(ifelse(contractor_data$q06 %in% c(7,NA) & rowAny(contractor_data[q7toq9]),1,0)))

Rule3_pre <- lapply(q7toq9, crosstabs_f,"q06")  #Check frequencies before implementing rule
names(Rule3_pre) <- q7toq9

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q7toq9),~ case_when(q06 %in% c(7,NA) ~ NA,TRUE ~ .)))

Rule3_post <- lapply(q7toq9, crosstabs_f,"q06")  #Check frequencies after implementing rule
names(Rule3_post) <- q7toq9
#===
#Rule 4: Were you satisfied with the appointment you were offered?####
#>	If Q8 = 1 and any of Q9 is not blank – set Q9 to blank # this is different to before, now 84 extra records
q9 <-subset_qs(9,9)
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 4"),
          "rule_label" = c("If Q8 = 1 and any of Q9 is not blank – set Q9 to blank"),
          "value" = sum(ifelse(contractor_data$q08 ==1 & rowAny(contractor_data[q9]),1,0),na.rm = TRUE))

Rule4_pre <- lapply(q9, crosstabs_f,"q08")  #Check frequencies before implementing rule
names(Rule4_pre) <- q9

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q9),~ case_when(q08 == 1 ~ NA,TRUE ~ .)))

Rule4_post <- lapply(q9, crosstabs_f,"q08")  #Check frequencies after implementing rule
names(Rule4_post) <- q9

#===
#Rule 5: The last time you needed to see or speak to a doctor or nurse from your GP Practice quite urgently, how long did you have to wait? ####
## If Q10 = 1, 2 or 4 or blank and Q11 is not blank – set Q11 to blank.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 5"),
          "rule_label" = c("If Q10 = 1, 2 or 4 or blank and Q11 is not blank – set Q11 to blank."),
          "value" = sum(ifelse(contractor_data$q10 %in% c(1,2,4,NA) & !is.na(contractor_data$q11),1,0)))

Rule5_pre <- tabyl(contractor_data,q10,q11)  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q11= case_when(q10 %in% c(1,2,4,NA) ~ NA,TRUE ~ q11))

Rule5_post <- tabyl(contractor_data,q10,q11)  #Check frequencies after implementing rule

#===
#Rule 6: The last time you received treatment or advice at your GP practice in the last 12 months, what did you receive treatment or advice for?####

q14atoq14e <-c("q14a","q14b","q14c","q14d","q14e")
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 6a"),
          "rule_label" = c("If Q14f = 1 and any of Q14a to Q14e = 1 – set Q14f to blank."),
          "value" = sum(if_else(contractor_data$q14f ==1 & rowAny(contractor_data[q14atoq14e]),1,0),na.rm = TRUE))

Rule6a_pre <- lapply(q14atoq14e, crosstabs_f,"q14f")  #Check frequencies before implementing rule
names(Rule6a_pre) <-q14atoq14e

# contractor_data <- contractor_data %>% #Implement rule:
#   mutate(across(all_of(q14atoq14e),~ case_when( == 1 ~ NA,TRUE ~ .)))

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q14f = case_when(rowAny(contractor_data[q14atoq14e]) ~ NA,TRUE ~ q14f))

Rule6a_post <- lapply(q14atoq14e, crosstabs_f,"q14f")  #Check frequencies after implementing rule
names(Rule6a_post) <- q14atoq14e

##b > If Q14f = 1 and Q15 to Q17 are not all blank – set Q15 to Q17 to blank. #not the same

q15toq17 <-subset_qs(15,17)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 6b"),
          "rule_label" = c("If Q14f = 1 and Q15 to Q17 are not all blank – set Q15 to Q17 to blank."),
          "value" = sum(if_else(contractor_data$q14f ==1 & rowAny(contractor_data[q15toq17]),1,0),na.rm = TRUE))

Rule6b_pre <- lapply(q15toq17, crosstabs_f,"q14f")  #Check frequencies before implementing rule
names(Rule6b_pre) <-q15toq17

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q15toq17),~ case_when(q14f == 1 ~ NA,TRUE ~ .)))

Rule6b_post <- lapply(q15toq17, crosstabs_f,"q14f")  #Check frequencies after implementing rule
names(Rule6b_post) <- q15toq17

#--
## c > If none of Q14a-e = 1 then set Q15 to Q17 to blank.
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 6c"),
          "rule_label" = c("If none of Q14a-e = 1 then set Q15 to Q17 to blank."),
          "value" = sum(if_else(rowAll(contractor_data[q14atoq14e]) & rowAny(contractor_data[q15toq17]),1,0),na.rm = TRUE))

#add temporary helper variable to dataset
contractor_data <- contractor_data %>% 
  mutate(q14atoq14e_any = case_when(if_any(all_of(q14atoq14e)) == 1 ~ 1,TRUE ~ 0))

Rule6c_pre <- lapply(q15toq17, crosstabs_f,"q14atoq14e_any")  #Check frequencies before implementing rule
names(Rule6c_pre) <-q15toq17

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q15toq17),~ case_when(q14atoq14e_any == 0 ~ NA,TRUE ~ .)))

Rule6c_post <- lapply(q15toq17, crosstabs_f,"q14atoq14e_any")  #Check frequencies after implementing rule
names(Rule6c_post) <- q15toq17

contractor_data <- contractor_data %>% select(-q14atoq14e_any) #drop helper variable
  
#===
#Rule 7: In the past 12 months, have you contacted an NHS service when you wanted to see a GP, but your GP practice was closed?####
## a > If Q19 = 2 and Q20 to Q25 are not all blank – set Q20 to Q25 to blank.
q20toq25 <- subset_qs(20,25)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 7a"),
          "rule_label" = c("If Q19 = 2 and Q20 to Q25 are not all blank – set Q20 to Q25 to blank."),
          "value" = sum(ifelse(contractor_data$q19 ==2 & rowAny(contractor_data[q20toq25]),1,0),na.rm = TRUE))

Rule7a_pre <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies before implementing rule
names(Rule7a_pre) <-q20toq25

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q20toq25),~ case_when(q19 == 2 ~ NA,TRUE ~ .)))

Rule7a_post <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies after implementing rule
names(Rule7a_post) <- q20toq25

#--
## 7b > If Q19 is blank and Q20 to Q25 are not all blank – set Q19 to 1.
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 7b"),
          "rule_label" = c("If Q19 is blank and Q20 to Q25 are not all blank – set Q19 to 1."),
          "value" = sum(ifelse(is.na(contractor_data$q19) & rowAny(contractor_data[q20toq25]),1,0),na.rm = TRUE))

Rule7b_pre <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies before implementing rule
names(Rule7b_pre) <-q20toq25

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q20toq25),~ case_when(is.na(q19) ~ NA,TRUE ~ .)))

Rule7b_post <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies after implementing rule
names(Rule7b_post) <- q20toq25

#===
#Rule 8: In the past 12 months, have you had any help or support with everyday living?####
#check pre-validation status
q28toq32 <- subset_qs(28,32)
social_care_section_pre_validation <- contractor_data %>% 
  select(starts_with("q27"),all_of(q28toq32)) %>% 
  mutate(help_indicator = case_when(q27a == 1 | q27b == 1 |q27c == 1 | q27d == 1|q27e == 1 | q27f == 1 ~ "1 Help received",
                                    q27g == 1~"2 Help needed, not received",
                                    q27h == 1~"3 No help needed",TRUE ~ "4 Not complete"),
         no_help_reason = case_when(q32a == 1|q32b == 1|q32c == 1|q32d == 1|q32e == 1|q32f == 1|q32g == 1|q32h == 1 ~ "1 Reason given",
                                    q32i == 1~ "2 Not applicable",
                                    TRUE ~ "3 Not complete")) %>% 
  group_by(help_indicator,no_help_reason) %>% 
  summarise(count = n())

#Rule 8: If you are not receiving all the help and care services for everyday living that you feel you need, which options describe your situation? Please tick all that apply. NEW 

## a > If Q27g = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27g to blank.
q27atoq27f <- c("q27a","q27b","q27c","q27d","q27e","q27f")
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 8a"),
          "rule_label" = c("If Q27g = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27g to blank."),
          "value" = sum(ifelse(contractor_data$q27g ==1 & rowAny(contractor_data[q27atoq27f]),1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q27g = if_else(rowAny(contractor_data[q27atoq27f]),NA,q27g))

## b > If Q27h = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27h to blank.
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 8b"),
          "rule_label" = c("If Q27h = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27h to blank."),
          "value" = sum(ifelse(contractor_data$q27h ==1 & rowAny(contractor_data[q27atoq27f]),1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #Implement rule: 
  mutate(q27h = if_else(rowAny(contractor_data[q27atoq27f]),NA,q27h))

## c > If Q27g = 1 and Q27h = 1 – set Q27h to blank.
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 8c"),
          "rule_label" = c("If Q27g = 1 and Q27h = 1 – set Q27h to blank."),
          "value" = sum(ifelse(contractor_data$q27g ==1 & contractor_data$q27h == 1,1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q27h = case_when(q27g == 1 ~ NA,TRUE ~ q27h))


## d > If Q27g = 1 – set Q28 to Q31 to blank.
q28toq31 <- subset_qs(28,31)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 8d"),
          "rule_label" = "If Q27g = 1 – set Q28 to Q31 to blank.",
          "value" = sum(if_else(contractor_data$q27g ==1 & rowAny(contractor_data[q28toq31]),1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q28toq31),~ case_when(q27g ==1 ~ NA,TRUE ~ .)))

## e > If Q27h = 1 – set Q28 to Q32 to blank.
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 8e"),
          "rule_label" = "If Q27h = 1 – set Q28 to Q32 to blank.",
          "value" = sum(if_else(contractor_data$q27h ==1 & rowAny(contractor_data[q28toq32]),1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q28toq32),~ case_when(q27h ==1 ~ NA,TRUE ~ .)))

## f > If Q27g = 1 and Q32i = 1 – set Q32i to blank.
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 8f"),
          "rule_label" = "If Q27g = 1 and Q32i = 1 – set Q32i to blank.",
          "value" = sum(ifelse(contractor_data$q27g ==1 & contractor_data$q32i == 1,1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q32i = case_when(q27g == 1 ~ NA,TRUE ~ q32i))

## g > If Q27a to Q27h are all blank (and Q28 to Q32 are not all blank) - set Q28 to Q32 to blank
#This is, if respondent hasn't completed this routing question, blank rest of section.
#Impact check

q27atoq27h <- c("q27a","q27b","q27c","q27d","q27e","q27f","q27g","q27h")
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 8g"),
          "rule_label" = "If Q27a to Q27h are all blank (and Q28 to Q32 are not all blank) - set Q28 to Q32 to blank",
          "value" = sum(ifelse((rowAll(contractor_data[q27atoq27h])
                               & rowAny(contractor_data[q28toq32])),1,0),na.rm = TRUE))

#Implement rule
contractor_data <- contractor_data %>% 
  mutate(across(all_of(q28toq32), ~replace(., rowAll(contractor_data[q27atoq27h]), NA)))

#check overall effect
social_care_section <- contractor_data %>% 
  select(qh_psid,starts_with("q27")|starts_with("q32")) %>% 
  mutate(help_indicator = case_when(q27a == 1 | q27b == 1 |q27c == 1 | q27d == 1|q27e == 1 | q27f == 1 ~ "1 Help received",
                                   q27g == 1~"2 Help needed, not received",
                                   q27h == 1~"3 No help needed",TRUE ~ "4 Not complete"),
         no_help_reason = case_when(q32a == 1|q32b == 1|q32c == 1|q32d == 1|q32e == 1|q32f == 1|q32g == 1|q32h == 1 ~ "1 Reason given",
                                    q32i == 1~ "2 Not applicable",
                                    TRUE ~ "3 Not complete"))

table(social_care_section$no_help_reason,social_care_section$help_indicator)

#===
#Rule 9: Do you look after, or give any regular help or support, to …..?####
# > If Q33 not in (1,2,3,4,5) and Q34 to Q37 are not all blank – set Q34 to Q37 to blank.
q34toq37 <-subset_qs(34,37)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 9"),
          "rule_label" = "If Q33 not in (1,2,3,4,5) and Q34 to Q37 are not all blank – set Q34 to Q37 to blank",
          "value" = sum(ifelse(contractor_data$q33  %in% c(6,NA) & rowAny(contractor_data[q34toq37]),1,0),na.rm = TRUE))


Rule9_pre <- lapply(q34toq37, crosstabs_f,"q33")  #Check frequencies before implementing rule
names(Rule9_pre) <-q34toq37

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q34toq37),~ case_when(q33 %in% c(6,NA) ~ NA,TRUE ~ .)))

Rule9_post <- lapply(q34toq37, crosstabs_f,"q33")  #Check frequencies after implementing rule
names(Rule9_post) <- q34toq37

#===
#Rule 10.	Do you have any of the following?####
##a > If all of Q39a to Q32k are blank, and Q32jOther has text, set Q32j = 1 
q39 <- subset_qs(39,39)

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 10a"),
          "rule_label" = "If all of Q39a to Q32k are blank, and Q32jOther has text, set Q32j = 1",
          "value" = sum(ifelse(rowAll(contractor_data[q39]) & !is.na(contractor_data$q39jOther),1,0),na.rm = TRUE))

contractor_data <- contractor_data %>% #Add helper variable
  mutate(q39jOther_text = case_when(is.na(contractor_data$q39jOther) ~ "No text",TRUE ~ "Text"))

Rule10a_pre <- lapply(q39, crosstabs_f,"q39jOther_text")  #Check frequencies before implementing rule
names(Rule10a_pre) <-q39

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q39j= case_when(rowAll(contractor_data[q39]) & !is.na(contractor_data$q39jOther) ~ 1, TRUE ~ q39j))

Rule10a_post <- lapply(q39, crosstabs_f,"q39jOther_text")  #Check frequencies after implementing rule
names(Rule10_post) <- q39

contractor_data <- contractor_data %>% #Drop helper variable
  select(-q39jOther_text)

##b > If any of Q39a to Q39j = 1 and Q39k = 1 – set Q39k to blank. 
q39atoq39j <- q39[q39 != "q39k"]
rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 10b"),
          "rule_label" = "If any of Q39a to Q39j = 1 and Q39k = 1 – set Q39k to blank",
          "value" = sum(if_else(rowAny(contractor_data[q39atoq39j]) & contractor_data$q39k == 1,1,0),na.rm = TRUE))

#===
#Rule 11: Which of the following best describes your sexual orientation? #### 
##> If Q42 = 0 and Q42 text box (q42Other) is not blank then q42 = 4.

Rule11  <- ifelse(contractor_data$q42 == 0 & (!(is.na(contractor_data$q42Other))),1,0)

#Implement rule
contractor_data$q42[contractor_data$q42 == 0 & (!(is.na(contractor_data$q42Other)))] <- 4
#Check frequencies after implementing rule
table(contractor_data$q42)

#===
#Rule 12: What best describes your ethnic group? Please tick one box only#### 
##> If Q43 = 0 and Q43 text box (q43Other) is not blank then q43 = 6.
Rule12  <- ifelse(contractor_data$q43 == 0 & (!(is.na(contractor_data$q43Other))),1,0)

#Implement rule
contractor_data$q43[contractor_data$q43 == 0 & (!(is.na(contractor_data$q43Other)))] <- 6
#Check frequencies after implementing rule
table(contractor_data$q43)

#===
#Rule 13: What religion, religious denomination or body do you belong to? #### 
##> If Q44 = 0 and Q44 text box (q44Other) is not blank then q44 = 11.
Rule13  <- ifelse(contractor_data$q44 == 0 & (!(is.na(contractor_data$q44Other))),1,0)

#Implement rule
contractor_data$q44[contractor_data$q44 == 0 & (!(is.na(contractor_data$q44Other)))] <- 11

#This outputs the frequencies of all the question responses
post_validation_freq <- apply(contractor_data[questions], MARGIN=2, table)

#Create rule summary####
rule_tables <- c("Rule2a_pre","Rule2a_post","Rule2b_pre","Rule2b_post","Rule3_pre","Rule3_post",
                 "Rule4_pre","Rule4_post","Rule5_pre","Rule5_post",
                 "Rule6a_pre","Rule6a_post","Rule6b_pre","Rule6b_post", "Rule6c_pre","Rule6c_post",
                 "Rule7a_pre","Rule7a_post","Rule7b_pre","Rule7b_post",
                 "Rule8a_pre","Rule8a_post","Rule8b_pre","Rule8b_post", "Rule8c_pre","Rule8c_post",
                 "Rule9_pre","Rule9_post","Rule10a_pre","Rule10a_post", "Rule10b_pre","Rule10b_post",
                 "Rule11_pre","Rule11_post","Rule12_pre","Rule12_post", "Rule13_pre","Rule13_post")
                 
rule_list <- list(Rule2b_pre,Rule2b_post,Rule3_pre,Rule3_post)

#save out rules output

write_out_list_f <- function(x,wb) {  # function to write list of tables to single excel sheet
  curr_row <- 1
for(i in seq_along(x)) {
  writeData(wb, deparse(substitute(x)),names(x)[i], startCol = 1, startRow = curr_row)
  writeData(wb, deparse(substitute(x)),x[[i]], startCol = 1, startRow = curr_row+1)
  curr_row <- curr_row + nrow(x[[i]]) + 2
}
}

for (x in rule_list) {  #this does not work
  write_out_list_f
}

saveWorkbook(wb, "outputs/analysis_output/rules_output.xlsx",overwrite=TRUE)
write_out_list_f(Rule2b_pre)

write.xlsx(rule_tables, file = "outputs/analysis_output/rules_results.xlsx")

###########################################################################################################################################
#Complete template####
template <- loadWorkbook(paste0("outputs/analysis_output/file_overview_template.xlsx"))
for (sheet in rule_tables) {
  addWorksheet(template, sheet)
}
writeData(template, "summary", today(), startCol = 2, startRow = 4, colNames = FALSE)
writeData(template, "summary", summary_file_name, startCol = 2, startRow = 5, colNames = FALSE)
writeData(template, "summary", summary_record_count, startCol = 2, startRow = 8, colNames = FALSE)
writeData(template, "summary", summary_duplicates, startCol = 2, startRow = 10, colNames = FALSE)
write_out_list_f(pre_validation_freq,template)
writeData(template,"rules_summary",rules_summary, startCol = 2, startRow = 1, colNames = FALSE)

#write_out_list_f(Rule2a_pre,template)
#write_out_list_f(Rule2a_post,template)
write_out_list_f(Rule2b_pre,template)
write_out_list_f(Rule2b_post,template)
write_out_list_f(Rule3_pre,template)
write_out_list_f(Rule3_post,template)
write_out_list_f(Rule4_pre,template)
write_out_list_f(Rule4_post,template)
#write_out_list_f(Rule5_pre,template)
#write_out_list_f(Rule5_post,template)
write_out_list_f(Rule6a_pre,template)
write_out_list_f(Rule6a_post,template)
write_out_list_f(Rule6b_pre,template)
write_out_list_f(Rule6b_post,template)
write_out_list_f(Rule6c_pre,template)
write_out_list_f(Rule6c_post,template)
write_out_list_f(Rule7a_pre,template)
write_out_list_f(Rule7a_post,template)
write_out_list_f(Rule7b_pre,template)
# write_out_list_f(Rule8b_post,template)
# write_out_list_f(Rule8c_pre,template)
# write_out_list_f(Rule8c_post,template)
# write_out_list_f(Rule8d_pre,template)
# write_out_list_f(Rule8d_post,template)
# write_out_list_f(Rule8e_pre,template)
# write_out_list_f(Rule8e_post,template)
# write_out_list_f(Rule8f_pre,template)
# write_out_list_f(Rule8f_post,template)
# write_out_list_f(Rule8g_pre,template)
# write_out_list_f(Rule8g_post,template)
write_out_list_f(Rule9_pre,template)
write_out_list_f(Rule9_post,template)
write_out_list_f(Rule10a_pre,template)
write_out_list_f(Rule10b_post,template)
write_out_list_f(Rule11_pre,template)
write_out_list_f(Rule11_post,template)
write_out_list_f(Rule12_pre,template)
write_out_list_f(Rule12_post,template)
write_out_list_f(Rule13_pre,template)
write_out_list_f(Rule13_post,template)
saveWorkbook(template, paste0("outputs/analysis_output/","populated_file_overview.xlsx"), overwrite =TRUE)


#Drop the comments columns which are all blank
contractor_data$q18 <- NULL
contractor_data$q26 <- NULL

#q32h, q39j, q42d, q43 and q44 are other/another options with write in boxes.

#check if the same as before
hist.file <- readRDS("data/Results/data_Validated_results.rds")
identical(hist.file,contractor_data)

#Save outfile####
saveRDS(contractor_data, file="data/Results/data_Validated_results.rds")

#Save outfile as xlsx####
write.xlsx(contractor_data, file="data/Results/data_Validated_results.xlsx")

#29/01/2024 then 06/02/2024 Create anonymised version of validated results for SG:
SGFile_Validated_Prov <- contractor_data

#Remove PSID(QH patient identifier) & PatientID (PHS patient identifier)
SGFile_Validated_Prov$qh_psid <- NULL
SGFile_Validated_Prov$patientid <- NULL

#Save out anonymised version of unvalidated data for SG
#check if the same as before
hist.file <- readRDS("data/Results/anonymised_data_Validated_results_for_SG.rds")
identical(hist.file,SGFile_Validated_Prov)
saveRDS(SGFile_Validated_Prov, file="data/Results/anonymised_data_Validated_results_for_SG.rds")

#Save out anonymised version of validated data for SG as csv
write_excel_csv(SGFile_Validated_Prov, "data/Results/anonymised_data_Validated_results_for_SG.csv") 