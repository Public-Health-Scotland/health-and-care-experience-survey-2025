#Purpose: Reads in patient level data for HACE 2025 Survey and applies and checks validation rules

#Inputs: "response_data_from_contractor/HA25_Final_v1.xlsx #Raw data received from contractor -  
#data_path,"results/q32_comments_recode analysis_25-26.xlsx" Mapping data received from SCAU  

#Outputs: 
#"Final_unrouted_data.Rds"
#"anonymised_unvalidated_response_data_for_SG.rds"
# analysis_output_path,"file_overview_populated_",today(),".xlsx"
#"data_Validated results.rds"
#"data_Validated results.xlsx"
#"anonymised_data_Validated results for SG.rds"
#"anonymised_data_Validated results for SG.csv"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#Step 1: Read in unrouted responses received from contractor#### 
contractor_data <- read.xlsx(paste0(data_path,"response_data_from_contractor/HA25_Final_Data_v1.xlsx"), sheet = "RESPONSES")

#Rename Variables as necessary: 
contractor_data <- contractor_data %>% 
  rename_with(tolower)%>% 
  rename_with(.fn = ~ paste0("q", .),   .cols = matches("^\\d")) %>%      # Rename columns which start with a digit - these are the questions
  rename(qh_psid = participant.id,#pnumber last survey
         responsereportingdatetime = response.date.time,
         responsecode = response.code,
         responsesubcode = response.sub.code) %>% #need response sub code too?
  mutate(responsereportingdatetime = as.Date(responsereportingdatetime, origin = "1899-12-30"), #reformat excel data
         patientid = as.character(patientid),#are there others this needs to apply to?
         across(all_of(questions), ~ as.character(.))) #all response options as character

#variables for file summary
summary_file_name <-"HA25_Final_Data_v1.xlsx"
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
hist.file <- readRDS(paste0(data_path,"response_data_from_contractor/final_unrouted_data.rds"))
all.equal(hist.file,contractor_data) 
#Save out reformatted data
saveRDS(contractor_data, file=paste0(data_path,"response_data_from_contractor/final_unrouted_data.rds"))

#Create anonymised version of unvalidated data as received from QH for SG:
SGFile <- contractor_data %>% 
  select(-c(qh_psid,patientid)) #Remove PSID(QH patient identifier) & PatientID (PHS patient identifier)

#check if the same as before
hist.file <- readRDS(paste0(data_path,"response_data_from_contractor/anonymised_unvalidated_response_data_for_SG.rds"))
all.equal(hist.file,SGFile)
#Save out anonymised version of unvalidated data for SG
saveRDS(SGFile, file=paste0(data_path,"response_data_from_contractor/anonymised_unvalidated_response_data_for_SG.rds"))

#Step 2: Read in reformatted responses####
contractor_data <- readRDS(paste0(data_path,"response_data_from_contractor/final_unrouted_data.rds"))

contractor_data <- contractor_data %>% 
  select(-c(q18,q26,q40j_other,q43_other,q44_other)) #Drop the comments columns which are all blank

#read in and process the free text data
free_text_file <- read.xlsx(paste0(data_path,"response_data_from_contractor/HA25_REDACTED_40j_43_44_v1.xlsx"))
free_text_file <- free_text_file %>% 
  rename_with(tolower)%>% 
  select(-matches("response"),-patientid) %>% 
  rename(qh_psid = psid) %>% 
  mutate(qh_psid = paste0("P", qh_psid),
         across(.cols = matches("other"),.fns = ~ sub("\\.+$", "",.)), #remove trailling '.
         across(.cols = matches("other"),.fns = ~ tolower(str_trim(.))))

contractor_data <- contractor_data %>% #match on free text data
  left_join(free_text_file,by = c("qh_psid"))  
  
#Outputs the frequencies of all the question responses
pre_validation_freq <- apply(contractor_data[questions], MARGIN=2, table)
questions_in_data <- names(contractor_data)[str_detect(names(contractor_data), "^q\\d+")] #get list of questions in response data

#Check that the response data received matches the created questionnaire lookup
questions_in_lookup_not_data <- questions[!questions %in% questions_in_data] #this should be empty
questions_in_data_not_lookup <- questions_in_data[!questions_in_data %in% questions] #this should have only 'other' questions

#Step 3: Apply validation rules####

#Rule 1: FOR QH ONLY: ####
#'Tick one box only’ questions: if respondent selects more than one box, then question is cleared. The majority of questions are this type, 
#so it’s easier to list the questions that this does not apply to: Q11, Q20, Q29, Q30, Q36, Q37 and Q40.

#===
#Rule 2: When did you last contact the GP Practice named on the enclosed letter?####
## a > If Q1 is blank and Q2 is not blank - set Q1 to 1
rule_table <- data.frame("rule" = c("Rule 02a"), #Set up rule table
                         "rule_label" = c("If Q1 is blank, and Q2 is not blank – set Q1 to 1"),
                         "value" = sum(is.na(contractor_data$q01) & !is.na(contractor_data$q02),na.rm = TRUE))

Rule02a_pre <- lapply("q02", crosstabs_f,"q01")  #Frequencies before implementing rule

contractor_data <- contractor_data %>% 
  mutate(q01 = if_else(is.na(q01) & !is.na(q02),"1",q01))# implement rule

Rule02a_post <- lapply("q02", crosstabs_f,"q01") #Check frequencies after implementing rule

## b > If Q1 <> 1 and Q2 to Q17 are not all blank – set Q2 to Q17 to blank.#### Note that NA needs to be explicit, as NA <>!= 1 
q2toq17 <- subset_qs(2,17)

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(any_not_empty_f(q2toq17) & (q01 != "1"| is.na(q01)),1,0)) %>% 
  group_by("rule" = "Rule 02b",
           "rule_label" = c("If Q1 <> 1 and Q2 to Q17 are not all blank – set Q2 to Q17 to blank")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule02b_pre <- lapply(q2toq17, crosstabs_f,"q01")  #Frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q2toq17),~ case_when(q01 != "1" ~ NA,is.na(q01) ~ NA,TRUE ~ .)))

Rule02b_post <- lapply(q2toq17, crosstabs_f,"q01")  #Frequencies after implementing rule

#===
#Rule 3: The last time you needed an appointment with your general practice, what kind of appointment…?####
#If Q7 = 7 (or blank) and Q8 to Q9 are not all blank – set Q7 to Q9 to blank.
q8toq9 <-subset_qs(8,9)

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(any_not_empty_f(q8toq9) & q07 %in% c("7",NA),1,0)) %>% 
  group_by("rule" = c("Rule 03"),
           "rule_label" = c("If Q7 = 7 (or blank) and Q8 to Q9 are not all blank – set Q8 to Q9 to blank")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule03_pre <- lapply(q8toq9, crosstabs_f,"q07")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q8toq9),~ case_when(q07 %in% c("7",NA) ~ NA,TRUE ~ .)))

Rule03_post <- lapply(q8toq9, crosstabs_f,"q07")  #Check frequencies after implementing rule

#Rule 4: The last time you needed to see or speak to a doctor or nurse from your GP Practice quite urgently, how long did you have to wait? ####
## 4a If Q10 = 1, 2 or 4 and Q11 is not blank – set Q11 to blank.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 04a"),
          "rule_label" = c("If Q10 = 1, 2 or 4 or blank and Q11 is not blank – set Q11 to blank."),
          "value" = sum(if_else(contractor_data$q10 %in% c("1","2","4") & !is.na(contractor_data$q11),1,0)))

Rule04a_pre <- lapply("q10", crosstabs_f,"q11")   #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q11= case_when(q10 %in% c("1","2","4") ~ NA,TRUE ~ q11))

Rule04a_post <- lapply("q10", crosstabs_f,"q11")   #Check frequencies after implementing rule

## 4b > If Q10 is blank and Q11 is not blank – set Q10 to 3.

rule_table <- rule_table %>% 
  add_row("rule" = c("Rule 04b"),
          "rule_label" = c("If Q10 is blank and Q11 is not blank – set Q10 to 3."),
          "value" = sum(if_else(is.na(contractor_data$q10) & !is.na(contractor_data$q11),1,0),na.rm = TRUE))

Rule04b_pre <- lapply("q10", crosstabs_f,"q11")   #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q10= case_when(is.na(q10) & !is.na(q11) ~ "3",TRUE ~ q10))

Rule04b_post <- lapply("q10", crosstabs_f,"q11")   #Check frequencies after implementing rule

#Rule 5: the last time you received treatment or advice at your General Practice in the last 12 months. What was it for? ####
##a > If Q14f = 1 and any of Q14a to Q14e = 1 – set Q14f to blank. 
q14atoq14e <-c("q14a","q14b","q14c","q14d","q14e")

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q14f == "1" & any_not_empty_f(q14atoq14e),1,0)) %>% 
  group_by("rule" = c("Rule 05a"),
           "rule_label" = c("If Q14f = 1 and any of Q14a to Q14e = 1 – set Q14f to blank.")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule05a_pre <- lapply(q14atoq14e, crosstabs_f,"q14f")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q14f = case_when(any_not_empty_f(q14atoq14e) ~ NA,TRUE ~ q14f))

Rule05a_post <- lapply(q14atoq14e, crosstabs_f,"q14f")  #Check frequencies after implementing rule

##b > If Q14f = 1 and Q15 to Q17 are not all blank – set Q15 to Q17 to blank. 

q15toq17 <-subset_qs(15,17)

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q14f == "1" & any_not_empty_f(q15toq17),1,0)) %>% 
  group_by("rule" = c("Rule 05b"),
           "rule_label" = c("If Q14f = 1 and Q15 to Q17 are not all blank – set Q15 to Q17 to blank.")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule05b_pre <- lapply(q15toq17, crosstabs_f,"q14f")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q15toq17),~ case_when(q14f == "1" ~ NA,TRUE ~ .)))

Rule05b_post <- lapply(q15toq17, crosstabs_f,"q14f")  #Check frequencies after implementing rule

## c > If none of Q14a-e = 1 then set Q15 to Q17 to blank.

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(all_empty_f(q14atoq14e) & any_not_empty_f(q15toq17),1,0)) %>% 
  group_by("rule" = c("Rule 05c"),
           "rule_label" = c("If none of Q14a-e = 1 then set Q15 to Q17 to blank.")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

contractor_data <- contractor_data %>% #add temporary helper variable to dataset
  mutate(q14atoq14e_any = case_when(any_not_empty_f(q14atoq14e) ~ 1,TRUE ~ 0)) #q14atoq14e_any = 1 if any of q14atoq14e = 1

Rule05c_pre <- lapply(q15toq17, crosstabs_f,"q14atoq14e_any")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q15toq17),~ case_when(q14atoq14e_any == 0 ~ NA,TRUE ~ .)))

Rule05c_post <- lapply(q15toq17, crosstabs_f,"q14atoq14e_any")  #Check frequencies after implementing rule

contractor_data <- contractor_data %>% select(-q14atoq14e_any) #drop helper variable
  
#Rule 6: In the past 12 months, have you contacted an NHS service when you wanted to see a healthcare professional, but your General Practice was closed? ####
## a > If Q19 = 2 and Q20 to Q25 are not all blank – set Q20 to Q25 to blank.

q20toq25 <- subset_qs(20,25)

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q19 =="2" & any_not_empty_f(q20toq25),1,0)) %>% 
  group_by("rule" = c("Rule 06a"),
           "rule_label" = c("If Q19 = 2 and Q20 to Q25 are not all blank – set Q20 to Q25 to blank.")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule06a_pre <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q20toq25),~ case_when(q19 == "2" ~ NA,TRUE ~ .)))

Rule06a_post <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies after implementing rule

## 6b > If Q19 is blank and Q20 to Q25 are not all blank – set Q19 to 1.

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(is.na(q19) & any_not_empty_f(q20toq25),1,0)) %>% 
  group_by("rule" = c("Rule 06b"),
           "rule_label" = c("If Q19 is blank and Q20 to Q25 are not all blank – set Q19 to 1.")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule06b_pre <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q19 = case_when(is.na(q19) & any_not_empty_f(q20toq25) ~ "1", TRUE ~ q19))

Rule06b_post <- lapply(q20toq25, crosstabs_f,"q19")  #Check frequencies after implementing rule

#Rule 7: In the past 12 months, have you had any help or support with everyday living?####
#Rule 7a: If you are not receiving all the help and care services for everyday living that you feel you need, which options describe your situation? Please tick all that apply. NEW 

## a > If Q27g = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27g to blank.

q27atoq27f <- c("q27a","q27b","q27c","q27d","q27e","q27f")

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q27g == "1" & any_not_empty_f(q27atoq27f),1,0)) %>% 
  group_by("rule" = c("Rule 07a"),
           "rule_label" = c("If Q27g = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27g to blank.")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule07a_pre <- lapply(q27atoq27f, crosstabs_f,"q27g")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q27g = if_else(any_not_empty_f(q27atoq27f),NA,q27g))

Rule07a_post <- lapply(q27atoq27f, crosstabs_f,"q27g")  #Check frequencies after implementing rule

## b > If Q27h = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27h to blank.

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q27h == "1" & any_not_empty_f(q27atoq27f),1,0)) %>% 
  group_by("rule" = c("Rule 07b"),
           "rule_label" = c("If Q27h = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 – set Q27h to blank.")) %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule07b_pre <- lapply(q27atoq27f, crosstabs_f,"q27h")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule: 
  mutate(q27h = if_else(any_not_empty_f(q27atoq27f),NA,q27h))

Rule07b_post <- lapply(q27atoq27f, crosstabs_f,"q27h")  #Check frequencies after implementing rule

## c > If Q27g = 1 and Q27h = 1 – set Q27h to blank.

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q27g == "1" & q27h == "1",1,0)) %>% 
  group_by("rule" = "Rule 07c",
           "rule_label" = "If Q27g = 1 and Q27h = 1 – set Q27h to blank.") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule07c_pre <- lapply("q27g", crosstabs_f,"q27h")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q27h = case_when(q27g == "1" ~ NA,TRUE ~ q27h))

Rule07c_post <- lapply("q27g", crosstabs_f,"q27h")  #Check frequencies after implementing rule

## d > If Q27g = 1 – set Q28 to Q31 to blank.

q28toq31 <- subset_qs(28,31)

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q27g == "1" & any_not_empty_f(q28toq31),1,0)) %>% 
  group_by("rule" = "Rule 07d",
           "rule_label" = "If Q27g = 1 – set Q28 to Q31 to blank.") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule07d_pre <- lapply(q28toq31, crosstabs_f,"q27g")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q28toq31),~ case_when(q27g =="1" ~ NA,TRUE ~ .)))

Rule07d_post <- lapply(q28toq31, crosstabs_f,"q27g")  #Check frequencies after implementing rule

## e > If Q27h = 1 – set Q28 to Q32 to blank.
q28toq32 <- subset_qs(28,32)

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q27h == "1" & any_not_empty_f(q28toq32),1,0)) %>% 
  group_by("rule" = "Rule 07e",
           "rule_label" = "If Q27h = 1 – set Q28 to Q32 to blank.") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule07e_pre <- lapply(q28toq32, crosstabs_f,"q27h")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q28toq32),~ case_when(q27h =="1" ~ NA,TRUE ~ .)))

Rule07e_post <- lapply(q28toq32, crosstabs_f,"q27h")  #Check frequencies after implementing rule

## f > If Q27a to Q27h are all blank (and Q28 to Q32 are not all blank) - set Q28 to Q32 to blank
#This is, if respondent hasn't completed this routing question, blank rest of section.

q27atoq27h <- c("q27a","q27b","q27c","q27d","q27e","q27f","q27g","q27h")

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(all_empty_f(q27atoq27h) & any_not_empty_f(q28toq32),1,0)) %>% 
  group_by("rule" = "Rule 07f",
           "rule_label" = "If Q27a to Q27h are all blank (and Q28 to Q32 are not all blank) - set Q28 to Q32 to blank") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

contractor_data <- contractor_data %>% #add temporary helper variable to dataset
  mutate(q27atoq27h_any = case_when(any_not_empty_f(q27atoq27h)~ 1,TRUE ~ 0))#q27atoq27h_any = 1 if any of q27atoq27h = 1

Rule07f_pre <- lapply(q28toq32, crosstabs_f,"q27atoq27h_any")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule
  mutate(across(all_of(q28toq32), ~ case_when(q27atoq27h_any == 0 ~ NA, TRUE ~ .)))

Rule07f_post <- lapply(q28toq32, crosstabs_f,"q27atoq27h_any")  #Check frequencies after implementing rule

contractor_data <- contractor_data %>% select(-q27atoq27h_any) #drop helper variable

#Rule 8: Do you look after, or give any regular help or support, to …..?####
# > If Q33 not in (1,2,3,4,5) and Q34 to Q37 are not all blank – set Q34 to Q37 to blank.

q34toq37 <-subset_qs(34,37)

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q33  %in% c("6",NA) & any_not_empty_f(q34toq37),1,0)) %>% 
  group_by("rule" = "Rule 08",
           "rule_label" = "If Q33 not in (1,2,3,4,5) and Q34 to Q37 are not all blank – set Q34 to Q37 to blank") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule08_pre <- lapply(q34toq37, crosstabs_f,"q33")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q34toq37),~ case_when(q33 %in% c("6",NA) ~ NA,TRUE ~ .)))

Rule08_post <- lapply(q34toq37, crosstabs_f,"q33")  #Check frequencies after implementing rule

#Rule 9.	Q35. Have you received any support to help with your caring role in the last 12 months?####
#9a If Q35f = 1 and any of Q35a to Q35e = 1 – set Q35f to blank.
#9b If Q35g = 1 and any of Q35a to Q35e = 1 – set Q35g to blank.
#9c If Q35f = 1 and q35g = 1 – set Q35g to blank.

q35toq35 <-subset_qs(35,35)
q35atoq35e <- q35toq35[!q35toq35 %in% c("q35f","q35g")]

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q35f  == "1" & any_not_empty_f(q35atoq35e),1,0)) %>% 
  group_by("rule" = "Rule 09a",
           "rule_label" = "If Q35f = 1 and any of Q35a to Q35e = 1 – set Q35f to blank.") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule09a_pre <- lapply(q35atoq35e, crosstabs_f,"q35f")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q35f = if_else(any_not_empty_f(q35atoq35e),NA,q35f))

Rule09a_post <- lapply(q35atoq35e, crosstabs_f,"q35f")  #Check frequencies after implementing rule

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q35g  == "1" & any_not_empty_f(q35atoq35e),1,0)) %>% 
  group_by("rule" = "Rule 09b",
           "rule_label" = "If Q35g = 1 and any of Q35a to Q35e = 1 – set Q35g to blank.") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule09b_pre <- lapply(q35atoq35e, crosstabs_f,"q35g")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q35g = if_else(any_not_empty_f(q35atoq35e),NA,q35g))

Rule09b_post <- lapply(q35atoq35e, crosstabs_f,"q35g")  #Check frequencies after implementing rule

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q35f  == "1" & q35g  == "1",1,0)) %>% 
  group_by("rule" = "Rule 09c",
           "rule_label" = "If Q35f = 1 and q35g = 1 – set Q35g to blank.") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule09c_pre <- lapply("q35f", crosstabs_f,"q35g")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q35g = case_when(q35f == "1"~ NA,TRUE ~ q35g))

Rule09c_post <- lapply("q35f", crosstabs_f,"q35g")   #Check frequencies after implementing rule

#Rule 10. Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more? ####
#If Q39 <> 1 (this is equivalent to 2 or NA) (and Q40 & Q41 are not all blank) - set Q40 and Q41 to blank.
#Note that this doesn't overwrite q40j_other
q40toq41 <-subset_qs(40,41)

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(q39  %in% c("2",NA) & any_not_empty_f(q40toq41),1,0)) %>% 
  group_by("rule" = "Rule 10",
           "rule_label" = "If Q39 <> 1 (and Q40 & Q41 are not all blank ) - set Q40 and Q41 to blank") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule10_pre <- lapply(q40toq41, crosstabs_f,"q39")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(across(all_of(q40toq41),~ case_when(q39 %in% c("2",NA) ~ NA,TRUE ~ .)))

Rule10_post <- lapply(q40toq41, crosstabs_f,"q39")  #Check frequencies after implementing rule

#Rule 11. Q32: SCAU to manually recode where the free-text response suggests the survey respondent ####
#should have ticked a different box. SCAU have provided an excel file containing the patientID and 
#recoded responses to Q32 (1-8/a-h) only for survey respondents who provided a free-text comment in Q32.
#q32a to q32h in provided file are identical to those in original file so these are not needed
#Assume NA is same as 0 - no evidence of unmet need
scau_file <- read.xlsx(paste0(data_path,"results/q32_comments_recode analysis_25-26 - 2026-05-12.xlsx"))%>% 
  filter(is.na(genuine_other)| genuine_other != "Final assessment") %>%  #remove 2nd header row
  select(patientid_sg,genuine_other) %>% 
  mutate(genuine_other = case_when(genuine_other == "0" ~ NA,TRUE ~ genuine_other))

contractor_data <- contractor_data %>% 
  left_join(scau_file,by = c("patientid_sg"))

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else((is.na(q32h) & !is.na(genuine_other))|
                                (!is.na(q32h) & is.na(genuine_other)),1,0)) %>% 
  group_by("rule" = "Rule 11",
           "rule_label" = "If Q32h is in conflict with free text provided, recode") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule11_pre <- lapply("q32h", crosstabs_f,"genuine_other")  #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q32h = case_when(is.na(genuine_other) ~ NA,
                          genuine_other=="1" ~ "1", TRUE ~ "Error")) 

Rule11_post <- lapply("q32h", crosstabs_f,"genuine_other")  #Check frequencies after implementing rule
contractor_data <- contractor_data %>% 
  select(-genuine_other) #remove field as now incorporated with q32h

#Rule 12.	Recoding based on free-text responses####
# Q40, Q43 and Q44: Retain the tick box option. If no boxes ticked, use the free text given to map to one of the options only where the wording matches, i.e. map to ‘white’ if free text is ‘white’. If no match, then treated as ‘N/A’.
# Rule 12a. Q40 - Condition, effects. Recoding needs to only apply if Q39 == 1, and none of q40 a to j have been ticked.
q40all <-subset_qs(40,40)
q40atoq40j <-q40all[!q40all =="q40k"]

recoded_free_text_q40 <- contractor_data %>% #create a file with all the values to be recoded.
  select(patientid,q39,all_of(q40all),q40j_other_redacted) %>% 
  filter(q39 == "1" & all_empty_f(q40atoq40j)
            & !is.na(q40j_other_redacted) & str_replace_all(q40j_other_redacted," ","") != "") %>% 
  mutate(a = case_when(grepl("vision|blind|sight",q40j_other_redacted) &
                       !grepl("in future",q40j_other_redacted) ~ "1",TRUE ~ NA),
       b = case_when(grepl("hearing|deaf",q40j_other_redacted) ~ "1",TRUE ~ NA),
       c = case_when(grepl("mobility|walk|clim",q40j_other_redacted) &
                       !grepl("hyper|stop me|but still|in the future|can still run",q40j_other_redacted) ~ "1",TRUE ~ NA),
       d = case_when(grepl("dexterity|lift|carry",q40j_other_redacted) ~ "1",TRUE ~ NA),
       e = case_when(grepl("learn|understand|concentrat",q40j_other_redacted) ~ "1",TRUE ~ NA),
       f = case_when(grepl("memory",q40j_other_redacted) ~ "1",TRUE ~ NA),
       g = case_when(grepl("mental",q40j_other_redacted) & 
                       ! grepl("gone now",q40j_other_redacted)~ "1",TRUE ~ NA),
       h = case_when(grepl("stamina|breath|fatigue",q40j_other_redacted) ~ "1",TRUE ~ NA),
       i = case_when(grepl("social|behaviour|autism|spectrum|asd|adhd",q40j_other_redacted) ~ "1",TRUE ~ NA)) %>% 
  filter(!is.na(a)|!is.na(b)|!is.na(c)|!is.na(d)|!is.na(e)|!is.na(f)|!is.na(g)|!is.na(h)|!is.na(i)) %>% 
  select(patientid,a:i) %>% 
  mutate(recode = 1) #create a help variable to indicate that q40 is to be recoded
  
contractor_data <- contractor_data %>% 
  left_join(recoded_free_text_q40,by = c("patientid"))

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(recode == 1,1,0)) %>% 
  group_by("rule" = "Rule 12a",
           "rule_label" = "Recode Q40 according to free text") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule12a_pre <- lapply(contractor_data[q40all], tabyl)#Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
   mutate(q40a = case_when(a == "1" & is.na(q40a) ~ "1",TRUE ~ q40a),
          q40b = case_when(b == "1" & is.na(q40b) ~ "1",TRUE ~ q40b),
          q40c = case_when(c == "1" & is.na(q40c) ~ "1",TRUE ~ q40c),
          q40d = case_when(d == "1" & is.na(q40d) ~ "1",TRUE ~ q40d),
          q40e = case_when(e == "1" & is.na(q40e) ~ "1",TRUE ~ q40e),
          q40f = case_when(f == "1" & is.na(q40f) ~ "1",TRUE ~ q40f),
          q40g = case_when(g == "1" & is.na(q40g) ~ "1",TRUE ~ q40g),
          q40h = case_when(h == "1" & is.na(q40h) ~ "1",TRUE ~ q40h),
          q40i = case_when(i == "1" & is.na(q40i) ~ "1",TRUE ~ q40i))

Rule12a_post <- lapply(contractor_data[q40all], tabyl)#Check frequencies after implementing rule

contractor_data <- contractor_data %>% #drop helper variables
  select(-matches("^[a-z]$"),-recode)

# Rule 12b. Q43: sexual orientation. Recoding only to apply when none of Q43 has been ticked
table(contractor_data$q43)
recoded_free_text_q43 <- contractor_data %>% #create a file with all the values to be recoded.
  select(patientid,q43,q43_other_redacted) %>% 
  filter(!is.na(q43_other_redacted) & is.na(q43)) %>% 
  mutate(a = case_when(grepl("straight|hetero",q43_other_redacted) ~ "1",TRUE ~ NA),
         b = case_when(grepl("gay|lesbian",q43_other_redacted) 
                                 & !grepl("but ",q43_other_redacted)~ "1",TRUE ~ NA),
         c = case_when(grepl("bisexual",q43_other_redacted) ~ "1",TRUE ~ NA))%>% 
  filter(!is.na(a)|!is.na(b)|!is.na(c)) %>% 
  mutate(recode = 1) %>% 
  select(patientid,a:c,recode) 

contractor_data <- contractor_data %>% 
           left_join(recoded_free_text_q43,by = c("patientid"))

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(recode == 1,1,0)) %>% 
  group_by("rule" = "Rule 12b",
           "rule_label" = "Recode Q43 according to free text") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule12b_pre <- list(tabyl(contractor_data,q43) %>%  adorn_totals()) #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q43 = case_when(a == 1 & is.na(q43) ~ "1",
                         b == 1 & is.na(q43) ~ "2",
                         c == 1 & is.na(q43) ~ "3",TRUE ~ q43))

Rule12b_post <- list(tabyl(contractor_data,q43) %>%  adorn_totals()) #Check frequencies after implementing rule

contractor_data <- contractor_data %>% #drop helper variables
  select(-matches("^[a-z]$"),recode)

# Rule 12c. Q44: ethnicity. Recoding only to apply Q44 has not been ticked

recoded_free_text_q44 <- contractor_data %>% #create a file with all the values to be recoded.
  select(patientid,q44,q44_other_redacted) %>% 
  filter(!is.na(q44_other_redacted) & is.na(q44)) %>% 
  mutate(q44_recode = case_when(grepl("african",q44_other_redacted) ~ "4",
                   grepl("mixed|multiple",q44_other_redacted) ~ "2",
                   grepl("white",q44_other_redacted) ~ "1",
                   grepl("asian",q44_other_redacted) ~ "3",
                   grepl("caribbean|black",q44_other_redacted) ~ "5",TRUE ~ NA)) %>% 
  filter(!is.na(q44_recode)) %>% 
  select(patientid,q44_recode) 

contractor_data <- contractor_data %>% 
  left_join(recoded_free_text_q44,by = c("patientid"))

rule_table <- contractor_data %>% 
  mutate(rule_value = if_else(!is.na(q44_recode),1,0)) %>% 
  group_by("rule" = "Rule 12c",
           "rule_label" = "Recode Q44 according to free text") %>% 
  summarise(value = sum(rule_value,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(rule_table)

Rule12c_pre <- list(tabyl(contractor_data,q44) %>%  adorn_totals()) #Check frequencies before implementing rule

contractor_data <- contractor_data %>% #Implement rule:
  mutate(q44 = case_when(q44_recode == "1" & is.na(q44) ~ "1",
                         q44_recode == "2" & is.na(q44) ~ "2",
                         q44_recode == "3" & is.na(q44) ~ "3",
                         q44_recode == "4" & is.na(q44) ~ "4" ,
                         q44_recode == "5" & is.na(q44) ~ "5",TRUE ~ q44))

Rule12c_post <- list(tabyl(contractor_data,q44) %>%  adorn_totals()) #Check frequencies after implementing rule

contractor_data <- contractor_data %>% #drop helper variables
  select(-q44_recode)

#This outputs the recoded frequencies of all the question responses
post_validation_freq <- apply(contractor_data[questions], MARGIN=2, table)

#TATA Rule: Apply Tick all that apply processing rule. ####
#Add in variables to get 'tick all that apply' totals 
#Blanks in TATA variables should be set to "No" (0) unless none of the response options were ticked, in which case all response options should be set to NA.

contractor_data <- contractor_data %>%
  mutate(q08 = case_when(any_not_empty_f(subset_qs(8,8))~ "1", TRUE ~ NA), #if any of the q8 questions are not zero or NA, then q08 (total) = 1, else it is NA
         q14 = case_when(any_not_empty_f(subset_qs(14,14)) ~ "1",TRUE ~ NA), 
         q20 = case_when(any_not_empty_f(subset_qs(20,20)) ~ "1",TRUE ~ NA), 
         q27 = case_when(any_not_empty_f(subset_qs(27,27)) ~ "1",TRUE ~ NA), 
         q28 = case_when(any_not_empty_f(subset_qs(28,28)) ~ "1",TRUE ~ NA),
         q32 = case_when(any_not_empty_f(subset_qs(32,32)) ~ "1",TRUE ~ NA), 
         q34 = case_when(any_not_empty_f(subset_qs(34,34)) ~ "1",TRUE ~ NA), 
         q35 = case_when(any_not_empty_f(subset_qs(35,35)) ~ "1",TRUE ~ NA),
         q40 = case_when(any_not_empty_f(subset_qs(40,40)) ~ "1",TRUE ~ NA)) %>% 
  mutate(across(all_of(subset_qs(8,8)), function(x) if_else(q08 == "1", replace_na(x,"0"),x)), #if q08 (total) is 1, recode all of the q8 questions from NA to 0
         across(all_of(subset_qs(14,14)), function(x) if_else(q14 == "1", replace_na(x,"0"),x)),
         across(all_of(subset_qs(20,20)), function(x) if_else(q20 == "1", replace_na(x,"0"),x)),
         across(all_of(subset_qs(27,27)), function(x) if_else(q27 == "1", replace_na(x,"0"),x)),
         across(all_of(subset_qs(28,28)), function(x) if_else(q28 == "1", replace_na(x,"0"),x)),
         across(all_of(subset_qs(32,32)), function(x) if_else(q32 == "1", replace_na(x,"0"),x)),
         across(all_of(subset_qs(34,34)), function(x) if_else(q34 == "1", replace_na(x,"0"),x)),
         across(all_of(subset_qs(35,35)), function(x) if_else(q35 == "1", replace_na(x,"0"),x)),
         across(all_of(subset_qs(40,40)), function(x) if_else(q40 == "1", replace_na(x,"0"),x)))

#Create rule summary####

# Get list of Rules objects"
rule_list <- sort(c(ls(pattern = "^Rule.*pre"),ls(pattern = "^Rule.*post")))
rule_table <- rule_table %>% arrange(rule)

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

# lapply(rule_list,write_out_list_f) #this doesn't work! there is a longform below
# for (sheet in rule_list) {
#   write_out_list_f3(sheet)}
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
write_out_list_f(Rule09a_post)
write_out_list_f(Rule09a_pre)
write_out_list_f(Rule09b_post)
write_out_list_f(Rule09b_pre)
write_out_list_f(Rule09c_post)
write_out_list_f(Rule09c_pre)
write_out_list_f(Rule10_post)
write_out_list_f(Rule10_pre)
write_out_list_f(Rule11_post)
write_out_list_f(Rule11_pre)
write_out_list_f(Rule12a_post)
write_out_list_f(Rule12a_pre)
write_out_list_f(Rule12b_post)
write_out_list_f(Rule12b_pre)
write_out_list_f(Rule12c_post)
write_out_list_f(Rule12c_pre)

writeData(template, "summary", today(), startCol = 2, startRow = 4, colNames = FALSE)
writeData(template, "summary", summary_file_name, startCol = 2, startRow = 5, colNames = FALSE)
writeData(template, "summary", summary_record_count, startCol = 2, startRow = 8, colNames = FALSE)
writeData(template, "summary", summary_duplicates, startCol = 2, startRow = 10, colNames = FALSE)
writeData(template, "summary", response_codes, startCol = 2, startRow = 12, colNames = TRUE)
writeData(template,"rules_summary",rule_table, startCol = 1, startRow = 2, colNames = TRUE)
saveWorkbook(template, paste0(analysis_output_path,"file_overview_populated_",today(),".xlsx"), overwrite =TRUE)

#check if the same as before
hist.file <- readRDS(paste0(data_path,"results/data_Validated_results.rds"))
all.equal(hist.file,contractor_data)

#Save outfile####
saveRDS(contractor_data, file=paste0(data_path,"results/data_Validated_results.rds"))

#Create and save out anonymised version of validated results for SG:
SGFile_Validated <- contractor_data %>% 
   select(-c(qh_psid,patientid)) #Remove PSID(QH patient identifier) & PatientID (PHS patient identifier)
saveRDS(SGFile_Validated, file=paste0(data_path,"results/anonymised_data_Validated_results_for_SG.rds"))
