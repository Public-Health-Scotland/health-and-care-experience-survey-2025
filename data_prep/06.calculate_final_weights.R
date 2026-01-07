# *****************************************
# January 2025 WIP.

# *******************************************************************************************************************************************************************
# File Name: 06. calculate_final_weights.R.
# Description: This syntax applies the weights to the validated results and produces a 
# patient level file with weights at all levels.
# 
# Input files:
# analysis_output_path,"responses_with_categories.rds" #created in 05.calculate_non_response_weight2.R
# weights_path,[nat,hb,hscp,gpcl,gp,ca,locality,ca_locality]_weights.rds #created in 05.calculate_non_response_weight2.R

# Output files:
#weights_path,"weights_vars.rds", also .csv
#weights_path,weights_vars_sg.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

add_final_weights <- function(section_no) {
  varname <- paste0(deparse(substitute(section_no)), "_weight")#deparse substitute is to use variable as a value
  weights <- weights %>% 
    filter(section == str_remove(varname,"_weight")) 
  responses <- responses %>%
    mutate(in_section = {{section_no}}) %>% #curly brackets to use variable as a variable name
    left_join(weights,
              by = c("in_section","report_area","age_band","sex")) 
  responses[[varname]] <- with(responses, if_else(in_section == "Y", gp_wt1*weight,0))
  responses <- responses %>% select(-c(weight,section))
  return(responses)
}

#define the trimming function
# Aggregate the file to calculate the mean weight, to be used in capping the weights by 2 standard deviations
trim_weights <- function(level) {
  responses <- responses %>%   
    group_by({{level}}) %>% 
    mutate(s1_weight_cap = mean(s1_weight)+2*sd(s1_weight),
           s2_weight_cap = mean(s2_weight)+2*sd(s2_weight),
           s3_weight_cap = mean(s3_weight)+2*sd(s3_weight),
           s4_weight_cap = mean(s4_weight)+2*sd(s4_weight),
           s5_weight_cap = mean(s5_weight)+2*sd(s5_weight),
           s6_weight_cap = mean(s6_weight)+2*sd(s6_weight),
           s1_wt_final = if_else(s1_weight > s1_weight_cap,s1_weight_cap,s1_weight),
           s2_wt_final = if_else(s2_weight > s2_weight_cap,s2_weight_cap,s2_weight),
           s3_wt_final = if_else(s3_weight > s3_weight_cap,s3_weight_cap,s3_weight),
           s4_wt_final = if_else(s4_weight > s4_weight_cap,s4_weight_cap,s4_weight),
           s5_wt_final = if_else(s5_weight > s5_weight_cap,s5_weight_cap,s5_weight),
           s6_wt_final = if_else(s6_weight > s6_weight_cap,s6_weight_cap,s6_weight))
}

# Open the responses file with validation applied, and with categories added in script 5.
responses <- readRDS(paste0(analysis_output_path,"responses_with_categories.rds"))

#National weights####
responses <- responses %>%  mutate(report_area = "Scotland",age_band = age_band_6)

#read in the relevant weights, and bind into a data frame
nat_weights <- readRDS(paste0(weights_path,"nat_weights.rds"))
weights <- bind_rows(nat_weights, .id = "section") %>% 
  select(report_area,age_band,sex,in_section = section_category,weight,section)

#add the final weights
responses <- add_final_weights(s1)  
responses <- add_final_weights(s2) 
responses <- add_final_weights(s3) 
responses <- add_final_weights(s4) 
responses <- add_final_weights(s5) 
responses <- add_final_weights(s6) 

sections <- unique(weights$section)
for (section in sections) {
  responses <- add_final_weights(section)}#this doesn't work

#trim the weights
responses <- trim_weights("all")

#tidy the file
old_names <- c("s1_wt_final","s2_wt_final","s3_wt_final","s4_wt_final","s5_wt_final","s6_wt_final")
new_names <- c("nat_s1_wt","nat_s2_wt","nat_s3_wt","nat_s4_wt","nat_s5_wt","nat_s6_wt")
drop_vars <- c("s1_weight","s2_weight","s3_weight","s4_weight","s5_weight","s6_weight")

responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#NHS Board weights####
responses <- responses %>%  mutate(report_area = practice_board_code,age_band = age_band_3)

#read in the relevant weights, and bind into a data frame
hb_weights <- readRDS(paste0(weights_path,"hb_weights.rds"))
weights <- bind_rows(hb_weights, .id = "section") %>% 
  select(report_area,age_band,sex,in_section = section_category,weight,section)

#add the final weights for each section
responses <- add_final_weights(s1)  
responses <- add_final_weights(s2) 
responses <- add_final_weights(s3) 
responses <- add_final_weights(s4) 
responses <- add_final_weights(s5) 
responses <- add_final_weights(s6) 

responses <- trim_weights("all")#trim the weights

#tidy file
new_names <- c("hb_s1_wt","hb_s2_wt","hb_s3_wt","hb_s4_wt","hb_s5_wt","hb_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#HSCP weights####
#apply weight categories
responses <- responses %>% mutate(report_area = practice_hscp_code,age_band = age_band_3)

#read in the relevant weights, and bind into a data frame
hscp_weights <- readRDS(paste0(weights_path,"hscp_weights.rds"))
weights <- bind_rows(hscp_weights, .id = "section") %>% 
  select(report_area,age_band,sex,in_section = section_category,weight,section)

#add the final weights for each section
responses <- add_final_weights(s1)  
responses <- add_final_weights(s2) 
responses <- add_final_weights(s3) 
responses <- add_final_weights(s4) 
responses <- add_final_weights(s5) 
responses <- add_final_weights(s6) 

responses <- trim_weights("all")#trim the weights

#tidy file
new_names <- c("hscp_s1_wt","hscp_s2_wt","hscp_s3_wt","hscp_s4_wt","hscp_s5_wt","hscp_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#GP Cluster weights####
#apply weight categories
responses <- responses %>% mutate(report_area = practice_hscp_cluster,age_band = age_band_2)

#read in the relevant weights, and bind into a data frame
gpcl_weights <- readRDS(paste0(weights_path,"gpcl_weights.rds"))
weights <- bind_rows(gpcl_weights, .id = "section") %>% 
  select(report_area,age_band,sex,in_section = section_category,weight,section)

#add the final weights for each section
responses <- add_final_weights(s1)  
responses <- add_final_weights(s2) 
responses <- add_final_weights(s3) 
responses <- add_final_weights(s4) 
responses <- add_final_weights(s5) 
responses <- add_final_weights(s6) 

responses <- trim_weights("all")#trim the weights

#tidy file
new_names <- c("gpcl_s1_wt","gpcl_s2_wt","gpcl_s3_wt","gpcl_s4_wt","gpcl_s5_wt","gpcl_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#GP weights####
#apply weight categories
responses <- responses %>% mutate(report_area = gp_prac_no,age_band = age_band_2)

#read in the relevant weights, and bind into a data frame
gp_weights <- readRDS(paste0(weights_path,"gp_weights.rds"))
weights <- bind_rows(gp_weights, .id = "section") %>% 
  select(report_area,age_band,sex,in_section = section_category,weight,section)

#add the final weights for each section
responses <- add_final_weights(s1)  
responses <- add_final_weights(s2) 
responses <- add_final_weights(s3) 
responses <- add_final_weights(s4) 
responses <- add_final_weights(s5) 
responses <- add_final_weights(s6) 

responses <- trim_weights("all")#trim the weights

#tidy file
new_names <- c("gp_s1_wt","gp_s2_wt","gp_s3_wt","gp_s4_wt","gp_s5_wt","gp_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#CA weights####
#apply weight categories
responses <- responses %>% mutate(report_area = practice_council_code,age_band = age_band_3)

#read in the relevant weights, and bind into a data frame
ca_weights <- readRDS(paste0(weights_path,"ca_weights.rds"))
weights <- bind_rows(ca_weights, .id = "section") %>% 
  select(report_area,age_band,sex,in_section = section_category,weight,section)

#add the final weights for each section
responses <- add_final_weights(s1)  
responses <- add_final_weights(s2) 
responses <- add_final_weights(s3) 
responses <- add_final_weights(s4) 
responses <- add_final_weights(s5) 
responses <- add_final_weights(s6) 

responses <- trim_weights("all")#trim the weights

#tidy file
new_names <- c("ca_s1_wt","ca_s2_wt","ca_s3_wt","ca_s4_wt","ca_s5_wt","ca_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#Locality weights####
#apply weight categories
responses <- responses %>% mutate(report_area = patient_hscp_locality,age_band = age_band_2)

#read in the relevant weights, and bind into a data frame
locality_weights <- readRDS(paste0(weights_path,"locality_weights.rds"))
weights <- bind_rows(locality_weights, .id = "section") %>% 
  select(report_area,age_band,sex,in_section = section_category,weight,section)

#add the final weights for each section
responses <- add_final_weights(s1)  
responses <- add_final_weights(s2) 
responses <- add_final_weights(s3) 
responses <- add_final_weights(s4) 
responses <- add_final_weights(s5) 
responses <- add_final_weights(s6) 

responses <- trim_weights("all")#trim the weights

#tidy file
new_names <- c("locality_s1_wt","locality_s2_wt","locality_s3_wt","locality_s4_wt","locality_s5_wt","locality_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#ca_Locality weights####
#apply weight categories
responses <- responses %>% mutate(report_area = patient_ca_locality,age_band = age_band_2)

#read in the relevant weights, and bind into a data frame
ca_locality_weights <- readRDS(paste0(weights_path,"ca_locality_weights.rds"))
weights <- bind_rows(ca_locality_weights, .id = "section") %>% 
  select(report_area,age_band,sex,in_section = section_category,weight,section)

#add the final weights for each section
responses <- add_final_weights(s1)  
responses <- add_final_weights(s2) 
responses <- add_final_weights(s3) 
responses <- add_final_weights(s4) 
responses <- add_final_weights(s5) 
responses <- add_final_weights(s6) 

responses <- trim_weights("all")#trim the weights

#tidy file
new_names <- c("ca_locality_s1_wt","ca_locality_s2_wt","ca_locality_s3_wt","ca_locality_s4_wt","ca_locality_s5_wt","ca_locality_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

####create_weights_only_file
####PHS_weights_only_file
weights_vars <- responses %>% ungroup() %>%
  select(qh_psid,patientid,patientid_sg,ends_with('_wt'),gp_wt1,eligible_pats)

#check if the same as before
hist.file <- readRDS(paste0(weights_path,"weights_vars.rds"))
all.equal(hist.file,weights_vars)
write.table(weights_vars,paste0(weights_path,"weights_vars.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(weights_vars,paste0(weights_path,"weights_vars.rds"))

####create SG_weights_only_file (not locality weights)
weights_vars_sg <- weights_vars %>% 
  select(-qh_psid, - patientid, - starts_with('locality'), - starts_with('ca_locality'))#remove non sg required columns

#check if the same as before
hist.file <- readRDS(paste0(weights_path,"weights_vars_sg.rds"))
all.equal(hist.file,weights_vars_sg)
write.table(weights_vars_sg,paste0(weights_path,"weights_vars_sg.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(weights_vars_sg,paste0(weights_path,"weights_vars_sg.rds"))
