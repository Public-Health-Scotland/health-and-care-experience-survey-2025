# Written by Catriona Haddow and Martin Leitch
# November 2025.

# *******************************************************************************************************************************************************************
# File Name: 7 Calculate_final_weights.R.
# Description: This syntax applies the weights to the validated results and produces a 
# patient level file with weights at all levels.
# 
# Input files: #UPDATE!
#"output/analysis_output/responses_with_categories.rds"
#"output/weights/nat_s1_weights.rds - nat_s6_weights.rds"
#"output/weights/hb_s1_weights.rds - hb_s6_weights.rds"
#"output/weights/hscp_s1_weights.rds - hscp_s6_weights.rds"
#"output/weights/gpcl_s1_weights.rds - gpcl_s6_weights.rds"
#"output/weights/gp_s1_weights.rds - gp_s6_weights.rds"
#"output/weights/ca_s1_weights.rds - ca_s6_weights.rds"
#"output/weights/locality_s1_weights.rds - locality_s6_weights.rds"
#"output/weights/ca_locality_s1_weights.rds - ca_locality_s6_weights.rds"

# Output files:
#"output/weights/weights_vars.csv"
#"output/weights/weights_vars.rds"
#"output/weights/weights_vars_sg.csv"
#"output/weights/weights_vars_sg.rds"

# 
# *****************************************
# *****************************************
# *******************************************************************************************************************************************************************.

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#define the weights function
add_final_weights <- function(section_no) {
  responses <- responses %>%
  mutate(in_section = {{section_no}})
  responses <- responses %>%
    left_join(weights_section,
              by = c("in_section","report_area","age_band","sex")) %>%
  mutate(final_weight = if_else({{section_no}} == "Y", gp_wt1*weight,0)) %>%
    select(-weight,-section)
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

# Match on the Weight 2 values.
#read in all the national files, selecting only the columns which are needed and renaming the weight variable to match the file name

files = list.files(path = weights_path_202324, pattern = '^nat_s[0-9]_weights.rds')
file_names = gsub("s.rds", "", files)
weight_list = lapply(files, function (x) readRDS(paste0(weights_path_202324,x)))
names(weight_list) <- gsub(".*/(.*)\\..*", "\\1", file_names)
weight_list <- lapply(file_names, function(x) select(weight_list[[x]],-c(5:9)))

weight_list <- lapply(seq_along(file_names), function(x) {
  weight_list[[x]][6] <- names(weight_list[[x]])[4]
  names(weight_list[[x]])[6] <- "section"
  names(weight_list[[x]])[4] <- "in_section"
  weight_list[[x]]
})

weight_list <- bind_rows(weight_list)
sections <- unique(weight_list$section)

#This should all be possible to do within function, then lapply on sections vector, but can't get it to work!
weights_section <- weight_list[weight_list$section == sections[1],]
responses <- add_final_weights(s1)
responses <- responses %>% rename("s1_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[2],]
responses <- add_final_weights(s2)
responses <- responses %>% rename("s2_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[3],]
responses <- add_final_weights(s3)
responses <- responses %>% rename("s3_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[4],]
responses <- add_final_weights(s4)
responses <- responses %>% rename("s4_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[5],]
responses <- add_final_weights(s5)
responses <- responses %>% rename("s5_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[6],]
responses <- add_final_weights(s6)
responses <- responses %>% rename("s6_weight" = final_weight)

#trim the weights
responses <- trim_weights("all")

#tidy the file
old_names <- c("s1_wt_final","s2_wt_final","s3_wt_final","s4_wt_final","s5_wt_final","s6_wt_final")
new_names <- c("nat_s1_wt","nat_s2_wt","nat_s3_wt","nat_s4_wt","nat_s5_wt","nat_s6_wt")
drop_vars <- c("s1_weight","s2_weight","s3_weight","s4_weight","s5_weight","s6_weight")

responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#NHS Board weights####
responses <- responses %>%  mutate(report_area = practice_board_code,age_band = age_band_3)

# Match on the Weight 2 values.
#read in all the weight files, selecting only the columns which are needed and renaming the weight variable to match the file name

files = list.files(path = 'output/weights/', pattern = '^hb_s[0-9]_weights.rds')
file_names = gsub("s.rds", "", files)
weight_list = lapply(files, function (x) readRDS(paste0('output/weights/',x)))
names(weight_list) <- gsub(".*/(.*)\\..*", "\\1", file_names)
weight_list <- lapply(file_names, function(x) select(weight_list[[x]],-c(5:9)))

weight_list <- lapply(seq_along(file_names), function(x) {
  weight_list[[x]][6] <- names(weight_list[[x]])[4]
  names(weight_list[[x]])[6] <- "section"
  names(weight_list[[x]])[4] <- "in_section"
  weight_list[[x]]
})

weight_list <- bind_rows(weight_list)

#This should all be possible to do within function, then lapply on sections vector, but can't get it to work!
weights_section <- weight_list[weight_list$section == sections[1],]
responses <- add_final_weights(s1)
responses <- responses %>% rename("s1_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[2],]
responses <- add_final_weights(s2)
responses <- responses %>% rename("s2_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[3],]
responses <- add_final_weights(s3)
responses <- responses %>% rename("s3_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[4],]
responses <- add_final_weights(s4)
responses <- responses %>% rename("s4_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[5],]
responses <- add_final_weights(s5)
responses <- responses %>% rename("s5_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[6],]
responses <- add_final_weights(s6)
responses <- responses %>% rename("s6_weight" = final_weight)

#trim the weights
responses <- trim_weights("all")

#tidy file
new_names <- c("hb_s1_wt","hb_s2_wt","hb_s3_wt","hb_s4_wt","hb_s5_wt","hb_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))


#HSCP weights####
#apply weight categories
responses <- responses %>% mutate(report_area = practice_hscp_code,age_band = age_band_3)

# Match on the Weight 2 values.
#read in all the weight files, selecting only the columns which are needed and renaming the weight variable to match the file name

files = list.files(path = 'output/weights/', pattern = '^hscp_s[0-9]_weights.rds')
file_names = gsub("s.rds", "", files)
weight_list = lapply(files, function (x) readRDS(paste0('output/weights/',x)))
names(weight_list) <- gsub(".*/(.*)\\..*", "\\1", file_names)
weight_list <- lapply(file_names, function(x) select(weight_list[[x]],-c(5:9)))

weight_list <- lapply(seq_along(file_names), function(x) {
  weight_list[[x]][6] <- names(weight_list[[x]])[4]
  names(weight_list[[x]])[6] <- "section"
  names(weight_list[[x]])[4] <- "in_section"
  weight_list[[x]]
})

weight_list <- bind_rows(weight_list)

#This should all be possible to do within function, then lapply on sections vector, but can't get it to work!
weights_section <- weight_list[weight_list$section == sections[1],]
responses <- add_final_weights(s1)
responses <- responses %>% rename("s1_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[2],]
responses <- add_final_weights(s2)
responses <- responses %>% rename("s2_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[3],]
responses <- add_final_weights(s3)
responses <- responses %>% rename("s3_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[4],]
responses <- add_final_weights(s4)
responses <- responses %>% rename("s4_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[5],]
responses <- add_final_weights(s5)
responses <- responses %>% rename("s5_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[6],]
responses <- add_final_weights(s6)
responses <- responses %>% rename("s6_weight" = final_weight)

#trim the weights
responses <- trim_weights("all")

#tidy file
new_names <- c("hscp_s1_wt","hscp_s2_wt","hscp_s3_wt","hscp_s4_wt","hscp_s5_wt","hscp_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#GP Cluster weights####
#apply weight categories
responses <- responses %>% mutate(report_area = practice_hscp_cluster,age_band = age_band_2)

# Match on the Weight 2 values.
#read in all the weight files, selecting only the columns which are needed and renaming the weight variable to match the file name

files = list.files(path = 'output/weights/', pattern = '^gpcl_s[0-9]_weights.rds')
file_names = gsub("s.rds", "", files)
weight_list = lapply(files, function (x) readRDS(paste0('output/weights/',x)))
names(weight_list) <- gsub(".*/(.*)\\..*", "\\1", file_names)
weight_list <- lapply(file_names, function(x) select(weight_list[[x]],-c(5:9)))

weight_list <- lapply(seq_along(file_names), function(x) {
  weight_list[[x]][6] <- names(weight_list[[x]])[4]
  names(weight_list[[x]])[6] <- "section"
  names(weight_list[[x]])[4] <- "in_section"
  weight_list[[x]]
})

weight_list <- bind_rows(weight_list)

#This should all be possible to do within function, then lapply on sections vector, but can't get it to work!
weights_section <- weight_list[weight_list$section == sections[1],]
responses <- add_final_weights(s1)
responses <- responses %>% rename("s1_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[2],]
responses <- add_final_weights(s2)
responses <- responses %>% rename("s2_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[3],]
responses <- add_final_weights(s3)
responses <- responses %>% rename("s3_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[4],]
responses <- add_final_weights(s4)
responses <- responses %>% rename("s4_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[5],]
responses <- add_final_weights(s5)
responses <- responses %>% rename("s5_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[6],]
responses <- add_final_weights(s6)
responses <- responses %>% rename("s6_weight" = final_weight)

#trim the weights
responses <- trim_weights("all")

#tidy file
new_names <- c("gpcl_s1_wt","gpcl_s2_wt","gpcl_s3_wt","gpcl_s4_wt","gpcl_s5_wt","gpcl_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#GP weights####
#apply weight categories
responses <- responses %>% mutate(report_area = gp_prac_no,age_band = age_band_2)
# Match on the Weight 2 values.
#read in all the weight files, selecting only the columns which are needed and renaming the weight variable to match the file name

files = list.files(path = 'output/weights/', pattern = '^gp_s[0-9]_weights.rds')
file_names = gsub("s.rds", "", files)
weight_list = lapply(files, function (x) readRDS(paste0('output/weights/',x)))
names(weight_list) <- gsub(".*/(.*)\\..*", "\\1", file_names)
weight_list <- lapply(file_names, function(x) select(weight_list[[x]],-c(5:9)))

weight_list <- lapply(seq_along(file_names), function(x) {
  weight_list[[x]][6] <- names(weight_list[[x]])[4]
  names(weight_list[[x]])[6] <- "section"
  names(weight_list[[x]])[4] <- "in_section"
  weight_list[[x]]
})

weight_list <- bind_rows(weight_list)

#This should all be possible to do within function, then lapply on sections vector, but can't get it to work!
weights_section <- weight_list[weight_list$section == sections[1],]
responses <- add_final_weights(s1)
responses <- responses %>% rename("s1_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[2],]
responses <- add_final_weights(s2)
responses <- responses %>% rename("s2_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[3],]
responses <- add_final_weights(s3)
responses <- responses %>% rename("s3_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[4],]
responses <- add_final_weights(s4)
responses <- responses %>% rename("s4_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[5],]
responses <- add_final_weights(s5)
responses <- responses %>% rename("s5_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[6],]
responses <- add_final_weights(s6)
responses <- responses %>% rename("s6_weight" = final_weight)

#trim the weights
responses <- trim_weights("all")

#tidy file
new_names <- c("gp_s1_wt","gp_s2_wt","gp_s3_wt","gp_s4_wt","gp_s5_wt","gp_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#CA weights####
#apply weight categories
responses <- responses %>% mutate(report_area = practice_council_code,age_band = age_band_3)
# Match on the Weight 2 values.
#read in all the weight files, selecting only the columns which are needed and renaming the weight variable to match the file name

files = list.files(path = 'output/weights/', pattern = '^ca_s[0-9]_weights.rds')
file_names = gsub("s.rds", "", files)
weight_list = lapply(files, function (x) readRDS(paste0('output/weights/',x)))
names(weight_list) <- gsub(".*/(.*)\\..*", "\\1", file_names)
weight_list <- lapply(file_names, function(x) select(weight_list[[x]],-c(5:9)))

weight_list <- lapply(seq_along(file_names), function(x) {
  weight_list[[x]][6] <- names(weight_list[[x]])[4]
  names(weight_list[[x]])[6] <- "section"
  names(weight_list[[x]])[4] <- "in_section"
  weight_list[[x]]
})

weight_list <- bind_rows(weight_list)

#This should all be possible to do within function, then lapply on sections vector, but can't get it to work!
weights_section <- weight_list[weight_list$section == sections[1],]
responses <- add_final_weights(s1)
responses <- responses %>% rename("s1_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[2],]
responses <- add_final_weights(s2)
responses <- responses %>% rename("s2_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[3],]
responses <- add_final_weights(s3)
responses <- responses %>% rename("s3_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[4],]
responses <- add_final_weights(s4)
responses <- responses %>% rename("s4_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[5],]
responses <- add_final_weights(s5)
responses <- responses %>% rename("s5_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[6],]
responses <- add_final_weights(s6)
responses <- responses %>% rename("s6_weight" = final_weight)

#trim the weights
responses <- trim_weights("all")

#tidy file
new_names <- c("ca_s1_wt","ca_s2_wt","ca_s3_wt","ca_s4_wt","ca_s5_wt","ca_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#Locality weights####
#apply weight categories
responses <- responses %>% mutate(report_area = patient_hscp_locality,age_band = age_band_2)
# Match on the Weight 2 values.
#read in all the weight files, selecting only the columns which are needed and renaming the weight variable to match the file name

files = list.files(path = 'output/weights/', pattern = '^locality_s[0-9]_weights.rds')
file_names = gsub("s.rds", "", files)
weight_list = lapply(files, function (x) readRDS(paste0('output/weights/',x)))
names(weight_list) <- gsub(".*/(.*)\\..*", "\\1", file_names)
weight_list <- lapply(file_names, function(x) select(weight_list[[x]],-c(5:9)))

weight_list <- lapply(seq_along(file_names), function(x) {
  weight_list[[x]][6] <- names(weight_list[[x]])[4]
  names(weight_list[[x]])[6] <- "section"
  names(weight_list[[x]])[4] <- "in_section"
  weight_list[[x]]
})

weight_list <- bind_rows(weight_list)

#This should all be possible to do within function, then lapply on sections vector, but can't get it to work!
weights_section <- weight_list[weight_list$section == sections[1],]
responses <- add_final_weights(s1)
responses <- responses %>% rename("s1_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[2],]
responses <- add_final_weights(s2)
responses <- responses %>% rename("s2_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[3],]
responses <- add_final_weights(s3)
responses <- responses %>% rename("s3_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[4],]
responses <- add_final_weights(s4)
responses <- responses %>% rename("s4_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[5],]
responses <- add_final_weights(s5)
responses <- responses %>% rename("s5_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[6],]
responses <- add_final_weights(s6)
responses <- responses %>% rename("s6_weight" = final_weight)

#trim the weights
responses <- trim_weights("all")

#tidy file
new_names <- c("locality_s1_wt","locality_s2_wt","locality_s3_wt","locality_s4_wt","locality_s5_wt","locality_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

#ca_Locality weights####
#apply weight categories
responses <- responses %>% mutate(report_area = patient_ca_locality,age_band = age_band_2)
# Match on the Weight 2 values.
#read in all the weight files, selecting only the columns which are needed and renaming the weight variable to match the file name

files = list.files(path = 'output/weights/', pattern = '^ca_locality_s[0-9]_weights.rds')
file_names = gsub("s.rds", "", files)
weight_list = lapply(files, function (x) readRDS(paste0('output/weights/',x)))
names(weight_list) <- gsub(".*/(.*)\\..*", "\\1", file_names)
weight_list <- lapply(file_names, function(x) select(weight_list[[x]],-c(5:9)))

weight_list <- lapply(seq_along(file_names), function(x) {
  weight_list[[x]][6] <- names(weight_list[[x]])[4]
  names(weight_list[[x]])[6] <- "section"
  names(weight_list[[x]])[4] <- "in_section"
  weight_list[[x]]
})

weight_list <- bind_rows(weight_list)

#This should all be possible to do within function, then lapply on sections vector, but can't get it to work!
weights_section <- weight_list[weight_list$section == sections[1],]
responses <- add_final_weights(s1)
responses <- responses %>% rename("s1_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[2],]
responses <- add_final_weights(s2)
responses <- responses %>% rename("s2_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[3],]
responses <- add_final_weights(s3)
responses <- responses %>% rename("s3_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[4],]
responses <- add_final_weights(s4)
responses <- responses %>% rename("s4_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[5],]
responses <- add_final_weights(s5)
responses <- responses %>% rename("s5_weight" = final_weight)

weights_section <- weight_list[weight_list$section == sections[6],]
responses <- add_final_weights(s6)
responses <- responses %>% rename("s6_weight" = final_weight)

#trim the weights
responses <- trim_weights("all")

#tidy file
new_names <- c("ca_locality_s1_wt","ca_locality_s2_wt","ca_locality_s3_wt","ca_locality_s4_wt","ca_locality_s5_wt","ca_locality_s6_wt")
responses <- responses %>% setnames(old_names, new_names)%>% select(-all_of(drop_vars))

####create_weights_only_file
####PHS_weights_only_file
weights_vars <- responses %>% ungroup() %>%
  select(qh_psid,patientid,patientid_sg,ends_with('_wt'),gp_wt1,eligible_pats)

#check if the same as before
hist.file <- readRDS(paste0(weights_path,"weights_vars.rds"))
identical(hist.file,weights_vars)

write.table(weights_vars,paste0(weights_path,"weights_vars.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(weights_vars,paste0(weights_path,"weights_vars.rds"))

####create SG_weights_only_file (not locality weights)
weights_vars_sg <- weights_vars

#remove non sg required columns
weights_vars_sg <- weights_vars_sg %>% select(-qh_psid, - patientid, - starts_with('locality'), - starts_with('ca_locality'))

#check if the same as before
hist.file <- readRDS(paste0(weights_path,"weights_vars_sg.rds"))
identical(hist.file,weights_vars_sg)

write.table(weights_vars_sg,paste0(weights_path,"weights_vars_sg.csv"),quote = TRUE, sep = ",", na = "NA", dec = ".", row.names = FALSE)
saveRDS(weights_vars_sg,paste0(weights_path,"weights_vars_sg.rds"))
