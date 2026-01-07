# *****************************************
# January 2025 WIP.
#Purpose: Create weight1 - probability of non-selection

#Inputs:
#weights_path,eligible_pats_by_gp.rds" #created in 03.create_reference_files_from_eligible_pop
#weights_path,sample_size_by_gp.rds" #created in 02.create_patient_info_files_from_sample_pop

#Outputs:
#weights_path,weight1.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

# Match sample size to eligible size
#read in files

eligible_pats_by_gp <- readRDS(paste0(weights_path,"eligible_pats_by_gp.rds"))
sample_size_by_gp <- readRDS(paste0(weights_path,"sample_size_by_gp.rds"))

weight1 <- full_join(eligible_pats_by_gp,sample_size_by_gp,by = c("gp_prac_no"))

#Divide the eligible patients by the sampled patients in each GP Practice to calculate the Weight 1 value.
weight1 <- weight1 %>%
          mutate(gp_wt1 = eligible_pats/sample_pop)

#Check range of weights - output to a check file
nrow(weight1)
mean(weight1$gp_wt1)
range(weight1$gp_wt1)
sd(weight1$gp_wt1)

weight1_sum <- weight1%>%
                group_by('level'= "level") %>%
                summarise(gp_wt1 = sum(gp_wt1))
  
#check if the same as before
hist.file <- readRDS(paste0(weights_path,"weight1.rds"))
all.equal(hist.file,weight1)
saveRDS(weight1,paste0(weights_path,"weight1.rds"))
