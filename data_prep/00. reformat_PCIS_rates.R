source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

pcis_data <- read_excel(paste0(lookup_path,"PCIS008 - Data - Nov25.xlsx"), sheet = "Data")
pcis_per_6_agebands <- pcis_data %>% 
  mutate(age_band = case_when(age_band %in% c("65+") ~ "65 plus",
                              TRUE ~ age_band),
         sex = case_when(sex == "M" ~ "1",
                         sex == "F" ~ "2", TRUE ~ "9")) %>% 
  filter(sex != "9") %>% 
  group_by(age_band,sex) %>% 
  summarise(pcis_total_patients = sum(total_patients),
            pcis_with_appointments = sum(with_appointments)) %>% 
  mutate(Y = pcis_with_appointments/pcis_total_patients,
         N = 1 - pcis_with_appointments/pcis_total_patients) %>% 
  pivot_longer(cols = c("Y","N"),names_to = "s1",values_to = "pcis_per")
  
saveRDS(pcis_per_6_agebands,paste0(lookup_path,"pcis_rates_6_agebands.rds"))

pcis_per_3_agebands <- pcis_data %>% 
  mutate(age_band = case_when(age_band %in% c("17-24","25-34","35-44") ~ "17-44",
                              age_band %in% c("45-54","55-64") ~ "45-64",
                              age_band %in% c("65+") ~ "65 plus"),
         sex = case_when(sex == "M" ~ "1",
                         sex == "F" ~ "2", TRUE ~ "9")) %>% 
  filter(sex != "9") %>% 
  group_by(age_band,sex) %>% 
  summarise(pcis_total_patients = sum(total_patients),
            pcis_with_appointments = sum(with_appointments)) %>% 
  mutate(Y = pcis_with_appointments/pcis_total_patients,
         N = 1 - pcis_with_appointments/pcis_total_patients) %>% 
  pivot_longer(cols = c("Y","N"),names_to = "s1",values_to = "pcis_per")

saveRDS(pcis_per_3_agebands,paste0(lookup_path,"pcis_rates_3_agebands.rds"))

pcis_per_2_agebands <- pcis_data %>% 
  mutate(age_band = case_when(age_band %in% c("17-24","25-34","35-44","45-54") ~ "17-54",
                              age_band %in% c("55-64","65+") ~ "55 plus"),
         sex = case_when(sex == "M" ~ "1",
                         sex == "F" ~ "2", TRUE ~ "9")) %>% 
  filter(sex != "9") %>% 
  group_by(age_band,sex) %>% 
  summarise(pcis_total_patients = sum(total_patients),
            pcis_with_appointments = sum(with_appointments)) %>% 
  mutate(Y = pcis_with_appointments/pcis_total_patients,
         N = 1 - pcis_with_appointments/pcis_total_patients) %>% 
  pivot_longer(cols = c("Y","N"),names_to = "s1",values_to = "pcis_per")

saveRDS(pcis_per_2_agebands,paste0(lookup_path,"pcis_rates_2_agebands.rds"))

