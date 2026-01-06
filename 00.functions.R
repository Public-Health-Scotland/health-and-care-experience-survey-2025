###set vectors of report areas #CH need to add 2 locality report areas too?
report_areas <- c("scotland","gp_prac_no","practice_hscp_code","practice_board_code" ,"practice_hscp_cluster")
report_areas_output <- c("Scotland","GP","HSCP","Health Board" ,"GPCL")
report_area_wt <- c("nat_wt","hb_wt","hscp_wt","gpcl_wt","gp_wt")

###set vectors of questions numbers
questions <- unique(readRDS(paste0(lookup_path,"questions.rds")))
information_questions <- readRDS(paste0(lookup_path,"information_questions.rds"))
information_questions_tata <- readRDS(paste0(lookup_path,"information_questions_tata.rds"))
percent_positive_questions <- readRDS(paste0(lookup_path,"percent_positive_questions.rds"))

crosstabs_f <- function(x,comp_var) {  # function to produce list of crosstabs
  tabyl(contractor_data, !!sym(comp_var), !!sym(x)) %>%  adorn_totals(where = c("row", "col")) %>% adorn_title("combined")
}

rowAny <- function(x) {rowSums(x,na.rm = TRUE) > 0} #checks whether any of list of variables is not NA or 0
rowAll <- function(x) {rowSums(x,na.rm = TRUE) == 0} #checks whether all of list of variables are NA or 0
subset_qs <- function(x,y) {questions[between(as.numeric(str_extract(questions,"\\d+")),x,y)]} #function to subset questions from questions vector

#age groups

two_age_bands <- function(age) {
  
  two_age_bands <- character(length(age))
  two_age_bands <- case_when(age >=17 & age <= 54 ~ "17-54",
                             age >=55 & age <= 150 ~ "55 plus",
                             TRUE ~ "Dummy")
  return(two_age_bands)
  
}

three_age_bands <- function(age) {
  
  three_age_bands <- character(length(age))
  three_age_bands <- case_when(age >=17 & age <= 44 ~ "17-44",
                               age >=45 & age <= 64 ~ "45-64",
                               age >=65 & age <= 150 ~ "65 plus",
                               TRUE ~ "Dummy")
  return(three_age_bands)
  
}

six_age_bands <- function(age) {
  
  six_age_band <- character(length(age))
  six_age_band <- case_when(age >=17 & age <= 24 ~ "17-24",
                            age >=25 & age <= 34 ~ "25-34",
                            age >=35 & age <= 44 ~ "35-44",
                            age >=45 & age <= 54 ~ "45-54",
                            age >=55 & age <= 64 ~ "55-64",
                            age >=65 & age <= 150 ~ "65 plus",
                            TRUE ~ "Dummy")
  return(six_age_band)
  
}


