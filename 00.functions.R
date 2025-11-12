###set vectors of report areas #CH need to add 2 locality report areas too?
report_areas <- c("scotland","gp_prac_no","practice_hscp_code","practice_board_code" ,"practice_hscp_cluster")
report_areas_output <- c("Scotland","GP","HSCP","Health Board" ,"GPCL")
report_area_wt <- c("nat_wt","hb_wt","hscp_wt","gpcl_wt","gp_wt")

###set vectors of questions numbers
questions <- readRDS(paste0(lookup_path_202324,"questions.rds"))
information_questions <- readRDS(paste0(lookup_path,"information_questions.rds"))
information_questions_tata <- readRDS(paste0(lookup_path,"information_questions_tata.rds"))
percent_positive_questions <- readRDS(paste0(lookup_path,"percent_positive_questions.rds"))

crosstabs_f <- function(x,comp_var) {  # function to produce list of crosstabs
  tabyl(contractor_data, !!sym(comp_var), !!sym(x))
}

rowAny <- function(x) {rowSums(x,na.rm = TRUE) > 0} #checks whether any of list of variables is not NA or 0
rowAll <- function(x) {rowSums(x,na.rm = TRUE) == 0} #checks whether all of list of variables are NA or 0
subset_qs <- function(x,y) {questions[between(as.numeric(str_extract(questions,"\\d+")),x,y)]} #function to subset questions from questions vector



