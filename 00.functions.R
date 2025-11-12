###set vectors of report areas #CH need to add 2 locality report areas too?
report_areas <- c("scotland","gp_prac_no","practice_hscp_code","practice_board_code" ,"practice_hscp_cluster")
report_areas_output <- c("Scotland","GP","HSCP","Health Board" ,"GPCL")
report_area_wt <- c("nat_wt","hb_wt","hscp_wt","gpcl_wt","gp_wt")

###set vectors of questions numbers
questions <- readRDS(paste0(lookup_path,"questions.rds"))
information_questions <- readRDS(paste0(lookup_path,"information_questions.rds"))
information_questions_tata <- readRDS(paste0(lookup_path,"information_questions_tata.rds"))
percent_positive_questions <- readRDS(paste0(lookup_path,"percent_positive_questions.rds"))





