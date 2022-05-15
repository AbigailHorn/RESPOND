# R script with functions for data cleaning

run_survey_cleaning <- function(dat, variables) {
  for(var in variables){
    dat <- switch(var,
           "methodology"= methodology_clean(dat),
           
           "a1not" = a1not_clean(dat),
           
           "a3_1" = replace_1_clean(dat, var, label = "Black_AfricanAmerican"),
           "a3_2" = replace_1_clean(dat, var, label = "Nigerian"),
           
           "b1aa" = no_yes_dont_know_clean(dat, var),
           "b1ab" = no_yes_dont_know_clean(dat, var),
           "b1ac" = no_yes_dont_know_clean(dat, var),
           "b1bno" = replace_1_clean(dat, var, label = "No_Brothers"),
           
           "b1ba" = no_yes_dont_know_clean(dat, var),
           "b1ba2" = number_one_2plus_clean(dat, var),
           "b1bb" = no_yes_dont_know_clean(dat, var),
           "b1bc" = no_yes_dont_know_clean(dat, var),
           
           "b1cno" = replace_1_clean(dat, var, label = "No_Sons"),
           "b1ca" = no_yes_dont_know_clean(dat, var),
           "b1ca2" = number_one_2plus_clean(dat, var),
           "b1cb" = no_yes_dont_know_clean(dat, var),
           "b1cc" = no_yes_dont_know_clean(dat, var),
           
           "b1da" = no_yes_dont_know_clean(dat, var),
           "b1db" = no_yes_dont_know_clean(dat, var),
           "b1dc" = no_yes_dont_know_clean(dat, var),
           
           "b1ea" = no_yes_dont_know_clean(dat, var),
           "b1eb" = no_yes_dont_know_clean(dat, var),
           "b1ec" = no_yes_dont_know_clean(dat, var),
           
           "b2" = no_yes_clean(dat, var),
           
           "b2a_1" = replace_1_clean(dat, var, label = "Breast"),
           "b2a_2" = replace_1_clean(dat, var, label = "Ovarian"),
           "b2a_3" = replace_1_clean(dat, var, label = "Colorectal"),
           "b2a_4" = replace_1_clean(dat, var, label = "Lung"),
           "b2a_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
           "b2a_6" = replace_1_clean(dat, var, label = "No_Other_Cancer"),
           "b2a_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_Cancer"),
           
           "b2b_1" = replace_1_clean(dat, var, label = "Breast"),
           "b2b_3" = replace_1_clean(dat, var, label = "Colorectal"),
           "b2b_4" = replace_1_clean(dat, var, label = "Lung"),
           "b2b_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
           "b2b_6" = replace_1_clean(dat, var, label = "No_Other_Cancer"),
           "b2b_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_Cancer"),
           
           "b2cno"= replace_1_clean(dat, var, label = "No_Sisters"),
           "b2c_1" = replace_1_clean(dat, var, label = "Breast"),
           "b2c_2" = replace_1_clean(dat, var, label = "Ovarian"),
           "b2c_3" = replace_1_clean(dat, var, label = "Colorectal"),
           "b2c_4" = replace_1_clean(dat, var, label = "Lung"),
           "b2c_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
           "b2c_6" = replace_1_clean(dat, var, label = "No_Other_Cancer"),
           "b2c_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_Cancer"),
           
           "b2dno" = replace_1_clean(dat, var, label = "No_Brothers"),
           "b2d_1" = replace_1_clean(dat, var, label = "Breast"),
           "b2d_3" = replace_1_clean(dat, var, label = "Colorectal"),
           "b2d_4" = replace_1_clean(dat, var, label = "Lung"),
           "b2d_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
           "b2d_6" = replace_1_clean(dat, var, label = "No_Other_Cancer"),
           "b2d_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_Cancer"),
           
           "b2eno"= replace_1_clean(dat, var, label = "No_Daughters"),
           "b2e_1" = replace_1_clean(dat, var, label = "Breast"),
           "b2e_2" = replace_1_clean(dat, var, label = "Ovarian"),
           "b2e_3" = replace_1_clean(dat, var, label = "Colorectal"),
           "b2e_4" = replace_1_clean(dat, var, label = "Lung"),
           "b2e_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
           "b2e_6" = replace_1_clean(dat, var, label = "No_Other_Cancer"),
           "b2e_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_Cancer"),
           
           "b2fno" = replace_1_clean(dat, var, label = "No_Sons"),
           "b2f_1" = replace_1_clean(dat, var, label = "Breast"),
           "b2f_3" = replace_1_clean(dat, var, label = "Colorectal"),
           "b2f_4" = replace_1_clean(dat, var, label = "Lung"),
           "b2f_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
           "b2f_6" = replace_1_clean(dat, var, label = "No_Other_Cancer"),
           "b2f_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_Cancer"),
           
           "b3" = current_health_clean(dat),
           
           "b4aa" = no_yes_clean(dat, var),
           "b4ab" = age_clean(dat, var),
           "b4ba" = no_yes_clean(dat, var),
           "b4bb" = age_clean(dat, var),
           "b4ca" = no_yes_clean(dat, var),
           "b4cb" = age_clean(dat, var),
           "b4da" = no_yes_clean(dat, var),
           "b4db" = age_clean(dat, var),
           "b4ea" = no_yes_clean(dat, var),
           "b4eb" = age_clean(dat, var),
           "b4fa" = no_yes_clean(dat, var),
           "b4fb" = age_clean(dat, var),
           "b4fc" = no_yes_clean(dat, var),
           "b4ga" = no_yes_clean(dat, var),
           "b4gb" = age_clean(dat, var),
           "b4ha" = no_yes_clean(dat, var),
           "b4hb" = age_clean(dat, var),
           "b4ia" = no_yes_clean(dat, var),
           "b4ib" = age_clean(dat, var),
           "b4ja" = no_yes_clean(dat, var),
           "b4jb" = age_clean(dat, var),
           "b4jc" = no_yes_clean(dat, var),
           "b4jd" = no_yes_clean(dat, var),
           "b4ka" = no_yes_clean(dat, var),
           "b4kb" = age_clean(dat, var),
           "b4la" = no_yes_clean(dat, var),
           "b4lb" = age_clean(dat, var),
           "b4ma" = no_yes_clean(dat, var),
           "b4mb" = age_clean(dat, var),
           "b4na" = no_yes_clean(dat, var),
           "b4nb" = age_clean(dat, var),
           "b4oa" = no_yes_clean(dat, var),
           "b4ob" = age_clean(dat, var),
           "b4pa" = no_yes_clean(dat, var),
           "b4pb" = age_clean(dat, var),
           "b4qa" = no_yes_clean(dat, var),
           "b4qb" = age_clean(dat, var),
           "b4qother" = not_clean(dat, var),
           
           "b5" = routine_medical_care_location_clean(dat),
           "b5other" = not_clean(dat, var),
           
           "c1" = lived_at_current_address_clean(dat),
           
           not_clean(dat, var)
           )
    }
  return(dat)
}

## functions that apply to multiple variables
not_clean <- function(dat, var) {
  print(paste0(var, " NOT changed"))
  return(dat)
}

no_yes_dont_know_clean <- function(dat, var) {
    dat[,var]  <- as.data.frame(dat[,var]) %>% transmute(v = case_when(dat[,var] == "1" ~ "No", 
                                             dat[,var] == "2" ~ "Yes", 
                                             dat[,var] == "88" ~ "Dont_know", 
                                    TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Yes", "No", "Dont_know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

no_yes_clean <- function(dat, var) {
  dat[,var]  <- as.data.frame(dat[,var]) %>% transmute(v = case_when(dat[,var] == "1" ~ "No", 
                                                                     dat[,var] == "2" ~ "Yes", 
                                                                     TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Yes", "No"))
  print(paste0(var, " cleaned"))
  return(dat)
}

number_one_2plus_clean <- function(dat, var) {
  dat[,var]  <- as.data.frame(dat[,var]) %>% transmute(v = case_when(dat[,var] == "1" ~ "1", 
                                                                     dat[,var] == "2" ~ "2plus", 
                                                                     TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "1", "2plus"))
  print(paste0(var, " cleaned"))
  return(dat)  
  
}

replace_1_clean <- function(dat, var, label = "") {
  dat[,var]  <- as.data.frame(dat[,var]) %>% transmute(v = case_when(dat[,var] == "1" ~ label,
                                                                     TRUE ~ NA_character_))
  print(paste0(var, " cleaned"))
  return(dat)
}

age_clean <- function(dat, var) {
  dat[,var] <- as.numeric(dat[,var])
  print(paste0(var, " cleaned"))
  return(dat)
} 


## Variable specific functions
methodology_clean <- function(dat) {
  dat <- dat %>% 
    mutate(methodology = case_when(methodology == "P" ~ "Paper",
                                   methodology == "O" ~ "Online", 
                                   TRUE ~ NA_character_)) %>%
    mutate(methodology = fct_relevel(methodology, "Paper", "Online"))
  print("methodology cleaned")
  return(dat)
}

a1not_clean <- function(dat) {
  dat <- dat %>% 
    mutate(a1not = case_when(a1not == "1" ~ "NEVER_ProstateCancer", 
                             a1not == "2" ~ "HAVE_ProstateCancer", 
                             TRUE ~ NA_character_)) %>%
    mutate(a1not = fct_relevel(a1not, "HAVE_ProstateCancer", "NEVER_ProstateCancer"))
  print("a1not cleaned")
  return(dat)
}

current_health_clean <- function(dat) {
  dat <- dat %>% 
    mutate(b3 = case_when(b3 == "1" ~ "Excellent",
                          b3 == "2" ~ "Very_Good",
                          b3 == "3" ~ "Good",
                          b3 == "4" ~ "Fair",
                          b3 == "5" ~ "Poor",
                          TRUE ~ NA_character_)) %>%
    mutate(b3 = fct_relevel(b3, "Excellent", "Very_Good", "Good", "Fair", "Poor"))
  print("b3 cleaned")
  return(dat)  
}

routine_medical_care_location_clean <- function(dat) {
  dat <- dat %>% 
    mutate(b5 = case_when(b5 == "1" ~ "Community_Health_Center",
                          b5 == "2" ~ "Hospital",
                          b5 == "3" ~ "Private_Doctor",
                          b5 == "4" ~ "Emergency_Room",
                          b5 == "5" ~ "Veteran_Affairs",
                          TRUE ~ NA_character_))
  print("b5 cleaned")
  return(dat) 
}

lived_at_current_address_clean <- function(dat) {
  dat <- dat %>% 
    mutate(c1 = case_when(c1 == "1" ~ "Less_than_1_year",
                          c1 == "2" ~ "1_to_5_years",
                          c1 == "3" ~ "6_to_10_years",
                          c1 == "4" ~ "11_to_15_years",
                          c1 == "5" ~ "16_to_20_years",
                          c1 == "6" ~ "21_plus_years",
                          TRUE ~ NA_character_))
  print("c1 cleaned")
  return(dat) 
}
