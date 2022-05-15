# Updated on 2022-4-14
# Progress: 353/353
# TBD:       21/353

# Loading library and data
#```{r Loading}
library(tidyverse)
library(dplyr)
# load("./_data/vsurvall20220328.Rda")
# load("./_data/vsiteid20220401.Rda")
# 
# # remove spaces for now
# vsiteid <- vsiteid %>% 
#   mutate(registrywid = gsub(" ", "_", registrywid))
# 
# # working data.frame, with some cleaning of IDs etc
# d <- vsurvall %>% 
#   left_join(vsiteid, 'siteid') %>% 
#   # trim whitespace from surveyid
#   mutate(surveyid = trimws(surveyid))
#```

# R script with functions for data cleaning
#```{r Main function}
run_survey_cleaning <- function(dat, variables) {
  for(var in variables){
    dat <- switch(var,
                  "methodology"= methodology_clean(dat, var),
                  
                  "a1month" = not_clean(dat, var),
                  "a1year" = not_clean(dat, var),
                  "a1not" = a1not_clean(dat, var),
                  
                  "a2" = no_yes_clean(dat, var),
                  
                  "a3_1" = replace_1_clean(dat, var, label = "Black_or_AfrAme"),
                  "a3_2" = replace_1_clean(dat, var, label = "Nigerian"),
                  "a3_3" = replace_1_clean(dat, var, label = "Jamaican"),
                  "a3_4" = replace_1_clean(dat, var, label = "Ethiopian"),
                  "a3_5" = replace_1_clean(dat, var, label = "Haitian"),
                  "a3_6" = replace_1_clean(dat, var, label = "Somali"),
                  "a3_7" = replace_1_clean(dat, var, label = "Guyanese"),
                  "a3_8" = replace_1_clean(dat, var, label = "Creole"),
                  "a3_9" = replace_1_clean(dat, var, label = "West_Indian"),
                  "a3_10" = replace_1_clean(dat, var, label = "Caribbean"),
                  "a3_11" = replace_1_clean(dat, var, label = "White"),
                  "a3_12" = replace_1_clean(dat, var, label = "Asian"),
                  "a3_13" = replace_1_clean(dat, var, label = "Native_American"),
                  "a3_14" = replace_1_clean(dat, var, label = "MdEast_or_NrAfr"),
                  "a3_15" = replace_1_clean(dat, var, label = "Hawai_or_PcIsla"),
                  "a3_16" = replace_1_clean(dat, var, label = "Hispanic"),
                  "a3_17" = replace_1_clean(dat, var, label = "Latino"),
                  "a3_18" = replace_1_clean(dat, var, label = "Spanish"),
                  "a3_19" = replace_1_clean(dat, var, label = "Mexican"),
                  "a3_20" = replace_1_clean(dat, var, label = "Salvadoran"),
                  "a3_21" = replace_1_clean(dat, var, label = "Puerto_Rican"),
                  "a3_22" = replace_1_clean(dat, var, label = "Dominican"),
                  "a3_23" = replace_1_clean(dat, var, label = "Columbian"),
                  "a3_24" = replace_1_clean(dat, var, label = "Other"),
                  "a3other" = not_clean(dat, var),
                  
                  "a4month" = not_clean(dat, var),
                  "a4year" = not_clean(dat, var),
                  
                  "a5" = birth_location_clean(dat, var),
                  "a5other" = not_clean(dat, var),
                  
                  "a6" = birth_location_clean(dat, var),
                  "a6other" = not_clean(dat, var),
                  
                  "a7" = birth_location_clean(dat, var),
                  "a7other" = not_clean(dat, var),
                  
                  "a8" = a8_clean(dat, var),
                  
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
                  "b2a_6" = replace_1_clean(dat, var, label = "No_OtherCancer"),
                  "b2a_7" = replace_1_clean(dat, var, label = "Unknown"),
                  
                  "b2b_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2b_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2b_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2b_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2b_6" = replace_1_clean(dat, var, label = "No_OtherCancer"),
                  "b2b_7" = replace_1_clean(dat, var, label = "Unknown"),
                  
                  "b2cno"= replace_1_clean(dat, var, label = "No_Sisters"),
                  "b2c_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2c_2" = replace_1_clean(dat, var, label = "Ovarian"),
                  "b2c_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2c_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2c_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2c_6" = replace_1_clean(dat, var, label = "No_OtherCancer"),
                  "b2c_7" = replace_1_clean(dat, var, label = "Unknown"),
                  
                  "b2dno" = replace_1_clean(dat, var, label = "No_Brothers"),
                  "b2d_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2d_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2d_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2d_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2d_6" = replace_1_clean(dat, var, label = "No_OtherCancer"),
                  "b2d_7" = replace_1_clean(dat, var, label = "Unknown"),
                  
                  "b2eno"= replace_1_clean(dat, var, label = "No_Daughters"),
                  "b2e_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2e_2" = replace_1_clean(dat, var, label = "Ovarian"),
                  "b2e_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2e_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2e_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2e_6" = replace_1_clean(dat, var, label = "No_OtherCancer"),
                  "b2e_7" = replace_1_clean(dat, var, label = "Unknown"),
                  
                  "b2fno" = replace_1_clean(dat, var, label = "No_Sons"),
                  "b2f_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2f_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2f_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2f_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2f_6" = replace_1_clean(dat, var, label = "No_OtherCancer"),
                  "b2f_7" = replace_1_clean(dat, var, label = "Unknown"),
                  
                  "b3" = current_health_clean(dat, var),
                  
                  "b4aa" = no_yes_clean(dat, var),
                  "b4ab" = numeric_clean(dat, var),
                  "b4ba" = no_yes_clean(dat, var),
                  "b4bb" = numeric_clean(dat, var),
                  "b4ca" = no_yes_clean(dat, var),
                  "b4cb" = numeric_clean(dat, var),
                  "b4da" = no_yes_clean(dat, var),
                  "b4db" = numeric_clean(dat, var),
                  "b4dc" = no_yes_clean(dat, var),
                  "b4ea" = no_yes_clean(dat, var),
                  "b4eb" = numeric_clean(dat, var),
                  "b4fa" = no_yes_clean(dat, var),
                  "b4fb" = numeric_clean(dat, var),
                  "b4fc" = no_yes_clean(dat, var),
                  "b4ga" = no_yes_clean(dat, var),
                  "b4gb" = numeric_clean(dat, var),
                  "b4ha" = no_yes_clean(dat, var),
                  "b4hb" = numeric_clean(dat, var),
                  "b4ia" = no_yes_clean(dat, var),
                  "b4ib" = numeric_clean(dat, var),
                  "b4ja" = no_yes_clean(dat, var),
                  "b4jb" = numeric_clean(dat, var),
                  "b4jc" = no_yes_clean(dat, var),
                  "b4jd" = no_yes_clean(dat, var),
                  "b4ka" = no_yes_clean(dat, var),
                  "b4kb" = numeric_clean(dat, var),
                  "b4la" = no_yes_clean(dat, var),
                  "b4lb" = numeric_clean(dat, var),
                  "b4ma" = no_yes_clean(dat, var),
                  "b4mb" = numeric_clean(dat, var),
                  "b4na" = no_yes_clean(dat, var),
                  "b4nb" = numeric_clean(dat, var),
                  "b4oa" = no_yes_clean(dat, var),
                  "b4ob" = numeric_clean(dat, var),
                  "b4pa" = no_yes_clean(dat, var),
                  "b4pb" = numeric_clean(dat, var),
                  "b4qa" = no_yes_clean(dat, var),
                  "b4qb" = numeric_clean(dat, var),
                  "b4qother" = not_clean(dat, var),
                  
                  "b5" = routine_medical_care_location_clean(dat, var),
                  "b5other" = not_clean(dat, var),
                  
                  "c1" = lived_at_current_address_clean(dat, var),
                  
                  "c2a1" = agree_disagree_degree_5_clean(dat, var),
                  "c2a2" = agree_disagree_degree_5_clean(dat, var),
                  "c2a3" = agree_disagree_degree_5_clean(dat, var),
                  
                  "c2b1" = agree_disagree_degree_5_clean(dat, var),
                  "c2b2" = agree_disagree_degree_5_clean(dat, var),
                  "c2b3" = agree_disagree_degree_5_clean(dat, var),
                  
                  "c2c1" = agree_disagree_degree_5_clean(dat, var),
                  "c2c2" = agree_disagree_degree_5_clean(dat, var),
                  "c2c3" = agree_disagree_degree_5_clean(dat, var),
                  
                  "c3a1" = problem_dont_know_clean(dat, var),
                  "c3a2" = problem_dont_know_clean(dat, var),
                  "c3a3" = problem_dont_know_clean(dat, var),
                  
                  "c3b1" = problem_dont_know_clean(dat, var),
                  "c3b2" = problem_dont_know_clean(dat, var),
                  "c3b3" = problem_dont_know_clean(dat, var),
                  
                  "c3c1" = problem_dont_know_clean(dat, var),
                  "c3c2" = problem_dont_know_clean(dat, var),
                  "c3c3" = problem_dont_know_clean(dat, var),
                  
                  "c3d1" = problem_dont_know_clean(dat, var),
                  "c3d2" = problem_dont_know_clean(dat, var),
                  "c3d3" = problem_dont_know_clean(dat, var),
                  
                  "c4a1" = how_often_degree_3_dk_clean(dat, var),
                  "c4a2" = how_often_degree_3_dk_clean(dat, var),
                  "c4a3" = how_often_degree_3_dk_clean(dat, var),
                  
                  "c4b1" = how_often_degree_3_dk_clean(dat, var),
                  "c4b2" = how_often_degree_3_dk_clean(dat, var),
                  "c4b3" = how_often_degree_3_dk_clean(dat, var),
                  
                  "c4c1" = how_many_clean(dat, var),
                  "c4c2" = how_many_clean(dat, var),
                  "c4c3" = how_many_clean(dat, var),
                  
                  "c4d1" = how_many_clean(dat, var),
                  "c4d2" = how_many_clean(dat, var),
                  "c4d3" = how_many_clean(dat, var),
                  
                  "c4e1" = how_many_clean(dat, var),
                  "c4e2" = how_many_clean(dat, var),
                  "c4e3" = how_many_clean(dat, var),
                  
                  "d1aa" = no_yes_clean(dat, var),
                  "d1ab" = how_degree_4_clean(dat, var),
                  "d1ba" = no_yes_clean(dat, var),
                  "d1bb" = how_degree_4_clean(dat, var),
                  "d1ca" = no_yes_clean(dat, var),
                  "d1cb" = how_degree_4_clean(dat, var),
                  "d1da" = no_yes_clean(dat, var),
                  "d1db" = how_degree_4_clean(dat, var),
                  "d1ea" = no_yes_clean(dat, var),
                  "d1eb" = how_degree_4_clean(dat, var),
                  "d1fa" = no_yes_clean(dat, var),
                  "d1fb" = how_degree_4_clean(dat, var),
                  "d1ga" = no_yes_clean(dat, var),
                  "d1gb" = how_degree_4_clean(dat, var),
                  
                  "d2a" = agree_disagree_degree_4_clean(dat, var),
                  "d2b" = agree_disagree_degree_4_clean(dat, var),
                  "d2c" = agree_disagree_degree_4_clean(dat, var),
                  "d2d" = agree_disagree_degree_4_clean(dat, var),
                  "d2e" = agree_disagree_degree_4_clean(dat, var),
                  
                  "d3a1" = how_often_degree_4_clean(dat, var),
                  "d3a2" = how_often_degree_4_clean(dat, var),
                  "d3a3" = how_often_degree_4_clean(dat, var),
                  
                  "d3b1" = how_often_degree_4_clean(dat, var),
                  "d3b2" = how_often_degree_4_clean(dat, var),
                  "d3b3" = how_often_degree_4_clean(dat, var),
                  
                  "d3c1" = how_often_degree_4_clean(dat, var),
                  "d3c2" = how_often_degree_4_clean(dat, var),
                  "d3c3" = how_often_degree_4_clean(dat, var),
                  
                  "d3d1" = how_often_degree_4_clean(dat, var),
                  "d3d2" = how_often_degree_4_clean(dat, var),
                  "d3d3" = how_often_degree_4_clean(dat, var),
                  
                  "d3e1" = how_often_degree_4_clean(dat, var),
                  "d3e2" = how_often_degree_4_clean(dat, var),
                  "d3e3" = how_often_degree_4_clean(dat, var),
                  
                  "d3f1" = how_often_degree_4_clean(dat, var),
                  "d3f2" = how_often_degree_4_clean(dat, var),
                  "d3f3" = how_often_degree_4_clean(dat, var),
                  
                  "d3g1" = how_often_degree_4_clean(dat, var),
                  "d3g2" = how_often_degree_4_clean(dat, var),
                  "d3g3" = how_often_degree_4_clean(dat, var),
                  
                  "d3h1" = how_often_degree_4_clean(dat, var),
                  "d3h2" = how_often_degree_4_clean(dat, var),
                  "d3h3" = how_often_degree_4_clean(dat, var),
                  
                  "d3i1" = how_often_degree_4_clean(dat, var),
                  "d3i2" = how_often_degree_4_clean(dat, var),
                  "d3i3" = how_often_degree_4_clean(dat, var),
                  
                  "d3j1" = how_degree_4_clean(dat, var),
                  "d3j2" = how_degree_4_clean(dat, var),
                  "d3j3" = how_degree_4_clean(dat, var),
                  
                  "d4a" = agree_disagree_degree_4_clean(dat, var),
                  "d4b" = agree_disagree_degree_4_clean(dat, var),
                  "d4c" = agree_disagree_degree_4_clean(dat, var),
                  "d4d" = agree_disagree_degree_4_clean(dat, var),
                  "d4e" = agree_disagree_degree_4_clean(dat, var),
                  "d4f" = agree_disagree_degree_4_clean(dat, var),
                  "d4g" = agree_disagree_degree_4_clean(dat, var),
                  "d4h" = agree_disagree_degree_4_clean(dat, var),
                  "d4i" = agree_disagree_degree_4_clean(dat, var),
                  "d4j" = agree_disagree_degree_4_clean(dat, var),
                  "d4k" = agree_disagree_degree_4_clean(dat, var),
                  "d4l" = agree_disagree_degree_4_clean(dat, var),
                  
                  "d5a" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5b" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5c" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5d" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5e" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5f" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5g" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5h" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5i" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5j" = no_yes_dont_know_prefer_not_clean(dat, var),
                  "d5k" = no_yes_dont_know_prefer_not_clean(dat, var),
                  
                  "e1_1" = replace_1_clean(dat, var, label = "High_PSA_Test"),
                  "e1_2" = replace_1_clean(dat, var, label = "Dgt_Rectal_Exam"),
                  "e1_3" = replace_1_clean(dat, var, label = "Ur_Se_or_Bo_Prb"),
                  "e1_4" = replace_1_clean(dat, var, label = "Bone_Pain"),
                  "e1_5" = replace_1_clean(dat, var, label = "Was_Fearful"),
                  "e1_6" = replace_1_clean(dat, var, label = "Other"),
                  "e1other" = not_clean(dat, var),
                  
                  "e2aa" = no_yes_dont_know_clean(dat, var),
                  "e2ab" = number_one_3plus_clean(dat, var),
                  
                  "e2ba" = no_yes_dont_know_clean(dat, var),
                  "e2bb" = number_one_5plus_clean(dat, var),
                  
                  "e3" = decision_clean(dat, var),
                  
                  "e4" = risk_of_progression_clean(dat, var),
                  
                  "e5" = gleason_score_clean(dat, var),
                  
                  "e6" = stage_clean(dat, var),
                  
                  "e7" = no_yes_clean(dat, var),
                  
                  "e8" = decision_clean_6(dat, var),
                  
                  "e9_1" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_2" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_3" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_4" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_5" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_6" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_7" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_8" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_9" = replace_1_clean(dat, var, label = "Yes"),
                  "e9_10" = replace_1_clean(dat, var, label = "Yes"),
                  
                  "e10_1" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_2" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_3" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_4" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_5" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_6" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_7" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_8" = replace_1_clean(dat, var, label = "Yes"),
                  
                  "e10_3_1" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_3_2" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_3_3" = replace_1_clean(dat, var, label = "Yes"),
                  
                  "e10_4_1" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_4_2" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_4_3" = replace_1_clean(dat, var, label = "Yes"),
                  
                  "e10_5_1" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_5_2" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_5_3" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_5_4" = replace_1_clean(dat, var, label = "Yes"),
                  "e10_5_5" = replace_1_clean(dat, var, label = "Yes"),
                  
                  "e11a" = how_true_degree_5_clean(dat, var),
                  "e11b" = how_true_degree_5_clean(dat, var),
                  "e11c" = how_true_degree_5_clean(dat, var),
                  "e11d" = how_true_degree_5_clean(dat, var),
                  "e11e" = how_true_degree_5_clean(dat, var),
                  "e11f" = how_true_degree_5_clean(dat, var),
                  
                  "e12" = no_yes_dont_know_not_sure_clean(dat, var),
                  
                  "e13" = number_one_4plus_clean(dat, var),
                  
                  "e14" = no_yes_dont_know_not_sure_clean(dat, var),
                  
                  "e15" = no_yes_dont_know_not_sure_clean(dat, var),
                  
                  "f1ft" = numeric_clean(dat, var),
                  "f1in" = not_clean(dat, var),
                  "f1cm" = not_clean(dat, var),
                  
                  "f2lbs" = not_clean(dat, var),
                  "f2kgs" = not_clean(dat, var),
                  
                  "f3" = how_often_per_week_degree_4_clean(dat, var),
                  
                  "f4" = how_much_time_degree_4_clean(dat, var),
                  
                  "f5" = how_often_per_week_degree_6_clean(dat, var),
                  
                  "f6" = how_many_drinks_degree_3_clean(dat, var),
                  
                  "f7" = no_yes_clean(dat, var),
                  "f7age" = not_clean(dat, var),
                  "f7a" = cigarettes_per_day_clean(dat, var),
                  "f7b" = no_yes_clean(dat, var),
                  "f7bage" = not_clean(dat, var),
                  
                  "g1" = marital_status_clean(dat, var),
                  
                  "g2_1" = replace_1_clean(dat, var, label = "Live_Alone"),
                  "g2_2" = replace_1_clean(dat, var, label = "Spous_or_Partnr"),
                  "g2_3" = replace_1_clean(dat, var, label = "Other_Family"),
                  "g2_4" = replace_1_clean(dat, var, label = "Other_NonFamily"),
                  "g2_5" = replace_1_clean(dat, var, label = "Pets"),
                  
                  "g3" = sexual_identity_clean(dat, var),
                  "g3other" = not_clean(dat, var),
                  
                  "g4a" = education_clean(dat, var),
                  "g4b" = education_dk_clean(dat, var),
                  "g4c" = education_dk_clean(dat, var),
                  
                  "g5" = current_activity_clean(dat, var),
                  "g5other" = not_clean(dat, var),
                  
                  "g6_1" = replace_1_clean(dat, var, label = "Employ_or_Union"),
                  "g6_2" = replace_1_clean(dat, var, label = "Family_Member"),
                  "g6_3" = replace_1_clean(dat, var, label = "Insurance_Comp"),
                  "g6_4" = replace_1_clean(dat, var, label = "Exchange"),
                  "g6_5" = replace_1_clean(dat, var, label = "Medicaid"),
                  "g6_6" = replace_1_clean(dat, var, label = "Medicare"),
                  "g6_7" = replace_1_clean(dat, var, label = "VA"),
                  "g6_8" = replace_1_clean(dat, var, label = "Do_Not_Have_Any"),
                  
                  "g7" = family_income_clean(dat, var),
                  
                  "g8" = number_one_5plus_clean(dat, var),
                  
                  "g9a" = how_worried_degree_4_clean(dat, var),
                  "g9b" = how_worried_degree_4_clean(dat, var),
                  "g9c" = how_worried_degree_4_clean(dat, var),
                  
                  "g10" = home_ownership_clean(dat, var),
                  "g10other"= not_clean(dat, var),
                  
                  "g11" = household_security_clean(dat, var),
                  
                  "g12" = not_clean(dat, var),
                  
                  not_clean(dat, var)
    )
  }
  return(dat)
}
#```

# Operational functions, all converted to versatile
#```{r Versatile functions, alphabetical order}
not_clean <- function(dat, var) {
  print(paste0(var, " NOT changed"))
  return(dat)
}

a1not_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "NEVER_PCa",
                            dat[, var] == "2" ~ "HAVE_or_HAD_PCa", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "HAVE_or_HAD_PCa", "NEVER_PCa"))
  print(paste0(var, " cleaned"))
  return(dat)
}

a8_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "15_Yrs_or_Less",
                            dat[, var] == "2" ~ "16_to_25_Yrs", 
                            dat[, var] == "3" ~ "26_Yrs_or_More", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "15_Yrs_or_Less", "16_to_25_Yrs", "26_Yrs_or_More"))
  print(paste0(var, " cleaned"))
  return(dat)
}

agree_disagree_degree_4_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Strongly_Agree", 
                            dat[, var] == "2" ~ "Somewhat_Agree", 
                            dat[, var] == "3" ~ "Somewhat_Disagr", 
                            dat[, var] == "4" ~ "Strongly_Disagr", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Strongly_Agree", "Somewhat_Agree", "Somewhat_Disagr", "Strongly_Disagr"))
  print(paste0(var, " cleaned"))
  return(dat)
}

agree_disagree_degree_5_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Strongly_Agree", 
                            dat[, var] == "2" ~ "Agree", 
                            dat[, var] == "3" ~ "Neutral", 
                            dat[, var] == "4" ~ "Disagree", 
                            dat[, var] == "5" ~ "Strongly_Disagr", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Strongly_Agree", "Agree", "Neutral", "Disagree", "Strongly_Disagr"))
  print(paste0(var, " cleaned"))
  return(dat)
}

birth_location_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "United_States", 
                            dat[, var] == "2" ~ "Africa", 
                            dat[, var] == "3" ~ "Cuba_or_CarIsla", 
                            dat[, var] == "4" ~ "Other", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "United_States", "Africa", "Cuba_or_CarIsla", "Other"))
  print(paste0(var, " cleaned"))
  return(dat)
}

cigarettes_per_day_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "1_to_5", 
                            dat[, var] == "2" ~ "6_to_10", 
                            dat[, var] == "3" ~ "11_to_20", 
                            dat[, var] == "4" ~ "21_to_30", 
                            dat[, var] == "5" ~ "31plus", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "1_to_5", "6_to_10", "11_to_20", "21_to_30", "31plus"))
  print(paste0(var, " cleaned"))
  return(dat)
}

current_activity_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Full_Time_Job", 
                            dat[, var] == "2" ~ "Part_Time_Job", 
                            dat[, var] == "3" ~ "Unemployed", 
                            dat[, var] == "4" ~ "Retired", 
                            dat[, var] == "5" ~ "Perm_Disability", 
                            dat[, var] == "6" ~ "Temp_Leave", 
                            dat[, var] == "7" ~ "Work_WithoutPay", 
                            dat[, var] == "8" ~ "Other",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Full_Time_Job", "Part_Time_Job", "Unemployed", "Retired", "Perm_Disability", "Temp_Leave", "Work_WithoutPay", "Other"))
  print(paste0(var, " cleaned"))
  return(dat)
}

current_health_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Excellent",
                            dat[, var] == "2" ~ "Very_Good",
                            dat[, var] == "3" ~ "Good",
                            dat[, var] == "4" ~ "Fair",
                            dat[, var] == "5" ~ "Poor",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Excellent", "Very_Good", "Good", "Fair", "Poor"))
  print(paste0(var, " cleaned"))
  return(dat)  
}

decision_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Alone", 
                            dat[, var] == "2" ~ "With_A_FF", 
                            dat[, var] == "3" ~ "With_A_FF_HCP", 
                            dat[, var] == "4" ~ "With_HCP",
                            dat[, var] == "5" ~ "HCPs_Decision", 
                            dat[, var] == "88" ~ "DontKnow_Forgot", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Alone", "With_A_FF", "With_A_FF_HCP",  "With_HCP", "HCPs_Decision", "DontKnow_Forgot"))
  print(paste0(var, " cleaned"))
  return(dat)
}

decision_clean_6 <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Alone", 
                            dat[, var] == "2" ~ "With_A_FF", 
                            dat[, var] == "3" ~ "With_A_FF_HCP", 
                            dat[, var] == "4" ~ "With_HCP",
                            dat[, var] == "5" ~ "HCPs_Decision", 
                            dat[, var] == "6" ~ "DontKnow_Forgot", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Alone", "With_A_FF", "With_A_FF_HCP",  "With_HCP", "HCPs_Decision", "DontKnow_Forgot"))
  print(paste0(var, " cleaned"))
  return(dat)
}

education_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "GradeSc_or_Less", 
                            dat[, var] == "2" ~ "Some_HighSc", 
                            dat[, var] == "3" ~ "HighSc_Grad", 
                            dat[, var] == "4" ~ "VocationalSc", 
                            dat[, var] == "5" ~ "Some_College", 
                            dat[, var] == "6" ~ "Associates_Deg", 
                            dat[, var] == "7" ~ "Bachelors_Deg", 
                            dat[, var] == "8" ~ "Some_GradEdu", 
                            dat[, var] == "9" ~ "Graduate_Deg", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "GradeSc_or_Less", "Some_HighSc", "HighSc_Grad", "VocationalSc", "Some_College", "Associates_Deg", "Bachelors_Deg", "Some_GradEdu", "Graduate_Deg"))
  print(paste0(var, " cleaned"))
  return(dat)
}

education_dk_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "GradeSc_or_Less", 
                            dat[, var] == "2" ~ "Some_HighSc", 
                            dat[, var] == "3" ~ "HighSc_Grad", 
                            dat[, var] == "4" ~ "VocationalSc", 
                            dat[, var] == "5" ~ "Some_College", 
                            dat[, var] == "6" ~ "Associates_Deg", 
                            dat[, var] == "7" ~ "Bachelors_Deg", 
                            dat[, var] == "8" ~ "Some_GradEdu", 
                            dat[, var] == "9" ~ "Graduate_Deg", 
                            dat[, var] == "88" ~ "Dont_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "GradeSc_or_Less", "Some_HighSc", "HighSc_Grad", "VocationalSc", "Some_College", "Associates_Deg", "Bachelors_Deg", "Some_GradEdu", "Graduate_Deg", "Dont_Know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

family_income_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Less_Than_15K", 
                            dat[, var] == "2" ~ "15K_to_35999", 
                            dat[, var] == "3" ~ "36K_to_45999", 
                            dat[, var] == "4" ~ "46K_to_65999", 
                            dat[, var] == "5" ~ "66K_to_99999", 
                            dat[, var] == "6" ~ "100K_to_149999", 
                            dat[, var] == "7" ~ "150K_to_199999", 
                            dat[, var] == "8" ~ "200K_or_More",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Less_Than_15K", "15K_to_35999", "36K_to_45999", "46K_to_65999", "66K_to_99999", "100K_to_149999", "150K_to_199999", "200K_or_More"))
  print(paste0(var, " cleaned"))
  return(dat)
}

gleason_score_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "6_or_Less",
                            dat[, var] == "2" ~ "7",
                            dat[, var] == "3" ~ "8_to_10",
                            dat[, var] == "88" ~ "Dont_Know",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "6_or_Less", "7", "8_to_10", "Dont_Know"))
  print(paste0(var, " cleaned"))
  return(dat) 
}

home_ownership_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Owned", 
                            dat[, var] == "2" ~ "Rented_forMoney",
                            dat[, var] == "3" ~ "Other", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Owned", "Rented_forMoney", "Other"))
  print(paste0(var, " cleaned"))
  return(dat)
}

household_security_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Less_Than_1_Mo", 
                            dat[, var] == "2" ~ "1_to_2_Months", 
                            dat[, var] == "3" ~ "3_to_6_Months", 
                            dat[, var] == "4" ~ "More_Than_6_Mos",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Less_Than_1_Mo", "1_to_2_Months", "3_to_6_Months", "More_Than_6_Mos"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_degree_4_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Not_at_All", 
                            dat[, var] == "2" ~ "A_Little", 
                            dat[, var] == "3" ~ "Somewhat", 
                            dat[, var] == "4" ~ "Extremely", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Not_at_All", "A_Little", "Somewhat", "Extremely"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_many_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "A_Lot", 
                            dat[, var] == "2" ~ "Some", 
                            dat[, var] == "3" ~ "Few_or_None", 
                            dat[, var] == "88" ~ "Dont_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "A_Lot", "Some", "Few_or_None", "Dont_Know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_many_drinks_degree_3_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Did_Not_Drink", 
                            dat[, var] == "2" ~ "1_to_2", 
                            dat[, var] == "3" ~ "3_or_More", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Did_Not_Drink", "1_to_2", "3_or_More"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_much_time_degree_4_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Do_Not_Exercise", 
                            dat[, var] == "2" ~ "Less_Than_30", 
                            dat[, var] == "3" ~ "30_to_60", 
                            dat[, var] == "4" ~ "More_Than_60", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Do_Not_Exercise", "Less_Than_30", "30_to_60", "More_Than_60"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_often_degree_3_dk_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Often", 
                            dat[, var] == "2" ~ "Sometimes", 
                            dat[, var] == "3" ~ "Rarely_or_Never", 
                            dat[, var] == "88" ~ "Dont_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Often", "Sometimes", "Rarely_or_Never", "Dont_Know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_often_degree_4_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Never", 
                            dat[, var] == "2" ~ "Rarely", 
                            dat[, var] == "3" ~ "Sometimes", 
                            dat[, var] == "4" ~ "Often", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Never", "Rarely", "Sometimes", "Often"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_often_per_week_degree_4_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Do_Not_Exercise", 
                            dat[, var] == "2" ~ "1_to_2",
                            dat[, var] == "3" ~ "3_to_4",
                            dat[, var] == "4" ~ "5_to_7",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Do_Not_Exercise", "1_to_2", "3_to_4", "5_to_7"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_often_per_week_degree_6_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Did_Not_Drink", 
                            dat[, var] == "2" ~ "Less_Than_1_TPW", 
                            dat[, var] == "3" ~ "1_to_2_TPW",
                            dat[, var] == "4" ~ "3_to_4_TPW",
                            dat[, var] == "5" ~ "5_to_6_TPW",
                            dat[, var] == "6" ~ "Everyday", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Did_Not_Drink", "Less_Than_1_TPW", "1_to_2_TPW", "3_to_4_TPW", "5_to_6_TPW", "Everyday"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_true_degree_5_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Not_at_All", 
                            dat[, var] == "2" ~ "A_Little_Bit", 
                            dat[, var] == "3" ~ "Somewhat", 
                            dat[, var] == "4" ~ "Quite_A_Bit", 
                            dat[, var] == "5" ~ "Very_Much", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Not_at_All", "A_Little_Bit", "Somewhat", "Quite_A_Bit", "Very_Much"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_worried_degree_4_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Not_at_All", 
                            dat[, var] == "2" ~ "A_Little", 
                            dat[, var] == "3" ~ "Somewhat", 
                            dat[, var] == "4" ~ "Very", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Not_at_All", "A_Little", "Somewhat", "Very"))
  print(paste0(var, " cleaned"))
  return(dat)
}

lived_at_current_address_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Less_Than_1_Yr",
                            dat[, var] == "2" ~ "1_to_5_Yrs",
                            dat[, var] == "3" ~ "6_to_10_Yrs",
                            dat[, var] == "4" ~ "11_to_15_Yrs",
                            dat[, var] == "5" ~ "16_to_20_Yrs",
                            dat[, var] == "6" ~ "21plus_Yrs",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Less_Than_1_Yr", "1_to_5_Yrs", "6_to_10_Yrs", "11_to_15_Yrs", "16_to_20_Yrs", "21plus_Yrs"))
  print(paste0(var, " cleaned"))
  return(dat) 
}

marital_status_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Married", 
                            dat[, var] == "2" ~ "Separated", 
                            dat[, var] == "3" ~ "Divorced", 
                            dat[, var] == "4" ~ "Widowed", 
                            dat[, var] == "5" ~ "Never_Married", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Married", "Separated", "Divorced", "Widowed", "Never_Married"))
  print(paste0(var, " cleaned"))
  return(dat)
}

methodology_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "P" ~ "Paper",
                            dat[, var] == "O" ~ "Online", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Paper", "Online"))
  print(paste0(var, " cleaned"))
  return(dat)
}

no_yes_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "No", 
                            dat[, var] == "2" ~ "Yes", 
                            TRUE ~ NA_character_)) %>% 
    transmute(v = fct_relevel(v, "Yes", "No"))
  print(paste0(var, " cleaned"))
  return(dat)
}

no_yes_dont_know_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "No", 
                            dat[, var] == "2" ~ "Yes", 
                            dat[, var] == "88" ~ "Dont_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Yes", "No", "Dont_Know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

no_yes_dont_know_not_sure_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "No", 
                            dat[, var] == "2" ~ "Yes", 
                            dat[, var] == "88" ~ "DontKnow_Unsure", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Yes", "No", "DontKnow_Unsure"))
  print(paste0(var, " cleaned"))
  return(dat)
}

no_yes_dont_know_prefer_not_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "No", 
                            dat[, var] == "2" ~ "Yes", 
                            dat[, var] == "88" ~ "DontKnow_Unsure", 
                            dat[, var] == "99" ~ "PrefNotToAnswer",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Yes", "No", "DontKnow_Unsure", "PrefNotToAnswer"))
  print(paste0(var, " cleaned"))
  return(dat)
}

number_one_2plus_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "1", 
                            dat[, var] == "2" ~ "2plus", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "1", "2plus"))
  print(paste0(var, " cleaned"))
  return(dat)
}

number_one_3plus_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "1", 
                            dat[, var] == "2" ~ "2",
                            dat[, var] == "3" ~ "3plus", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "1", "2", "3plus"))
  print(paste0(var, " cleaned"))
  return(dat)
}

number_one_4plus_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "1", 
                            dat[, var] == "2" ~ "2", 
                            dat[, var] == "3" ~ "3", 
                            dat[, var] == "4" ~ "4plus",
                            dat[, var] == "88" ~ "DontKnow_Unsure", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "1", "2", "3", "4plus", "DontKnow_Unsure"))
  print(paste0(var, " cleaned"))
  return(dat)
}

number_one_5plus_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "1", 
                            dat[, var] == "2" ~ "2", 
                            dat[, var] == "3" ~ "3", 
                            dat[, var] == "4" ~ "4",
                            dat[, var] == "5" ~ "5plus", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "1", "2", "3", "4", "5plus"))
  print(paste0(var, " cleaned"))
  return(dat)
}

numeric_clean <- function(dat, var) {
  dat[, var] <- as.numeric(dat[, var])
  print(paste0(var, " cleaned"))
  return(dat)
}

problem_dont_know_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Non_or_Minor", 
                            dat[, var] == "2" ~ "SomewhatSerious", 
                            dat[, var] == "3" ~ "Very_Serious", 
                            dat[, var] == "88" ~ "Dont_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Non_or_Minor", "SomewhatSerious", "Very_Serious", "Dont_Know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

replace_1_clean <- function(dat, var, label = "") {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ label,
                            TRUE ~ NA_character_))
  print(paste0(var, " cleaned"))
  return(dat)
}

risk_of_progression_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Low_Risk",
                            dat[, var] == "2" ~ "Intermedia_Risk",
                            dat[, var] == "3" ~ "High_Risk",
                            dat[, var] == "4" ~ "Unknown_Risk",
                            dat[, var] == "88" ~ "DontKnow_Forgot",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Low_Risk", "Intermedia_Risk", "High_Risk", "Unknown_Risk", "DontKnow_Forgot"))
  print(paste0(var, " cleaned"))
  return(dat) 
}

routine_medical_care_location_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Community_HC",
                            dat[, var] == "2" ~ "Hospital",
                            dat[, var] == "3" ~ "Private_Doctor",
                            dat[, var] == "4" ~ "Emergency_Room",
                            dat[, var] == "5" ~ "VA",
                            dat[, var] == "6" ~ "Other",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Community_HC", "Hospital", "Private_Doctor", "Emergency_Room", "VA", "Other"))
  print(paste0(var, " cleaned"))
  return(dat) 
}

sexual_identity_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Heterosexual",
                            dat[, var] == "2" ~ "Bisexual", 
                            dat[, var] == "3" ~ "Homosexual", 
                            dat[, var] == "4" ~ "Other", 
                            dat[, var] == "99" ~ "PrefNotToAnswer",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Heterosexual", "Bisexual", "Homosexual", "Other", "PrefNotToAnswer"))
  print(paste0(var, " cleaned"))
  return(dat)
}

stage_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Localized",
                            dat[, var] == "2" ~ "Regional",
                            dat[, var] == "3" ~ "Distant",
                            dat[, var] == "88" ~ "Dont_Know",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Localized", "Regional", "Distant", "Dont_Know"))
  print(paste0(var, " cleaned"))
  return(dat) 
}
#```