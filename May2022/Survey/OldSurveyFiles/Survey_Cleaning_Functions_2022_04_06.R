# Updated on 2022-4-6
# Progress: 353/353
# TBD:       19/353

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
                  
                  "a3_1" = replace_1_clean(dat, var, label = "Black/African_American"),
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
                  "a3_12" = replace_1_clean(dat, var, label = "Asian/Asian_American"),
                  "a3_13" = replace_1_clean(dat, var, label = "Native_American_or_American_Indian_or_Alaskan_Native"),
                  "a3_14" = replace_1_clean(dat, var, label = "Middle_Eastern_or_North_African"),
                  "a3_15" = replace_1_clean(dat, var, label = "Native_Hawaiian_or_Pacific_Islander"),
                  "a3_16" = replace_1_clean(dat, var, label = "Hispanic"),
                  "a3_17" = replace_1_clean(dat, var, label = "Latino"),
                  "a3_18" = replace_1_clean(dat, var, label = "Spanish"),
                  "a3_19" = replace_1_clean(dat, var, label = "Mexican/Mexican_American"),
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
                  "b2a_6" = replace_1_clean(dat, var, label = "No_(Other)_Cancer"),
                  "b2a_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_(Other)_Cancer"),
                  
                  "b2b_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2b_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2b_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2b_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2b_6" = replace_1_clean(dat, var, label = "No_(Other)_Cancer"),
                  "b2b_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_(Other)_Cancer"),
                  
                  "b2cno"= replace_1_clean(dat, var, label = "No_Sisters"),
                  "b2c_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2c_2" = replace_1_clean(dat, var, label = "Ovarian"),
                  "b2c_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2c_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2c_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2c_6" = replace_1_clean(dat, var, label = "No_(Other)_Cancer"),
                  "b2c_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_(Other)_Cancer"),
                  
                  "b2dno" = replace_1_clean(dat, var, label = "No_Brothers"),
                  "b2d_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2d_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2d_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2d_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2d_6" = replace_1_clean(dat, var, label = "No_(Other)_Cancer"),
                  "b2d_7" = replace_1_clean(dat, var, label = "Unknown_If_Had_(Other)_Cancer"),
                  
                  "b2eno"= replace_1_clean(dat, var, label = "No_Daughters"),
                  "b2e_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2e_2" = replace_1_clean(dat, var, label = "Ovarian"),
                  "b2e_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2e_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2e_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2e_6" = replace_1_clean(dat, var, label = "No_(Other)_Cancer"),
                  "b2e_7" = replace_1_clean(dat, var, label = "Unknown_If_Had__(Other)_Cancer"),
                  
                  "b2fno" = replace_1_clean(dat, var, label = "No_Sons"),
                  "b2f_1" = replace_1_clean(dat, var, label = "Breast"),
                  "b2f_3" = replace_1_clean(dat, var, label = "Colorectal"),
                  "b2f_4" = replace_1_clean(dat, var, label = "Lung"),
                  "b2f_5" = replace_1_clean(dat, var, label = "Other_Cancer"),
                  "b2f_6" = replace_1_clean(dat, var, label = "No_(Other)_Cancer"),
                  "b2f_7" = replace_1_clean(dat, var, label = "Unknown_If_Had__(Other)_Cancer"),
                  
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
                  
                  "e1_1" = replace_1_clean(dat, var, label = "Had_A_High_PSA_(‘Prostate_Specific_Antigen’)_Test"),
                  "e1_2" = replace_1_clean(dat, var, label = "Did_A_Digital_Rectal_Exam_That_Indicated_An_Abnormality"),
                  "e1_3" = replace_1_clean(dat, var, label = "Had_Urinary_Sexual_or_Bowel_Problem"),
                  "e1_4" = replace_1_clean(dat, var, label = "Had_Bone_Pain"),
                  "e1_5" = replace_1_clean(dat, var, label = "Was_Fearful_of_Having_Cancer"),
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
                  "g2_2" = replace_1_clean(dat, var, label = "A_Spouse_or_Partner"),
                  "g2_3" = replace_1_clean(dat, var, label = "Other_Family"),
                  "g2_4" = replace_1_clean(dat, var, label = "Other_People_(Non_Family)"),
                  "g2_5" = replace_1_clean(dat, var, label = "Pets"),
                  
                  "g3" = sexual_identity_clean(dat, var),
                  "g3other" = not_clean(dat, var),
                  
                  "g4a" = education_clean(dat, var),
                  "g4b" = education_clean(dat, var),
                  "g4c" = education_clean(dat, var),
                  
                  "g5" = current_activity_clean(dat, var),
                  "g5other" = not_clean(dat, var),
                  
                  "g6_1" = replace_1_clean(dat, var, label = "Insurance_Provided_Through_Current_or_Former_Employer_or_Union_(Including_Kaiser/HMO/PPO)"),
                  "g6_2" = replace_1_clean(dat, var, label = "Insurance_Provided_by_Another_Family_Member_(e.g._Spouse)_Through_Their_Current_or_Former_Employer_or_Union_(Including_Kaiser/HMO/PPO)"),
                  "g6_3" = replace_1_clean(dat, var, label = "Insurance_Purchased_Directly_From_An_Insurance_Company_(By_Self_or_Another_Family_Member"),
                  "g6_4" = replace_1_clean(dat, var, label = "Insurance_Purchased_From_An_Exchange_(Sometimes_Called_Obamacare_or_the_Affordable_Care_Act"),
                  "g6_5" = replace_1_clean(dat, var, label = "Medicaid_or_Other_State_Provided_Insurance"),
                  "g6_6" = replace_1_clean(dat, var, label = "Medicare/Government_Insurance"),
                  "g6_7" = replace_1_clean(dat, var, label = "VA/Military_Facility_(Including_Those_Who_Have_Ever_Used_or_Enrolled_for_VA_Health_Care)"),
                  "g6_8" = replace_1_clean(dat, var, label = "Do_Not_Have_Any_Medical_Insurance"),
                  
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

# functions that apply to multiple variables
#```{r Versatile functions}
not_clean <- function(dat, var) {
  print(paste0(var, " NOT changed"))
  return(dat)
}

no_yes_dont_know_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "No", 
                            dat[, var] == "2" ~ "Yes", 
                            dat[, var] == "88" ~ "Don't_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Yes", "No", "Don't_Know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

no_yes_dont_know_not_sure_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "No", 
                            dat[, var] == "2" ~ "Yes", 
                            dat[, var] == "88" ~ "Don't_Know/Not_Sure", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Yes", "No", "Don't_Know/Not_Sure"))
  print(paste0(var, " cleaned"))
  return(dat)
}

no_yes_dont_know_prefer_not_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "No", 
                            dat[, var] == "2" ~ "Yes", 
                            dat[, var] == "88" ~ "Don't_Know/Not_Sure", 
                            dat[, var] == "99" ~ "Prefer_Not_to_Answer",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Yes", "No", "Don't_Know/Not_Sure", "Prefer_Not_to_Answer"))
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
                            dat[, var] == "4" ~ "4_or_More",
                            dat[, var] == "88" ~ "Don't_Know/Not_Sure", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "1", "2", "3", "4_or_More", "Don't_Know/Not_Sure"))
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

replace_1_clean <- function(dat, var, label = "") {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ label,
                            TRUE ~ NA_character_))
  print(paste0(var, " cleaned"))
  return(dat)
}

numeric_clean <- function(dat, var) {
  dat[, var] <- as.numeric(dat[, var])
  print(paste0(var, " cleaned"))
  return(dat)
}

birth_location_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "United_States_(Includes_Hawaii_and_US_Territories)", 
                            dat[, var] == "2" ~ "Africa", 
                            dat[, var] == "3" ~ "Cuba_or_Caribbean_Islands", 
                            dat[, var] == "4" ~ "Other", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "United_States_(Includes_Hawaii_and_US_Territories)", "Africa", "Cuba_or_Caribbean_Islands", "Other"))
  print(paste0(var, " cleaned"))
  return(dat)
}

agree_disagree_degree_5_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Strongly_Agree", 
                            dat[, var] == "2" ~ "Agree", 
                            dat[, var] == "3" ~ "Neutral_(Neither_Agree_nor_Disagree)", 
                            dat[, var] == "4" ~ "Disagree", 
                            dat[, var] == "5" ~ "Strongly_Disagree", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Strongly_Agree", "Agree", "Neutral_(Neither_Agree_nor_Disagree)", "Disagree", "Strongly_Disagree"))
  print(paste0(var, " cleaned"))
  return(dat)
}

agree_disagree_degree_4_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Strongly_Agree", 
                            dat[, var] == "2" ~ "Somewhat_Agree", 
                            dat[, var] == "3" ~ "Somewhat_Disagree", 
                            dat[, var] == "4" ~ "Strongly_Disagree", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Strongly_Agree", "Somewhat_Agree", "Somewhat_Disagree", "Strongly_Disagree"))
  print(paste0(var, " cleaned"))
  return(dat)
}

problem_dont_know_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Non/Minor_Problem", 
                            dat[, var] == "2" ~ "Somewhat_Serious_Problem", 
                            dat[, var] == "3" ~ "Very_Serious_Problem", 
                            dat[, var] == "88" ~ "Don't_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Non/Minor_Problem", "Somewhat_Serious_Problem", "Very_Serious_Problem", "Don't_Know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_often_degree_3_dk_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Often", 
                            dat[, var] == "2" ~ "Sometimes", 
                            dat[, var] == "3" ~ "Rarely/Never", 
                            dat[, var] == "88" ~ "Don't_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Often", "Sometimes", "Rarely/Never", "Don't_Know"))
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
    transmute(v = case_when(dat[, var] == "1" ~ "Less_Than_Once_Per_Week/Do_Not_Exercise", 
                            dat[, var] == "2" ~ "1_to_2_Times_Per_Week",
                            dat[, var] == "3" ~ "3_to_4_Times_Per_Week",
                            dat[, var] == "4" ~ "5_to_7_Times_Per_Week",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Less_Than_Once_Per_Week/Do_Not_Exercise", "1_to_2_Times_Per_Week", "3_to_4_Times_Per_Week", "5_to_7_Times_Per_Week"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_often_per_week_degree_6_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Did_Not_Drink", 
                            dat[, var] == "2" ~ "Fewer_Than_Once_Per_Week", 
                            dat[, var] == "3" ~ "1_to_2_Times_Per_Week",
                            dat[, var] == "4" ~ "3_to_4_Times_Per_Week",
                            dat[, var] == "5" ~ "5_to_6_Times_Per_Week",
                            dat[, var] == "6" ~ "Everyday", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Did_Not_Drink", "Fewer_Than_Once_Per_Week", "1_to_2_Times_Per_Week", "3_to_4_Times_Per_Week", "5_to_6_Times_Per_Week", "Everyday"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_many_drinks_degree_3_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Did_Not_Drink", 
                            dat[, var] == "2" ~ "1_to_2_Drinks", 
                            dat[, var] == "3" ~ "3_or_More_Drinks", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Did_Not_Drink", "1_to_2_Drinks", "3_or_More_Drinks"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_much_time_degree_4_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Do_Not_Exercise", 
                            dat[, var] == "2" ~ "Less_Than_30_Minutes", 
                            dat[, var] == "3" ~ "30_Minutes_to_1_Hour", 
                            dat[, var] == "4" ~ "More_Than_1_Hour", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Do_Not_Exercise", "Less_Than_30_Minutes", "30_Minutes_to_1_Hour", "More_Than_1_Hour"))
  print(paste0(var, " cleaned"))
  return(dat)
}

how_many_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "A_Lot", 
                            dat[, var] == "2" ~ "Some", 
                            dat[, var] == "3" ~ "Few/None", 
                            dat[, var] == "88" ~ "Don't_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "A_Lot", "Some", "Few/None", "Don't_Know"))
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
    transmute(v = case_when(dat[, var] == "1" ~ "Not_at_All_Worried", 
                            dat[, var] == "2" ~ "A_Little_Worried", 
                            dat[, var] == "3" ~ "Somewhat_Worried", 
                            dat[, var] == "4" ~ "Very_Worried", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Not_at_All_Worried", "A_Little_Worried", "Somewhat_Worried", "Very_Worried"))
  print(paste0(var, " cleaned"))
  return(dat)
}

decision_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Made_the_Decision_Alone", 
                            dat[, var] == "2" ~ "Made_the_Decision_Together_With_A_Family_Member_or_Friend", 
                            dat[, var] == "3" ~ "Made_the_Decision_Together_With_A_Family_Member_or_Friend_and_Doctor_Nurse_or_Health_Care_Provider", 
                            dat[, var] == "4" ~ "Made_the_Decision_Together_With_Doctor_Nurse_or_Health_Care_Provider",
                            dat[, var] == "5" ~ "Doctor_Nurse_or_Health_Care_Provider_Made_the_Decision", 
                            dat[, var] == "88" ~ "Don't_Know_or_Remember", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Made_the_Decision_Alone", "Made_the_Decision_Together_With_A_Family_Member_or_Friend", "Made_the_Decision_Together_With_A_Family_Member_or_Friend_and_Doctor_Nurse_or_Health_Care_Provider",  "Made_the_Decision_Together_With_Doctor_Nurse_or_Health_Care_Provider", "Doctor_Nurse_or_Health_Care_Provider_Made_the_Decision", "Don't_Know_or_Remember"))
  print(paste0(var, " cleaned"))
  return(dat)
}

decision_clean_6 <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Made_the_Decision_Alone", 
                            dat[, var] == "2" ~ "Made_the_Decision_Together_With_A_Family_Member_or_Friend", 
                            dat[, var] == "3" ~ "Made_the_Decision_Together_With_A_Family_Member_or_Friend_and_Doctor_Nurse_or_Health_Care_Provider", 
                            dat[, var] == "4" ~ "Made_the_Decision_Together_With_Doctor_Nurse_or_Health_Care_Provider",
                            dat[, var] == "5" ~ "Doctor_Nurse_or_Health_Care_Provider_Made_the_Decision", 
                            dat[, var] == "6" ~ "Don't_Know_or_Remember", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Made_the_Decision_Alone", "Made_the_Decision_Together_With_A_Family_Member_or_Friend", "Made_the_Decision_Together_With_A_Family_Member_or_Friend_and_Doctor_Nurse_or_Health_Care_Provider",  "Made_the_Decision_Together_With_Doctor_Nurse_or_Health_Care_Provider", "Doctor_Nurse_or_Health_Care_Provider_Made_the_Decision", "Don't_Know_or_Remember"))
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

marital_status_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Married_or_Living_With_A_Partner", 
                            dat[, var] == "2" ~ "Separated", 
                            dat[, var] == "3" ~ "Divorced", 
                            dat[, var] == "4" ~ "Widowed", 
                            dat[, var] == "5" ~ "Never_Married", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Married_or_Living_With_A_Partner", "Separated", "Divorced", "Widowed", "Never_Married"))
  print(paste0(var, " cleaned"))
  return(dat)
}

sexual_identity_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Straight/Heterosexual",
                            dat[, var] == "2" ~ "Bisexual", 
                            dat[, var] == "3" ~ "Gay/Homosexual/Same_Gender_Loving", 
                            dat[, var] == "4" ~ "Other", 
                            dat[, var] == "99" ~ "Prefer_Not_to_Answer",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Straight/Heterosexual", "Bisexual", "Gay/Homosexual/Same_Gender_Loving", "Other", "Prefer_Not_to_Answer"))
  print(paste0(var, " cleaned"))
  return(dat)
}

education_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Grade_School_or_Less", 
                            dat[, var] == "2" ~ "Some_High_School", 
                            dat[, var] == "3" ~ "High_School_Graduate_or_GED", 
                            dat[, var] == "4" ~ "Vocational_School", 
                            dat[, var] == "5" ~ "Some_College", 
                            dat[, var] == "6" ~ "Associate's_Degree", 
                            dat[, var] == "7" ~ "College_Graduate_(Bachelor's_Degree)", 
                            dat[, var] == "8" ~ "Some_Graduate_Education", 
                            dat[, var] == "9" ~ "Graduate_Degree", 
                            dat[, var] == "88" ~ "Don't_Know", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Grade_School_or_Less", "Some_High_School", "High_School_Graduate_or_GED", "Vocational_School", "Some_College", "Associate's_Degree", "College_Graduate_(Bachelor's_Degree)", "Some_Graduate_Education", "Graduate_Degree", "Don't_Know"))
  print(paste0(var, " cleaned"))
  return(dat)
}

current_activity_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Currently_Working_Full_Time", 
                            dat[, var] == "2" ~ "Currently_Working_Part_Time", 
                            dat[, var] == "3" ~ "Looking_for_Work_Unemployed", 
                            dat[, var] == "4" ~ "Retired", 
                            dat[, var] == "5" ~ "On_Disability_Permanently", 
                            dat[, var] == "6" ~ "On_Disability_for_A_Period_of_Time_(On_Sick_Leave_or_Paternity_Leave_or_Disability_Leave_for_Other_Reasons)", 
                            dat[, var] == "7" ~ "Volunteer_Work/Work_Without_Pay", 
                            dat[, var] == "8" ~ "Other",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Currently_Working_Full_Time", "Currently_Working_Part_Time", "Looking_for_Work_Unemployed", "Retired", "On_Disability_Permanently", "On_Disability_for_A_Period_of_Time_(On_Sick_Leave_or_Paternity_Leave_or_Disability_Leave_for_Other_Reasons)", "Volunteer_Work/Work_Without_Pay", "Other"))
  print(paste0(var, " cleaned"))
  return(dat)
}

family_income_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Less_Than_$15,000", 
                            dat[, var] == "2" ~ "$15,000_to_$35,999", 
                            dat[, var] == "3" ~ "$36,000_to_$45,999", 
                            dat[, var] == "4" ~ "$46,000_to_$65,999", 
                            dat[, var] == "5" ~ "$66,000_to_$99,999", 
                            dat[, var] == "6" ~ "$100,000_to_$149,999", 
                            dat[, var] == "7" ~ "$150,000_to_$199,999", 
                            dat[, var] == "8" ~ "$200,000_or_More",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Less_Than_$15,000", "$15,000_to_$35,999", "$36,000_to_$45,999", "$46,000_to_$65,999", "$66,000_to_$99,999", "$100,000_to_$149,999", "$150,000_to_$199,999", "$200,000_or_More"))
  print(paste0(var, " cleaned"))
  return(dat)
}

home_ownership_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Owned_or_Being_Bought_by_Self_(or_Someone_in_the_Household)", 
                            dat[, var] == "2" ~ "Rented_for_Money",
                            dat[, var] == "3" ~ "Other", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Owned_or_Being_Bought_by_Self_(or_Someone_in_the_Household)", "Rented_for_Money", "Other"))
  print(paste0(var, " cleaned"))
  return(dat)
}

household_security_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Less_Than_1_Month", 
                            dat[, var] == "2" ~ "1_to_2_Months", 
                            dat[, var] == "3" ~ "3_to_6_Months", 
                            dat[, var] == "4" ~ "More_Than_6_Months",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Less_Than_1_Month", "1_to_2_Months", "3_to_6_Months", "More_Than_6_Months"))
  print(paste0(var, " cleaned"))
  return(dat)
}
#```

# Variable specific functions
#```{r Variable-specific functions}
methodology_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "P" ~ "Paper",
                            dat[, var] == "O" ~ "Online", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Paper", "Online"))
  print(paste0(var, " cleaned"))
  return(dat)
}

a1not_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "NEVER_Had_Prostate_Cancer",
                            dat[, var] == "2" ~ "HAVE_or_Had_Prostate_Cancer", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "HAVE_or_Had_Prostate_Cancer", "NEVER_Had_Prostate_Cancer"))
  print(paste0(var, " cleaned"))
  return(dat)
}

a8_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "15_Years_or_Less",
                            dat[, var] == "2" ~ "16_to_25_Years", 
                            dat[, var] == "3" ~ "Whole_Life_or_More_Than_25_Years", 
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "15_Years_or_Less", "16_to_25_Years", "Whole_Life_or_More_Than_25_Years"))
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

routine_medical_care_location_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Community_Health_Center_or_Free_Clinic",
                            dat[, var] == "2" ~ "Hospital_(Not_Emergency)/Urgent_Care_Clinic",
                            dat[, var] == "3" ~ "Private_Doctor's_Office",
                            dat[, var] == "4" ~ "Emergency_Room",
                            dat[, var] == "5" ~ "Veteran's_Affairs",
                            dat[, var] == "6" ~ "Other_Type_of_Location",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Community_Health_Center_or_Free_Clinic", "Hospital_(Not_Emergency)/Urgent_Care_Clinic", "Private_Doctor's_Office", "Emergency_Room", "Veteran's_Affairs", "Other_Type_of_Location"))
  print(paste0(var, " cleaned"))
  return(dat) 
}

lived_at_current_address_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Less_Than_1_Year",
                            dat[, var] == "2" ~ "1_to_5_Years",
                            dat[, var] == "3" ~ "6_to_10_Years",
                            dat[, var] == "4" ~ "11_to_15_Years",
                            dat[, var] == "5" ~ "16_to_20_Years",
                            dat[, var] == "6" ~ "21_plus_Years",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Less_Than_1_Year", "1_to_5_Years", "6_to_10_Years", "11_to_15_Years", "16_to_20_Years", "21_plus_Years"))
  print(paste0(var, " cleaned"))
  return(dat) 
}

risk_of_progression_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Low_Risk_of_Progression",
                            dat[, var] == "2" ~ "Intermediate_Risk_of_Progression",
                            dat[, var] == "3" ~ "High_Risk_of_Progression",
                            dat[, var] == "4" ~ "Unknown_Risk_of_Progression",
                            dat[, var] == "88" ~ "Don't_Know/Don't_Remember",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Low_Risk_of_Progression", "Intermediate_Risk_of_Progression", "High_Risk_of_Progression", "Unknown_Risk_of_Progression", "Don't_Know/Don't_Remember"))
  print(paste0(var, " cleaned"))
  return(dat) 
}

gleason_score_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "6_or_Less",
                            dat[, var] == "2" ~ "7",
                            dat[, var] == "3" ~ "8_to_10",
                            dat[, var] == "88" ~ "Don't_Know",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "6_or_Less", "7", "8_to_10", "Don't_Know"))
  print(paste0(var, " cleaned"))
  return(dat) 
}

stage_clean <- function(dat, var) {
  dat[, var] <- as.data.frame(dat[, var]) %>% 
    transmute(v = case_when(dat[, var] == "1" ~ "Localized_Confined_to_Prostate",
                            dat[, var] == "2" ~ "Regional_Tumor_Extended_to_Regions_Around_the_Prostate",
                            dat[, var] == "3" ~ "Distant_Tumor_Extended_to_Bones_or_Other_Parts_of_Body",
                            dat[, var] == "88" ~ "Don't_Know_About_the_Stage",
                            TRUE ~ NA_character_)) %>%
    transmute(v = fct_relevel(v, "Localized_Confined_to_Prostate", "Regional_Tumor_Extended_to_Regions_Around_the_Prostate", "Distant_Tumor_Extended_to_Bones_or_Other_Parts_of_Body", "Don't_Know_About_the_Stage"))
  print(paste0(var, " cleaned"))
  return(dat) 
}
#```