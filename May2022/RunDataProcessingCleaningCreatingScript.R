

setwd("/Volumes/GoogleDrive/My Drive/Projects/RESPOND/CORE D/DataProcessingCleaningCreation/")

rmarkdown::render("Survey/Survey_Processing.Rmd")   # creates no output data
rmarkdown::render("Survey/Survey_Cleaning.Rmd") # creates output file="../Data/vsurvall_cleaned_YYYYMMDD.Rda"
rmarkdown::render("Survey/Survey_CreateVariables.Rmd") # creates output file="../Data/vsurvall_analysis_YYYYMMDD.Rda" and file="../Data/vsurvall_analysis_YYYYMMDD.csv"

rmarkdown::render("Registry/Registry_Processing.Rmd") # creates no output data
rmarkdown::render("Registry/CreateRegistryVariables.Rmd")

rmarkdown::render("MergeData/MergeRegistrySurvey.Rmd")
rmarkdown::render("MergeData/MergedDataSummary.Rmd")


