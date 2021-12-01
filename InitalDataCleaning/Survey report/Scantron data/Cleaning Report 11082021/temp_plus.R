
setwd ("C:/Users/Universe/Desktop/RESPOND/google drive")

library(rmarkdown)

#Scantron data
for (i in c("Los Angeles County.80", "Northern CA.30","Greater CA.10", "Detroit.60", "Louisiana.40", "Georgia.20","Michigan.61", "New Jersey.50", "Texas.70", "WebRecruit.99", "USC Other.81","USC MEC.82")) {
  render("Surveyonly_cleaning_all.Rmd",
                    params = list(site = i),
                    output_file=paste0('SurveyOnly cleaning report.', i, '.html'))
}
 
#  merged data
# for (i in c("Los Angeles County.80", "Northern CA.30","Greater CA.10", "Detroit.60", "Louisiana.40", "Georgia.20", "Michigan.61")) {
#   render("Cleaning report of B1.Rmd",
#          params = list(site = i),
#          output_file=paste0('Merge report.', i, '.html'))
# }
