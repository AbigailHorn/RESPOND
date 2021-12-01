
setwd ("C:/Users/Universe/Desktop/RESPOND/google drive")

library(rmarkdown)

#Scantron data
# for (i in c("Los Angeles County.80", "Northern CA.30","Greater CA.10", "Detroit.60", "Louisiana.40", "Georgia.20","Michigan.61", "New Jersey.50", "Texas.70", "WebRecruit.99", "Georgia.21","USC Other.81","USC MEC.82")) {
#   render("Survey_processing_all - Clean.Rmd",
#                     params = list(site = i),
#                     output_file=paste0('report.', i, '.html'))
# }

# merged data
for (i in c("Los Angeles County.80", "Northern CA.30","Greater CA.10", "Detroit.60", "Louisiana.40", "Georgia.20", "Michigan.61")) {
  render("Survey_processing_all.Rmd",
         params = list(site = i),
         output_file=paste0('report.', i, '.html'))
}
