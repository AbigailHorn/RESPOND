#setwd("C:/Users/Universe/Desktop/RESPOND/google drive")
setwd(here())

library(rmarkdown)

scan.dir = path(here::here("InitalDataCleaning/Survey report/Scantron data/Cleaning Report 12072021"))

# All sites reports
render(path(scan.dir, "RShiny_all sites.Rmd"), output_file=path(scan.dir,"SurveyOnly cleaning report.All sites.html"))


# Site-specific reports
for (i in c("Los Angeles County.80", "Northern CA.30","Greater CA.10", "Detroit.60", "Louisiana.40", "Georgia.20","Michigan.61", "New Jersey.50", "Texas.70", "WebRecruit.99", "USC Other.81","USC MEC.82")) {
#for (i in c("Los Angeles County.80")) {
    render(path(scan.dir, "RShiny_each site.Rmd"),
         params = list(site = i),
         output_file=paste0(path(scan.dir,'SurveyOnly cleaning report.'), i, '.html'))
}

