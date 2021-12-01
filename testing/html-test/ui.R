# shinyUI(fluidPage(
#   titlePanel("Uploading Files"),
#   mainPanel(
#     htmlOutput("inc")
#   )
# ))

# rsconnect::setAccountInfo(name='abigail-horn', token='31B8FC87628C3F8CDE3B886D5C085ED1', secret='1mfe25nVyMDUU8NmRD7SZWqCRAWvDrJ0wcE2r+3u')

## To update:
# library(rsconnect)
# rsconnect::deployApp('testing/html-test')

library(shiny)
shinyUI(fluidPage(
  titlePanel("Detailed site demographic reports"),
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:15%;",
      h1("Choose the site location"),
      selectInput("html", "Site Name", c("Choose site", "Georgia", "LA County")),
      width = 2
      #sliderInput("slider1", "Slide Me!", 0, 100, 0)
    ),
    mainPanel(
      #h3("Site report for:", textOutput("text")),
      htmlOutput("inc")
    )
  )
))

