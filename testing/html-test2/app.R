
library(shiny)
ui = fluidPage(
  #titlePanel("Detailed site demographic reports"),
  titlePanel("Option 2: Shiny"),
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
)

server <- function(input, output) {
  getPage<-function() {
    if (input$html == "Choose site") {
      return(includeHTML("fragment.html"))}
    if (input$html == "Georgia") {
      return(includeHTML("Georgia.html"))}
    if (input$html == "LA County") {
      return(includeHTML("LAC.html"))}
    if (input$html == "Fragment") {
      return(includeHTML("fragment.html"))}
  }
  output$inc<-renderUI({getPage()})
  output$text <- renderText(input$html)
}

shinyApp(ui, server)