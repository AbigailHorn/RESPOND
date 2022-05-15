library(shiny)
library(shinydashboard)
library(table1)

ui <- dashboardPage(
  dashboardHeader(title = "Table1"),
  dashboardSidebar(),
  dashboardBody(
    box(tableOutput("T1")),
    box(selectInput('cat_var', 'Variable', c("Type", "Treatment")), width = 4
    )
  )
)

#Server
server <- function(input, output){
  
  output$T1 = renderTable({
    table1(~ conc + uptake | input$cat_var, data=CO2, topclass="Rtable1-shade")
  })
  
}

shinyApp(ui, server)