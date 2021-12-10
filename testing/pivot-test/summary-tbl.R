library(shiny)
library(htmlwidgets)
library(pivottabler)

ui <- fluidPage(
  
  titlePanel("Pivottabler Minimal Example Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selectRows", label = h5("Rows"),
                  choices = list("Train Category" = "TrainCategory",
                                 "TOC" = "TOC",
                                 "Power Type" = "PowerType"), selected = "TOC"),
      selectInput("selectCols", label = h5("Columns"),
                  choices = list("Train Category" = "TrainCategory",
                                 "TOC" = "TOC",
                                 "Power Type" = "PowerType"), selected = "TrainCategory")
    ),
    
    mainPanel(
      pivottablerOutput('pvt')
    )
  )
)


library(fs)
library(here)


server <- function(input, output) {
  
  cancer <- load(path(here("InitalDataCleaning/Data/Clean Data/Shiny_merge20211122.rda")))
  
  output$pvt <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(bhmtrains)
    pt$addColumnDataGroups(input$selectCols)
    pt$addRowDataGroups(input$selectRows)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
    pt$evaluatePivot()
    pivottabler(pt)
  })
}

shinyApp(ui = ui, server = server)



#######

site.agg <- new.d %>% select(siteid, surveyid, aggre) #%>% filter(siteid=="Michigan.61")
site.pivot <- pivot_wider(site.agg, names_from = "siteid", values_from = "aggre")

table1(siteid ~ aggre, overall="LACHS Full Sample Population", data = site.agg)
site.agg %>% select(-surveyid) %>% gtsummary::tbl_summary(by=aggre,  percent = "row")

new.d %>% select(siteid, aggre_cat) %>% gtsummary::tbl_summary(by=aggre_cat,  percent = "row")
new.d %>% select(siteid, ComorbScore) %>% gtsummary::tbl_summary(by=ComorbScore,  percent = "row")
new.d %>% select(siteid, psa_cat) %>% gtsummary::tbl_summary(by=psa_cat,  percent = "row")
new.d %>% select(aggre_cat, grade) %>% gtsummary::tbl_summary(by=grade,  percent = "row")

# Heatmaps by site, 




