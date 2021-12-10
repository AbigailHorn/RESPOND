
library(shiny)
library(htmlwidgets)
library(pivottabler)
library(fs)
library(here)

load(path(here::here("InitalDataCleaning/Data/Clean Data/Shiny_Merge_2021_12_09.rda")))
view(d)

# Weija data
# load(path(here::here("InitalDataCleaning/Data/Clean Data/Clean_Scantron20211122.rda")))

# Summary table output
# d %>% select(clinical_stage_category, d2a) %>% gtsummary::tbl_summary(by=d2a,  percent = "row")


ui <- fluidPage(
  
  titlePanel("Disease Characteristics by Survey Variables"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selectRows", label = h5("Disease Characteristic"),
                  # choices = list("Train Category" = "TrainCategory",
                  #                "TOC" = "TOC",
                  #                "Power Type" = "PowerType"), selected = "TOC"),
                  choices = list("Clinical Stage" = "clinical_stage_category",
                                 "Gleason Category" = "gleason_category",
                                 "D'Amico Risk Group" = "D_Amico_risk_groups",
                                 "Aggressiveness - Grant" = "aggressiveness_grant",
                                 "Aggressiveness - Epi" = "aggressiveness_epi"
                  ), selected = "clinical_stage_category"),
      
      selectInput("selectCols", label = h5("Survey Variable"),
                  # choices = list("Train Category" = "TrainCategory",
                  #                "TOC" = "TOC",
                  #                "Power Type" = "PowerType"), selected = "TrainCategory")
                  choices = list(
                    "Medical mistrust - patients deceived or misled at hospitals" = "d2a",
                    "Medical mistrust - hospitals do harmful experiments " = "d2c",
                    "Before diagnosis - negative biopsy?" = "e2aa",
                    "Before diagnosis - how many negative biopsies?" = "e2aa",
                    "Before diagnosis - normal PSA blood tests?" = "e2aa",
                    "Before diagnosis - how many normal PSA blood tests?" = "e2aa",
                    "Understanding of aggressiveness" = "e4",
                    "Gleason Score" = "e5",
                    "Understanding of Stage" = "e6",
                    "Alcohol Frequency" = "f5",
                    "Smoked > 100 cigarettes" = "f7",
                    "Age started smoking" = "f7age",
                    "Cigarettes smoked/day" = "f7a",
                    "Quit smoking yet?" = "f7b",
                    "Age quit smoking" = "f7bage",
                    "Marital status" = "g1"
                  ), selected = "d2a")
    ),
    
    mainPanel(
      pivottablerOutput('pvt')
    )
  )
)

server <- function(input, output) {
  
  output$pvt <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(d)
    pt$addColumnDataGroups(input$selectCols)
    pt$addRowDataGroups(input$selectRows)
    #pt$defineCalculation(calculationName="TotalNo.", summariseExpression="n()")
    
    pt$defineCalculation(calculationName="Count", visible=FALSE, summariseExpression="n()", 
                         caption="Count")
    filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor=input$selectRows)
    pt$defineCalculation(calculationName="TotalNo.", visible=FALSE, filters=filterOverrides, 
                         summariseExpression="n()", caption="Total No.")
    pt$defineCalculation(calculationName="Percentage", type="calculation", 
                         basedOn=c("Count", "TotalNo."),
                         calculationExpression="values$Count/values$TotalNo.*100", 
                         format="%.1f %%", caption="% of total")
    
    
    pt$evaluatePivot()
    pivottabler(pt)
  })
}

shinyApp(ui = ui, server = server)
