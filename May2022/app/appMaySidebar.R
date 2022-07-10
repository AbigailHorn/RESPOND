

library(shiny)
library(shinyAce)
library(htmlwidgets)
library(pivottabler)
library(fs)
library(here)
library(shinythemes)

require("RPostgreSQL") # for PostgreSQL
library(zoo) # for date conversion
library(expss) # for labels
library(DBI) # for Database connectivity
library(summarytools) # for summarizing variables
library(tidyverse)
library(plyr)
library(knitr) # for creating tables
#library(questionr)
library(table1)
library(xtable)
library(ggplot2)

# Dave data
load(path(here::here("May2022/Data/vmerged_20220430.Rda")))

# Column Variables
colvars = colnames(d)

# Get the first ID
start_loc = match("FH_Sum",colvars)

# Get the second ID
end_loc = match("aggressiveness_epi",colvars)

# Subset range
b = d[,start_loc:end_loc]
varsToSelect <- colnames(b)

ui <- fluidPage(theme = shinytheme("superhero"),
  
  titlePanel("Disease Characteristics by Survey Variables"),
  
  sidebarLayout(
    sidebarPanel(
      
      # includeCSS(here::here(master.dir, "www/table1.css")),
      # includeScript(here::here(master.dir, "www/js/jquery-ui-1.10.3.custom.min.js")),
     # jsCodeHandler(),
      
      wellPanel(
        p("An interface for producing a typical Table 1 from RESPOND survey variables.")
      ),
      
      wellPanel(
        p(helpText("Select the factor variable(s) that will produce the rows"),
          selectInput("selectRows","Disease Characteristic:", choices=varsToSelect, 
                      multiple=TRUE, selected = c("Breast_Yes", "Ovarian_Yes","Colorectal_Yes", "Lung_Yes", "Other_Cancer_Yes", "Any_Cancer_Yes"))
        )),
      
      wellPanel(
        p(helpText("Select the factor variable that will produce the columns"),
          selectInput("selectCols","Survey Variable:", choices=varsToSelect[varsToSelect != "psa"], 
                      multiple=FALSE, selected = "aggressiveness_grant")
        ))
      
   
    ), # end sidebarpanel
    
    # mainPanel(
    # 
    #   tabsetPanel(id="mainPanelTabset",
    #               tabPanel("a",
    #                        tableOutput("T1")
    #               ),
    #               tabPanel("b",
    #                 tableOutput("T1")
    #               )
    #               # tabPanel("Source",
    #               #          aceEditor("acer", mode="r")
    #               # )
    #   )
    # 
    # ) # mainpanel
    mainPanel(
      #pivottablerOutput('pvt')
      h3(textOutput("selected_var")),
      tableOutput("T1")
    )
    
    
  ) # sidebarLayout
)



server <- function(input, output) {
  
  table1_reactive <- reactive({
    
    validate(
      need(input$selectRows != "", "Please select a disease characteristic")
    )
    
    pvalue <- function(x, ...) {
      # Construct vectors of data y, and groups (strata) g
      y <- unlist(x)
      g <- factor(rep(1:length(x), times=sapply(x, length)))
      if (is.numeric(y)) {
        # For numeric variables, perform a anova ## standard 2-sample t-test
        p <- anova(lm(y~g))$`Pr(>F)`   # t.test(y ~ g)$p.value  
      } else {
        # For categorical variables, perform a chi-squared test of independence
        p <- chisq.test(table(y, g))$p.value
      }
      # Format the p-value, using an HTML entity for the less-than sign.
      # The initial empty string places the output on the line below the variable label.
      c("", sub("<", "<", format.pval(p, digits=3, eps=0.001)))
    }
    
    
    
    rowInput <- input$selectRows
    colInput <- input$selectCols
    inputs = paste0( "~" ,  paste(rowInput,collapse="+"), "|", colInput)
    
    table1( as.formula(inputs), extra.col=list('P-value'=pvalue), data=b, topclass="Rtable1-shade", overall=FALSE)
    
    
    # output$T1 = renderTable({
    #   #table1(~ conc + uptake | input$cat_var, data=CO2)
    # table1( as.formula(inputs), extra.col=list('P-value'=pvalue), data=d, topclass="Rtable1-shade", overall=FALSE)
    #   
    # })
    
    
  })
  
  
  
  
  output$T1 <- renderTable({
    table1_reactive()
  })  
  
  
  output$selected_var <- renderText({ 
    paste("Table by categories of ", input$selectCols)
  })
  
  # output$pvt <- renderPivottabler({
  #   pt <- PivotTable$new()
  #   pt$addData(d.relevel)
  #   #pt$defineCalculation(calculationName="TotalNo.", summariseExpression="n()")
  #   
  #   pt$defineCalculation(calculationName="Count", summariseExpression="n()", 
  #                        caption="Count")
  #   filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor=input$selectRows)
  #   pt$defineCalculation(calculationName="TotalNo.", visible=FALSE, filters=filterOverrides, 
  #                        summariseExpression="n()", caption="Total No.")
  #   pt$defineCalculation(calculationName="Percentage", type="calculation", 
  #                        basedOn=c("Count", "TotalNo."),
  #                        calculationExpression="values$Count/values$TotalNo.*100", 
  #                        format="%.1f %%", caption="% of total")
  #   
  #   pt$addRowCalculationGroups()
  #   pt$addColumnDataGroups(input$selectCols)
  #   pt$addRowDataGroups(input$selectRows)
  #   
  #   
  #   pt$evaluatePivot()
  #   pivottabler(pt)
  # })
}

shinyApp(ui = ui, server = server)