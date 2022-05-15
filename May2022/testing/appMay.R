


library(shiny)
library(htmlwidgets)
library(pivottabler)
library(fs)
library(here)

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



# table1( as.formula("~Breast_Yes + Ovarian_Yes | 
#          aggressiveness_grant"), extra.col=list('P-value'=pvalue), data=d, topclass="Rtable1-shade", overall=FALSE)
# 
# inputs = paste0( "~" ,  paste("Breast_Yes", "Ovarian_Yes", sep = "+"), "|", "aggressiveness_grant")
# table1( as.formula(inputs), extra.col=list('P-value'=pvalue), data=d, topclass="Rtable1-shade", overall=FALSE)
# 
# rowInput <- c("Breast_Yes", "Ovarian_Yes","Colorectal_Yes", "Lung_Yes", "Other_Cancer_Yes", "Any_Cancer_Yes")
# rowInput <- "psa"
# colInput <- "psa_category"
# 
# # Final version - character inputs to formula to table1
# inputs = paste0( "~" ,  paste(rowInput,collapse="+"), "|", colInput)
# table1( as.formula(inputs), extra.col=list('P-value'=pvalue), data=d, topclass="Rtable1-shade", overall=FALSE)

# Next: get row selections as a vector and column selection as a single option













# Summary table output
# d %>% select(clinical_stage_category, d2a) %>% gtsummary::tbl_summary(by=d2a,  percent = "row")

# rsconnect::setAccountInfo(name='abigail-horn', token='31B8FC87628C3F8CDE3B886D5C085ED1', secret='1mfe25nVyMDUU8NmRD7SZWqCRAWvDrJ0wcE2r+3u')
# library(rsconnect)
# rsconnect::deployApp('shiny_deploy')

# load(path(here::here("InitalDataCleaning/Data/Clean Data/Shiny_Merge_2021_12_09.rda")))
# d.relevel <- d %>% dplyr::mutate(
#   clinical_stage_category = factor(clinical_stage_category, levels = c("high","med","low")),
#   gleason_category = factor(gleason_category, levels = c("high","med","low")),
#   D_Amico_risk_groups = factor(D_Amico_risk_groups, levels = c("high","med","low","Other")),
#   aggressiveness_grant = factor(aggressiveness_grant, levels = c("high","med","low", "Metastatic", "Other")),
# )
# # save(d.relevel, file ="testing/pivot-deploy2/Shiny_Merge_2021_12_09_relevel.rda")

#load("Shiny_Merge_2021_12_09_relevel.rda")

ui <- fluidPage(
  
  titlePanel("Disease Characteristics by Survey Variables"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selectRows", label = h5("Disease Characteristic"),
                  #'Variable',
                  choices = c("Breast_Yes", "Ovarian_Yes","Colorectal_Yes", "Lung_Yes", "Other_Cancer_Yes", "Any_Cancer_Yes"),
                  # choices = list("Clinical Stage" = "clinical_stage_category",
                  #                "Gleason Category" = "gleason_category",
                  #                "D'Amico Risk Group" = "D_Amico_risk_groups",
                  #                "Aggressiveness - Grant" = "aggressiveness_grant",
                  #                "Aggressiveness - Epi" = "aggressiveness_epi" ),
                  multiple = TRUE,
                  selected = "Breast_Yes"),
      
      selectInput("selectCols", label = h5("Survey Variable"),
                  #'Variable',
                  # choices = list("Train Category" = "TrainCategory",
                  #                "TOC" = "TOC",
                  #                "Power Type" = "PowerType"), selected = "TrainCategory")
                  choices = c("aggressiveness_grant","psa_category"),
                  multiple = FALSE,
                 
                  selected = "aggressiveness_grant")
    ),
    
    mainPanel(
      #pivottablerOutput('pvt')
      tableOutput("T1")
    )
  )
)

server <- function(input, output) {
  
  table1_reactive <- reactive({
    
    
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
      c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
    }
    
    
    
    rowInput <- input$selectRows
    colInput <- input$selectCols
    inputs = paste0( "~" ,  paste(rowInput,collapse="+"), "|", colInput)
    
    table1( as.formula(inputs), extra.col=list('P-value'=pvalue), data=d, topclass="Rtable1-shade", overall=FALSE)
    
    
    # output$T1 = renderTable({
    #   #table1(~ conc + uptake | input$cat_var, data=CO2)
    # table1( as.formula(inputs), extra.col=list('P-value'=pvalue), data=d, topclass="Rtable1-shade", overall=FALSE)
    #   
    # })
    
    
  })
  


  
  output$T1 <- renderTable({
    table1_reactive()
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


## To update:
# library(rsconnect)
# rsconnect::deployApp('testing/html-test')
