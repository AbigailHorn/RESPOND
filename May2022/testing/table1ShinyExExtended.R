

library(shiny)
library(shinyAce)
library(whisker)
library(here)


# navigator.dir = path("/Volumes/GoogleDrive/Shared drives/Disparities Navigator")
# d <- read.csv(here::here(navigator.dir, "LAC_AAIRs_bySiteSex5YearRangeRaceEth_20002019_03FEB22.csv"), na.strings = c("NA", "~"))

this.dir <- path("May2022/testing/Table1-master/R")
master.dir <- path("May2022/testing/Table1-master")
source(here::here(this.dir,"dkdfinfo.r"))
source(here::here(this.dir,"lib.r"))
source(here::here(this.dir,"melanoma.r"))

if (packageVersion("shiny") < "0.11")
  stop("Need shiny version >= 0.11 for bootstrap 3")

# to run
# shiny:::runApp()
# shiny:::runApp("../Table1", launch.browser = rstudio::viewer)

data(iris)

# Define UI for dataset viewer application
ui <- fluidPage(
  
  titlePanel("Table1"),
  
  sidebarLayout(
    sidebarPanel(
      
      includeCSS(here::here(master.dir, "www/table1.css")),
      includeScript(here::here(master.dir, "www/js/jquery-ui-1.10.3.custom.min.js")),
      jsCodeHandler(),
      
      wellPanel(
        p("Table1 is an interface to the Gmisc htmlTable function for producing a typical cases vs controls Table 1.")
      ),
      
      selectInput("dataset", "Dataframe:", choices = getDataFrames()),
      
      wellPanel(
        p(helpText("Select the factor variable that will produce the columns, ",
                   "typically the Cases vs Controls ID var."),
          selectInput("colFactor","Columns Variable:", choices=getdfinfo(getDataFrames()[1])$factors$name, multiple=F)
        )),
      
      wellPanel(
        p(helpText("Select the numerics and factors to include ",
                   "in the rows of the table.")),
        selectizeInput("numerics", "Numerics:", choices=getdfinfo(getDataFrames()[1])$numerics$name, selected="", multiple=T, 
                       options=list(placeholder="Select numeric(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""))),
        selectizeInput("factors", "Factors:", choices=getdfinfo(getDataFrames()[1])$factors$name, selected="", multiple=T, 
                       options=list(placeholder="Select factor(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop="")))
      ),
      
      p("Options:"),
      accordion("optionsAccordion", 
                accordionPanel("Column",
                               checkboxInput("chkStatistics", "Show Statistics", F),
                               checkboxInput("chkTotals", "Show Total Column", T),
                               checkboxInput("chkNEJM", "NEJM Style n (%)", T),
                               checkboxInput("chkColN", "N= in column header", T),
                               p(),
                               spreadsheetInput("tblColOptions", rbind(c("","","")), 
                                                colHeaders='["Name","Justify","Group"]',
                                                options='columns: [ {}, {type: "dropdown", source: ["c","l","r"] }, {} ]')
                ), 
                accordionPanel("Row", 
                               spreadsheetInput("tblRowOptions", rbind(c("","")), colHeaders='["Name","Digits"]'),
                               radioButtons("describeNumeric", "Numeric statistic:", choices=c("Mean","Median"), selected="Mean", inline=T)
                ),
                accordionPanel("Table",
                               textInput("txtCaption", "Caption:"),
                               textInput("txtCapLoc", "Caption Location:", "top"),
                               textInput("txtFooter", "Footer:"),
                               radioButtons("radTableWidth", "Table Width", choices=c("40%","60%","80%","100%"), selected="80%")
                )
      ) # accordion
    ), # end sidebarpanel
    
    mainPanel(
      
      tabsetPanel(id="mainPanelTabset",
                  tabPanel("Table", 
                           htmlOutput("Table1")
                  ),
                  tabPanel("Source",
                           aceEditor("acer", mode="r")
                  )
      )
      
    ) # mainpanel
  ) # sidebarLayout
)

source(here::here(this.dir,"table1.r"))

server <- function(input, output, session){
  
  
  
  # Define server logic required to summarize and view the selected dataset
  # shinyServer(function(input, output, session) {
    
    colGroups <<- c()
    n.colGroups <<- c()
    
    # Return the requested dataset
    getSelectedDFName <- reactive({
      input$dataset
    })
    
    getSelectedDF <- reactive({
      eval(parse(text=input$dataset))
    })
    
    getDFInfo <- reactive({
      getdfinfo(input$dataset)
    })
    
    # use observe to connect a change in input$dataset to the select boxes
    observe({
      dfinfo = getDFInfo()
      
      # update colFactor
      updateSelectInput(session, "colFactor", choices=dfinfo$factors$name)
      # Update the field selects
      updateSelectInput(session, "numerics", choices=dfinfo$numerics$name)
      updateSelectInput(session, "factors", choices=dfinfo$factors$name)
      
    }, priority=1)
    
    # if the column factor selection is changed....
    observe({
      isolate({curdf = getSelectedDF()}) # use isolate to avoid circular reactivity
      colFactor = input$colFactor
      
      colOptions = rbind()
      for (x in levels(curdf[, colFactor])) {
        colOptions = rbind(colOptions, c(x, "c", colFactor))
      }
      
      session$sendInputMessage("tblColOptions", list(value=colOptions))
      
      if (length(levels(curdf[, colFactor]))>2)
        disableControl("chkStatistics", session)
      else
        enableControl("chkStatistics", session)
      
    })
    
    getSelectedFields = reactive({
      selectedFields = rbind()
      
      # first get the current selections from the handsontable
      if (!is.null(input$tblRowOptions))
        selectedFields = t(sapply(fromJSON(input$tblRowOptions),paste))
      
      # add new selections from the field lists
      newSelection = which(!(c(input$numerics, input$factors) %in% selectedFields[,1]))
      if (length(newSelection))
        selectedFields = rbind(selectedFields, c(c(input$numerics, input$factors)[newSelection], "2"), deparse.level=0)
      
      # remove unselections
      removedItem = which(!(selectedFields[,1] %in% c(input$numerics, input$factors)))
      if (length(removedItem))
        selectedFields = rbind(selectedFields[-removedItem, ])
      
      # update tblRowOptions
      if (length(selectedFields) == 0) {
        session$sendInputMessage("tblRowOptions", list(value=rbind(c("",""))))
        selectedFields = rbind()
      }
      else
        session$sendInputMessage("tblRowOptions", list(value=selectedFields))
      
      return(selectedFields)
    })
    
    getColOptions = reactive({
      colOptions = rbind()
      
      # first get the current optionsselections from the handsontable
      if (!is.null(input$tblColOptions))
        colOptions = t(sapply(fromJSON(input$tblColOptions),paste)) 
      
      return(colOptions)
      
    })
    
    output$Table1 = renderText({
      
      selectedFields = getSelectedFields()
      rownames(selectedFields) = NULL
      colOptions = getColOptions()
      
      if (is.null(selectedFields)) 
        return("Select some numeric or factor fields from the selection boxs in the left sidebar.")
      
      colfactor = input$colFactor
      curdf = getSelectedDF()
      
      if (input$describeNumeric == "Mean")
      {
        mycontinuous_fn = describeMean
        mycontinuous_fns = "describeMean"
      }
      else
      {
        mycontinuous_fn = describeMedian
        mycontinuous_fns = "describeMedian"
      }
      
      css.class = paste0("gmisc_table", substr(input$radTableWidth,1,2))
      
      x = table1(curdf, colfactor, selectedFields, colOptions, 
                 add_total_col = input$chkTotals,
                 statistics = input$chkStatistics,
                 NEJMstyle = input$chkNEJM,
                 colN = input$chkColN,
                 caption = input$txtCaption,
                 pos.caption = input$txtCapLoc,
                 tfoot = input$txtFooter, 
                 continuous_fn = mycontinuous_fn,
                 css.class = css.class
      )
      
      observe({
        # hmm, ugly hack but works - a reactive dependency on selectedFields and coloptions
        # this allows to update source if fields changed even when output$table1 not visible
        selectedFields = getSelectedFields()
        colOptions = getColOptions()
        template = paste(readLines("table1.template"), collapse="\n")
        whiskerdata = list(
          add_total_col = input$chkTotals,
          statistics = input$chkStatistics,
          NEJMstyle = input$chkNEJM,
          curdf = input$dataset,
          colfactor = input$colFactor,
          caption = input$txtCaption,
          pos.caption = input$txtCapLoc,
          tfoot = input$txtFooter,
          selectedFields = paste(deparse(selectedFields), collapse=""),
          colOptions = paste(deparse(colOptions), collapse=""),
          continuous_fn = mycontinuous_fns,
          colN = input$chkColN,
          css.class = css.class
        )
        updateAceEditor(session, "acer", value = whisker.render(template, whiskerdata), mode="markdown")
      })
      
      return(x)
    })
    

  
  
}



shinyApp(ui, server)
