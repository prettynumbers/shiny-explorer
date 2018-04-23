source("R/dkdfinfo.r")
source("R/dkgraph.r")
source("R/dkutils.r")

# to run
# shiny:::runApp("../shiny-explorer")
# shiny:::runApp("../shiny-explorer", launch.browser = rstudio::viewer)

# a very ugly hack as can't send more data than character vectors to selectizeInput therefore need to encode the field info in the label and extract it
# NB: also need to strip the field info when
selectizeRenderStr = "
  {
    item:function(item,escape) {
      return '<div><span class=\"selectize-numitem\">' + escape((item.value).split('/')[0]) + '</span></div>'; 
    },
    option:function(item,escape) {
      return '<div><span class=\"selectize-name\">' + escape((item.label).split('/')[0]) + '</span><span class=\"selectize-caption\">' + escape((item.label).split('/')[1]) + '</span></div>'; 
    }
  }"

# for testing logistic regression
#mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

# Define UI for dataset viewer application
shinyUI(navbarPage("Shiny Explorer", position = "fixed-top",
  tabPanel("Explorer", icon = icon("list"),
    sidebarLayout(
      sidebarPanel(
        
        # header includes
        includeCSS("www/css/dkknitr.css"),
        includeCSS("www/css/hover.css"),
        
        includeScript("www/js/jquery-ui-1.10.3.custom.min.js"),
        includeScript("www/js/jquery.sparkline.min.js"),
        includeScript("www/js/highlight.pack.js"),
        includeScript("www/js/toc.js"),
        
        jsCodeHandler(), # for sending custom JS code to execute
        tags$style(type="text/css", "body {padding-top: 70px;}"), # stop fixed-top navbar from overlay body content
        
        h3("Variable Selection"),
        
        wellPanel(
          selectInput("dataset", "Dataframe:", choices = getDataFrames()),
          p(helpText("Choose the desired fields in the dropdowns",
                     "and click Analyse to show an analysis."))
        ),
        
        accordion("fieldsAccordion", 
          accordionPanel("Numeric Variables", 
            selectizeInput("numerics", label = "", choices = NULL, selected = "", multiple = T, #NB: choices is filled by observing input$dataset
              options = list(placeholder = "Select numeric variable(s)", dropdownParent = "body", plugins = list(remove_button = "", drag_drop = ""), 
                labelField = "label", render = I(selectizeRenderStr))), expanded = T),
          accordionPanel("Factor Variables", 
            selectizeInput("factors", label = "", choices = NULL, selected = "", multiple = T,
              options = list(placeholder = "Select factor variable(s)", dropdownParent = "body", plugins = list(remove_button = "", drag_drop = ""),
                labelField = "label", render = I(selectizeRenderStr)))),
          accordionPanel("Date Variables", 
            selectizeInput("dates", label = "", choices = NULL, selected = "", multiple = T, 
              options = list(placeholder = "Select date variable(s)", dropdownParent = "body", plugins = list(remove_button = "", drag_drop = ""),
                labelField = "label", render = I(selectizeRenderStr)))),
          accordionPanel("Logical Variables", 
            selectizeInput("logicals", label = "", choices = NULL, selected = "", multiple = T,
              options = list(placeholder = "Select logical variable(s)", dropdownParent = "body", plugins = list(remove_button = "", drag_drop = ""))))
        ),
        
        accordion("optionsAccordion",
          accordionPanel("Options",
            checkboxInput("chkggtheme", "Classic ggplots theme"))),
        
        p(
          # use actionButton rather than submitButton so that changing the dataframe dropdown automatically updates the field selects
          actionButton("go", strong("Analyse"), class = "hvr-icon-spin"), #icon("play")), 
          actionButton("deleteSelections", "Clear Selections", class = "hvr-icon-sink-away") #icon("trash-o"))
        )
        
      ), # sidebarPanel
      
      mainPanel(
        
        tabsetPanel(id = "mainPanelTabset",
          
          tabPanel("Variables",  #tabsetPanel(id="summaryTabset",
                   htmlwidgets::getDependency('sparkline'),
                   h4("Dimensions"),
                   textOutput("dimensions"),
                   h4("Numeric Variables"),
                   DT::dataTableOutput("numericInfo"),
                   h4("Factor Variables"),
                   DT::dataTableOutput("factorInfo"),
                   h4("Date Variables"),
                   DT::dataTableOutput("dateInfo"),
                   h4("Logical Variables"),
                   DT::dataTableOutput("logicalInfo")
                   # ,plotOutput("tabplot")
          ),
          
          navbarMenu("Data",
                     # tabPanel("TabPlot",
                     #          checkboxInput("limittabplot", label = "Show selected variables only"),
                     #          plotOutput("mytabplot")
                     # ),
                     tabPanel("Table", dataTableOutput("mydt")
                     ),
                     tabPanel("PivotTable", rpivotTableOutput("pivotTable"))
          ),
          tabPanel("Analysis", 
                   htmlOutput("analysis")
          ),
          tabPanel("Source",
                   aceEditor("acermd", mode = "markdown"))
        )
      ) # mainPanel
      
    ) # sidebarLayout
  ), # tabPanel(Explorer)
  
  navbarMenu("Import", icon = icon("upload"),
    tabPanel("Excel", icon = icon("file-excel-o"),
      sidebarLayout(
        sidebarPanel(   
          h3("Data Import"),
          wellPanel(
            h4("Excel .xls/.xlsx:"),
            tags$hr(),
            fileInput('importFile', label = NULL, accept = c('.xls','.xlsx')),
            selectInput("excelsheets", "Sheet:", choices = c()),
            textInput("xlsdataframe", "DataFrame Name:", "myxlsdf"),
            actionButton("assignxls", "Assign to DF")
          )
        ),
        mainPanel()
      )
    ),
    tabPanel("CSV", icon = icon("table"),
      sidebarLayout(
       sidebarPanel(   
         h3("Data Import"),
         wellPanel(
           h4("Import Parameters"),
           radioButtons('sep', 'Separator',
                        c(Comma = ',',
                          Semicolon = ';',
                          Tab = '\t'),
                        ','),
           numericInput('sampleSize', 'Numer of Samples', value = 10000),
           h4("CSV:"),
           tags$hr(),
           fileInput('importCsvFile', label = NULL, accept = c('.csv')),
           textInput("csvdataframe", "DataFrame Name:", "mycsvdf"),
           actionButton("assigncsv", "Assign to DF")
         )
       ),
       mainPanel()
      )
    )
  ) # navbarMenu(Import)
  # ,
  # navbarMenu("Tests", icon = icon("bar-chart"),
  #   tabPanel("2 Sample"),
  #   tabPanel("Correlation")
  # ) # navbarMenu(Tests)
))