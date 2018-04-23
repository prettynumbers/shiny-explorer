require(knitr)
require(brew)
library(tabplot)  # devtools::install_github("mtennekes/tabplot", subdir = "pkg")
library(ggplot2)
library(readxl)
library(sqldf)
library(DT)
library(sparkline)

# Setting global environment
# Reset the maximum upload file size
options(shiny.maxRequestSize = 10000 * 1024 ^ 2)

cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  
  ##============================= Data Input ===================================
  # File importing handler for excel file.
  observeEvent(input$importFile, {
    inFile <- input$importFile
    if(is.null(inFile)) { return(NULL) }
    
    # hack for readxl no extension open issue #85
    file.rename(inFile$datapath, paste0(inFile$datapath, ".xlsx"))
    xlsfile = paste0(inFile$datapath, ".xlsx")
    updateSelectInput(session, "excelsheets", choices = excel_sheets(xlsfile))
  })
  
  observeEvent(input$assignxls, {
    xlsfile = paste0(input$importFile$datapath, ".xlsx")
    x = read_excel(xlsfile, input$excelsheets)
    
    # clean up 1 - remove NA cols
    numna = sum(is.na(colnames(x)))
    newnames = paste0("NACOL", 1:numna)
    colnames(x)[is.na(colnames(x))] = newnames
    
    # clean up 2 - convert spaces to dots
    names(x) <- sub(" ", ".", names(x))
    
    assign(input$xlsdataframe, x, envir = .GlobalEnv)
    updateSelectInput(session, "dataset", "Dataframe:", choices = getDataFrames(), selected = getDataFrames()[1])
  })
  
  observeEvent(input$assigncsv, {
    if(is.null(input$importCsvFile)) { return(NULL) }
    x = read.csv.sql(input$importCsvFile$datapath, header = input$header, sep = input$sep,
                     sql = paste0("select * from file order by random() limit ", input$sampleSize))
    
    # clean up 1 - remove NA cols
    numna = sum(is.na(colnames(x)))
    newnames = paste0("NACOL", 1:numna)
    colnames(x)[is.na(colnames(x))] = newnames
    
    # clean up 2 - convert spaces to dots
    names(x) <- sub(" ", ".", names(x))
    
    assign(input$csvdataframe, x, envir = .GlobalEnv)
    updateSelectInput(session, "dataset", "Dataframe:", choices = getDataFrames(), selected = getDataFrames()[1])
  })
  
  getSelectedDF <- reactive({
    eval(parse(text = input$dataset))
  })
  
  # ===========================Update Variables List ===========================
  # when dataset changed populate the summaries and variable selection dropdowns
  observeEvent(input$dataset, {
    dfinfo = getdfinfo(input$dataset)
    
    # Update the field selects
    updateSelectInput(session, "numerics", choices = getNumerics(input$dataset))
    updateSelectInput(session, "factors", choices = getFactors(input$dataset))
    updateSelectInput(session, "dates", choices = getDates(input$dataset))
    updateSelectInput(session, "logicals", choices = dfinfo$logicals$Variable)
    
    # Populate the summary tab
    ## Dimensions
    if (length(dfinfo$numerics$Variable) + length(dfinfo$factors$Variable) + 
        length(dfinfo$logicals$Variable) + length(dfinfo$dates$Variable) == 0) {
      output$dimensions = renderText({"The dataframe is empty."})
    } else {
      output$dimensions = renderText({
        paste0("Observations = ", dfinfo$dimensions[1], "; ", "Variables = ", dfinfo$dimensions[2])
      })
    }
    
    ## Numerics
    # if(identical(dfinfo$numerics$Variable, character(0))) {
    if (length(dfinfo$numerics$Variable) == 0) {
      output$numericInfo = renderText({"There are no numeric fields"})
    } else {
      output$numericInfo = renderDT(
        as.data.frame(dfinfo$numerics, stringsAsFactors = FALSE),
        rownames = FALSE,
        escape = FALSE,
        options = list(drawCallback = cb)
      )
    }
    
    ## Factors
    # if (identical(dfinfo$factors$Variable, character(0))) {
    if (length(dfinfo$factors$Variable) == 0) {
      output$factorInfo = renderText({"There are no factor fields"})
    } else {
      output$factorInfo = renderDT(
        as.data.frame(dfinfo$factors, stringsAsFactors = FALSE),
        rownames = FALSE,
        escape = FALSE,
        options = list(drawCallback = cb)
      )
    }
    
    ## Dates
    # if (identical(dfinfo$dates$Variable, character(0))) {
    if (length(dfinfo$dates$Variable) == 0) {
      output$dateInfo = renderText({"There are no date fields"})
    } else {
      output$dateInfo = renderDT(
        as.data.frame(dfinfo$dates, stringsAsFactors = FALSE),
        rownames = FALSE,
        escape = FALSE
      )
    }
    
    ## Logicals
    # if (identical(dfinfo$logicals$Variable, character(0))) {
    if (length(dfinfo$logicals$Variable) == 0) {
      output$logicalInfo = renderText({"There are no logical fields"})
    } else {
      output$logicalInfo = renderDT(
        as.data.frame(dfinfo$logicals, stringsAsFactors = FALSE),
        colnames = c('% TRUE' = 'PercentTrue'),
        rownames = FALSE,
        escape = FALSE
      )
    }
  })
  
  observeEvent(input$deleteSelections, {
    # clear the selected fields
    dfinfo = getdfinfo(input$dataset)
    updateSelectInput(session, "numerics", choices = getNumerics(input$dataset))
    updateSelectInput(session, "factors", choices = getFactors(input$dataset))
    updateSelectInput(session, "dates", choices = getDates(input$dataset))
    updateSelectInput(session, "logicals", choices = dfinfo$logicals$Variable)
  })
  
  observeEvent(input$go, {
    # show the Analysis tab panel 
    updateTabsetPanel(session, "mainPanelTabset", selected = "Analysis")  
  })
  
  getAnalysis = eventReactive(input$go, {
    # Load the selected variables and dataframe
    #TODO refactor the get selected vars code - need to fix selectizeInput captioning
    rmdsub = "Error: There is no report template for this combination of selected fields."
    numerics = as.vector(sapply(input$numerics, function(x) { strsplit(x, "/")[[1]][1] })) # ugly hack to strip field info from selectizeInput
    factors = as.vector(sapply(input$factors, function(x) { strsplit(x, "/")[[1]][1] })) 
    dates = as.vector(sapply(input$dates, function(x) { strsplit(x, "/")[[1]][1] })) 
    logicals = input$logicals
    dfstr = input$dataset
    
    if ((length(numerics) + length(factors) + length(dates) + length(logicals)) == 0)
      output$analysis = return("Select some fields first")
    
    progress = shiny::Progress$new(session, min = 1, max = 3)
    on.exit(progress$close())
    progress$set(message = "Building report")
    
    # could simplify following by using knit_expand and {{mydf}}${{numeric1}} etc in templates
    # but this makes the templates hard to read and write
    
    if ((length(numerics) == 0) & (length(factors) == 1)) {
      rmdsource = paste(readLines("templates/factor-1.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, factor1 = factors[1]))
    }
    
    if ((length(numerics) == 0) & (length(factors) == 2)) {
      rmdsource = paste(readLines("templates/factor-2.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, factor1 = factors[1], factor2 = factors[2]))
    }
    
    if ((length(numerics) == 1) & (length(factors) == 0)) {
      rmdsource = paste(readLines("templates/numeric-1.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, numeric1 = numerics[1]))
    }
    
    if ((length(numerics) == 1) & (length(factors) == 1)) {
      rmdsource = paste(readLines("templates/numeric-1-factor-1.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, numeric1 = numerics[1], factor1 = factors[1]))
    }
    
    if ((length(numerics) == 1) & (length(factors) == 2)) {
      rmdsource = paste(readLines("templates/numeric1factor2.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, numeric1 = numerics[1], factor1 = factors[1], factor2 = factors[2]))
    }
    
    if ((length(numerics) == 2) & (length(factors) == 0)) {
      rmdsource = paste(readLines("templates/numeric-2.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, numeric1 = numerics[1], numeric2 = numerics[2]))
    }
    
    if ((length(numerics) > 2) & (length(factors) == 0)) {
      rmdsource = paste(readLines("templates/numeric-gt2.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, numericlist = paste0('c("', paste(numerics, collapse='", "'), '")'))) 
    }
    
    if ((length(numerics) == 2) & (length(factors) == 1)) {
      rmdsource = paste(readLines("templates/numeric-2-factor-1.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, numeric1 = numerics[1], numeric2 = numerics[2], factor1 = factors[1]))
    } 
    
    if ((length(numerics) > 2) & (length(factors) == 1)) {
      rmdsource = paste(readLines("templates/numeric-gt2-factor-1.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, numericlist = paste0('c("', paste(numerics, collapse='", "'), '")'), factor1 = factors[1])) 
    }
    
    if ((length(logicals) == 1) & (length(factors) > 0)) {
      rmdsource = paste(readLines("templates/logical-1.Rmd"), collapse = "\n")
      rmdsub = dkReplace(rmdsource, c(mydf = dfstr, logical1 = logicals[1], factorlist = paste0('c("', paste(factors, collapse='", "'), '")')))
    }
    
    # Some templates have conditional segements - hence use brew 
    # TODO: explore using whiskers instead of dkReplace/brew
    progress$inc()
    brewout = capture.output(brew(text = rmdsub))
    
    updateAceEditor(session, "acermd", mode = "markdown", value = paste(brewout, collapse = "\n"))
    
    # EXPERIMENT: rmarkdown version
    # Outcome: no particular advantage, require extra files and highlighting js = not used
#     writeLines(brewout, con=file("temp.Rmd"))
#     library(rmarkdown)
#     render("temp.Rmd", html_fragment(toc=T))
#     myhtml = paste(readLines("temp.html"), collapse="\n")
    
    if (input$chkggtheme) {
      theme_set(theme_classic())
    }
    else
      theme_set(theme_gray())
    
    render_html()  # this sets hooks to use highr
    myhtml = paste(  #paste(readLines("templates/navbar.rms"), collapse="\n"),
      try(knit2html(text = brewout, stylesheet = "", fragment.only = TRUE)),
      "<script>
            // javascript highlighting not needed if using render_html()
            // $('#analysis pre code').each(function(i, e) {hljs.highlightBlock(e)});
            
            // Insert a TOC after the h1 title
            $('#analysis h1').after('<div id=\"toc\"></div>')
            generateTOC($('#toc')[0], $('#analysis')[0]);
        </script>", 
      sep = '\n')
    
    progress$inc()

    session$onFlushed(function() {
      updateTabsetPanel(session, "mainPanelTabset", selected = "Analysis") 
    })

    return(myhtml)
    
  })

  output$analysis = renderText({
    getAnalysis()
  })
  
  output$mydt = DT::renderDT(
    { getSelectedDF() }, 
    options = list(
      lengthMenu = c(5, 10, 25), 
      pageLength = 10,
      scrollX = TRUE
    )
  )
  
  output$mytabplot = renderPlot({
    if (input$limittabplot) {
      #TODO refactor the get selected vars code - need to fix selectizeInput captioning
      numerics = as.vector(sapply(input$numerics, function(x) { strsplit(x, "/")[[1]][1] })) # ugly hack to strip field info from selectizeInput
      factors = as.vector(sapply(input$factors, function(x) { strsplit(x, "/")[[1]][1] })) 
      dates = as.vector(sapply(input$dates, function(x) { strsplit(x, "/")[[1]][1] })) 
      logicals = input$logicals
      vars = unlist(c(numerics, factors, dates, logicals))
      if (length(vars) > 0)
        tableplot(getSelectedDF()[, unlist(c(numerics, factors, dates, logicals))])
    }
    else
      tableplot(getSelectedDF())
  })
  
  output$pivotTable = renderRpivotTable({
    rpivotTable(data=getSelectedDF()) #, onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }"))
  })

})