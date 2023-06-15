
library(shiny)

source("./R/rwisp.r", local = TRUE)

ui <- fluidPage(
  
  titlePanel('WISP Calculator'),  
  
  p(
    "Select the spreadsheet to be processed. The file MUST be in CSV format, and you should remember not to use a thousand separator.",
    br(),
    "If you have any doubts, ",
    downloadLink("downloadData", "download the example file"),
    "."
  ),
  
  fileInput(
    "file",
    "Select spreadsheet",
    buttonLabel = "Browse...",
    accept = c("text/csv",
               "text/comma-separated-values,text/plain",
               ".csv")
  ),
  
  actionButton("do", "Resolve"),
  
  p(verbatimTextOutput("errors")),
  
  conditionalPanel(
    condition = "output.calculated==1",
    p(h3("Ranking Result"), tableOutput("ui")),
    p(h3("Normalized Data"), tableOutput("normalizedData")),
    p(h3("Utility Matrix"), tableOutput("utilities"))
  ),
  
  helpText(
    "This implementation follows the article DOI 10.1109/TEM.2021.3075783 however, the set of equations used in step 3 is that of the article DOI 10.3390/axioms10040347, following the recommendations of Professor Dragisa Stanujkić.",
    br(),
    "The code is open at",
    a(
      href = "https://github.com/dioubernardo/rwisp/",
      "https://github.com/dioubernardo/rwisp/",
      download = NA,
      target = "_blank"
    )
  )
)

server <- function(input, output, session) {
  output$calculated <- reactive(0)
  outputOptions(output, "calculated", suspendWhenHidden = FALSE)
  
  observeEvent(input$do, {
    tryCatch({
      output$errors <- NULL
      output$calculated <- reactive(0)
      
      if (is.null(input$file))
        stop("Select a file")
      
      result <- rwispfromcsv(input$file$datapath)
      
      colnames(result$ui) <- c('Position', 'ui')
      
      output$ui <- renderTable({
        result$ui[,2] <- formatC(result$ui[,2], digits = 2)
        result$ui[,1] <- formatC(result$ui[,1], digits = 0)
        result$ui
      }, rownames = TRUE, align = 'lrr')
      
      output$normalizedData <-
        renderTable(result$normalizedData,
                    rownames = TRUE,
                    digits = 3)
      
      colnames(result$utilities) <-
        c('uiwsd',
          'uiwpd',
          'uiwsr',
          'uiwpr',
          'ūiwsd',
          'ūiwpd',
          'ūiwsr',
          'ūiwpr')
      output$utilities <-
        renderTable({
          result$utilities[,] <- formatC(result$utilities[,], digits = 4)
          result$utilities
        }, rownames = TRUE, align = 'lrrrrrrrr')
      
      output$calculated <- reactive(1)
    },
    error = function(err) {
      output$errors <- renderText(geterrmessage())
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = "test.csv",
    content = function(con) {
      file.copy("tests/test.csv", con)
    }
  )
  
  observeEvent(input$selected_language, {
    update_lang(input$selected_language)
  })
  
}

shinyApp(ui, server)
