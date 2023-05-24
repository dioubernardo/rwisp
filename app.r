
library(shiny)

source("rwisp.r", local = TRUE)

ui <- fluidPage(
  titlePanel("WISP Calculator"),
  
  p(
    "Select the datasheet to be processed. It MUST be in CSV format, remember not to use a thousand separator.",
    "If in doubt, ",
    downloadLink("downloadData", "download the example file"),
    "."
  ),
  
  fileInput(
    "file",
    "Select data file",
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
    "This implementation is available at ",
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
      output$ui <- renderTable(result$ui,
                               rownames = TRUE,
                               digits = 5)
      
      output$normalizedData <-
        renderTable(result$normalizedData,
                    rownames = TRUE,
                    digits = 5)
      
      colnames(result$utilities) <-
        c('uiwsd',
          'uiwpd',
          'uiwsr',
          'uiwpr',
          'üiwsd',
          'üiwpd',
          'üiwsr',
          'üiwpr')
      output$utilities <-
        renderTable(result$utilities,
                    rownames = TRUE,
                    digits = -5)
      
      output$calculated <- reactive(1)
    },
    error = function(err) {
      output$errors <- renderText(geterrmessage())
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = "test.csv",
    content = function(con) {
      file.copy("test.csv", con)
    }
  )
  
}

shinyApp(ui, server)
