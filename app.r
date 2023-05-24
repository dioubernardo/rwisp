
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
  
  p(tableOutput("ui")),
  p(tableOutput("normalizedData")),
  p(tableOutput("utilities")),
  
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
  observeEvent(input$do, {
    tryCatch({
      output$errors <- NULL
      output$result <- NULL
      
      if (is.null(input$file))
        stop("Select a file")
      
      result <- rwispfromcsv(input$file$datapath)
      
      colnames(result$ui) <- c('Position', 'ui')
      output$ui <- renderTable(
        result$ui,
        rownames = TRUE,
        digits = 5,
        caption = "Ranking Result",
        caption.placement = getOption("xtable.caption.placement", "top")
      )
      
      output$normalizedData <-
        renderTable(
          result$normalizedData,
          rownames = TRUE,
          digits = 5,
          caption = "Normalized Data",
          caption.placement = getOption("xtable.caption.placement", "top")
        )
      
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
        renderTable(
          result$utilities,
          rownames = TRUE,
          digits = -5,
          caption = "Utility Matrix",
          caption.placement = getOption("xtable.caption.placement", "top")
        )
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
