
library(shiny)

source("rwisp.r", local = TRUE)

ui <- fluidPage(
  titlePanel("WISP Calculator"),
  
  p(
    "Select the datasheet to be processed. It MUST be in CSV format, using a COMMA as a separator, remember not to use a thousand separator.",
    "If in doubt, ",
    downloadLink("downloadData", "download the example file"),
    "."
  ),
  
  fileInput("file", "Select data file", accept = c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    ".csv")),
  
  actionButton("do", "Resolve"),
  
  p(verbatimTextOutput("errors")),
  p(tableOutput("result")),
  
  helpText(
    "This implementation is available at ",
    a(href="https://github.com/dioubernardo/rwisp/", "https://github.com/dioubernardo/rwisp/", download=NA, target="_blank")
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
      colnames(result) <- c('Alternatives','Global Ui')
      output$result <- renderTable(result)
    },
    error = function(err) {
      output$errors <- renderText(geterrmessage())
    })
  })
  
  output$downloadData <- downloadHandler(
    filename="test.csv", 
    content=function(con) {
      file.copy("test.csv", con)
    }
  )
  
}

shinyApp(ui, server)

