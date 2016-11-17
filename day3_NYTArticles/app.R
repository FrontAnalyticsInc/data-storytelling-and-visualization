#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NYT Articles"),
   textInput("caption", "Caption", "Data Summary"),
   verbatimTextOutput("value"),
   plotOutput('plot_xy'),
   dataTableOutput('table')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$value <- renderText({ input$caption })
  
  
  all_articles <- reactive({
    
    article_key <- "&api-key=b75da00e12d54774a2d362adddcc9bef"
    url <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",input$caption)
    req <- fromJSON(paste0(url, article_key))
    articles <- req$response$docs
    
    all_articles = cbind( articles$headline$main,
                          articles[, c("print_page", "word_count")] ) 
    all_articles
  })
  
  output$table <- renderDataTable({
    all_articles()
    })
  
  output$plot_xy <- renderPlot({
    all_articles = all_articles()
    plot(all_articles$print_page, all_articles$word_count)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

