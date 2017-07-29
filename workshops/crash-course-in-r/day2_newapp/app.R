#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)


setwd("/home/alton/Desktop/new_app/")

faith2 = read.csv("newapp/faith2.csv")

faith2[,2]
faith2$waiting_num <- as.numeric(as.character(faith2$waiting))

select(faith2,waiting,waiting_num)

faith2 %>% select(waiting,waiting_num) %>% arrange(waiting_num) %>% head

faith2$

faith2_grouped <- group_by(faith2, new_cat_col)
tmp <- summarise(faith2_grouped,
                   count = n(),
                   avg_waiting = mean(waiting_num, na.rm = TRUE))






cat_options = levels(faith2$new_cat_col)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         checkboxGroupInput("cat_var", 
                            "Cats to show:",
                            cat_options, 
                            "thistime")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         dataTableOutput('table')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      x    <- faith2[ 
                      ( 
                        !is.na(faith2$waiting_num)
                        & faith2$new_cat_col %in% input$cat_var
                      )
                        , "waiting_num"]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      #ggplot(faith2, aes(x = eruptions)) +
      #  geom_histogram()
       ggplot(faith2, aes(x=eruptions, fill=new_cat_col)) +
        geom_histogram(binwidth=.5, alpha=.5, position="identity")
   })
   
   output$table <- renderDataTable({
     
         x    <- faith2[ 
           ( 
             !is.na(faith2$waiting_num)
             & faith2$new_cat_col %in% input$cat_var
           )
           , ]
         
         x
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

