#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Starting a project without know what you want is bad.
#   Let's put together some goals for the UT OCC data
#     1. An app that Helps Someone choose a career based on:
#       a. availability
#          i. TOT_POP
#       b. salary
#       c. earnings certainty
#     2. Let's give them a tool that helps find the right job
#       for them

#Load the new data
load(file = 'utah.Rdata' )
load(file = 'industry.Rdata')
load(file = 'majors.Rdata')
load(file = 'idx_majors.Rdata')


# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

library(shiny)
library(shinythemes)
library(ggplot2)

library(shiny)

dataset <- diamonds

# Define UI for application that draws a histogram
ui <- fluidPage( theme  = shinytheme('flatly'),
  navbarPage("Utah Occupation",
    tabPanel("Plot", icon = icon("bar-chart-o"),
             # Page title
             titlePanel("Pareto Charts by Industry"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "display", 
                   "Information to Display:",
                   c('Mean','Median','Total Employ','Wage Spread')
                 ),
                 selectInput(
                   "sector", 
                   "Choose Sector of business:",
                   unique(majors$title_name)
                 ),
                 sliderInput("tops",
                             "Top # of Careers to Display:",
                             min = 5,
                             max = 25,
                             value = 10)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("mainPlot"),
                 h3(textOutput("codes"))
               )
             )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$mainPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    selected_ind <- majors$industry[which(majors$title_name == input$sector)]
    if (selected_ind == '00'){
      x <- industry
    } else {
      x <- industry[which(industry$industry == selected_ind),] 
    }
    if ( input$display == 'Mean') {
      x <- x[order(x$annualwage, decreasing = TRUE),]
      x <- x[which(!is.na(x$annualwage)),]
      x <- x[1:input$tops,]
      nms <- wrap.labels(x$title_name, 25)
      plt <- barplot(x$annualwage, main= 'Mean Salary', xaxt = 'n')
      text(plt, par("usr")[3]-.05*(par("usr")[4]-par("usr")[3]),labels = nms, xpd=TRUE, srt=45, pos=2, adj = c(1.1,1.1), cex = .6)
    } else if (  input$display == 'Median') {
      x <- x[order(x$wage_Q2, decreasing = TRUE),]
      x <- x[which(!is.na(x$wage_Q2)),]
      nms <- wrap.labels(x$title_name, 25)
      plt <- barplot(x$wage_Q2, main= 'Median Salary', xaxt = 'n')
      text(plt, par("usr")[3]-.05*(par("usr")[4]-par("usr")[3]),labels = nms, xpd=TRUE, srt=45, pos=2, adj = c(1.1,1.1), cex = .6)
    } else if (  input$display == 'Total Employ') {
      x <- x[order(x$total_empl, decreasing = TRUE),]
      x <- x[which(!is.na(x$total_empl)),]
      nms <- wrap.labels(x$title_name, 25)
      plt <- barplot(x$total_empl, main= 'Total Employment', xaxt = 'n')
      text(plt, par("usr")[3]-.05*(par("usr")[4]-par("usr")[3]),labels = nms, xpd=TRUE, srt=45, pos=2, adj = c(1.1,1.1), cex = .6)
    } else if (input$display == 'Wage Spread') {
      x <- x[order(x$centered_D9, decreasing = TRUE),]
      x <- x[which(!is.na(x$centered_D9)),]
      nms <- wrap.labels(x$title_name, 25)
      plt <- barplot(x$centered_D9, main= 'Reward Above Median: Top 90%', xaxt = 'n')
      text(plt, par("usr")[3]-.05*(par("usr")[4]-par("usr")[3]),labels = nms, xpd=TRUE, srt=45, pos=2, adj = c(1.1,1.1), cex = .6)
    }
  })
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  output$codes <- renderText({
    paste(majors$industry[which(majors$title_name == input$sector)],", ", input$sector)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
