#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(ggplot2)

data <- read_rds("graph1.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Political Landscape Survey, Summer 2017"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "x",
        label = "X-Axis: Select Vote Shares from Recent Elections:",
        choices = c("Clinton, 2016" = "clinton16", "Trump, 2016" = "trump16", "Democrats in the House, 2016" = "demhouse16", "Republicans in the House, 2016" = "rephouse16", 
                    "Obama, 2012" = "obama12",
                    "Romney, 2012" = "romney12")
      )
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  
  output$scatterPlot <- renderPlot({
    
    x <- reactive({ 
      
      str_to_title(str_replace_all(input$x, c("clinton16" = "Clinton Vote Share in 2016", 
                                             "trump16" = "Trump Vote Share in 2016",
                                             "demhouse16" = "House Democrats Vote Share in 2016",
                                             "rephouse16" = "House Republicans Vote Share in 2016",
                                             "obama12" = "Obama Vote Share in 2012",
                                             "romney12" = "Romney Vote Share in 2012"))) 
      
      })
    
    
    
    data %>%
      ggplot(aes_string(input$x, "error", color = "state")) + 
      geom_point() + geom_smooth(aes_string(input$x, "error"), method = "lm", inherit.aes = F) +
      xlab(x()) + ylab("Polling Error (in %)") + 
      ggtitle("Placeholder:") + 
      labs(subtitle = "Placeholder") + 
      theme_minimal() + labs(color = "State")
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

