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
library(ggpubr)
library(devtools)

data <- read_rds("graph1.rds")
new <- read_rds("c.rds") %>%
  mutate(stance = case_when(stance == "kav_oppose" ~ "Kavanaugh Appointment (Oppose)", 
                            stance == "mu_support" ~ "Robert Mueller's Work/Mandate (Support)",
                            stance == "ice_ab_support" ~ "Abolishment of ICE (Support)",
                            stance == "col_agree" ~ "Trump Colluded with Russia (Agree)",
                            stance == "fem_support" ~ "Feminism (Support)",
                            stance == "sp_support" ~ "Single-Payer Healthcare System (Support)"))

new$stance <- as.factor(new$stance)


ui <- fluidPage(
  
  # Application title
  titlePanel("UpShot/Siena House Race Polling Errors, 2018"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId="choice", label="Select a Summary Statistic:", 
                   choices=c("View frequencies only.", "Show mean.","Show mean, median, density histogram, and density curve.")
                   ),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = .1,
                  max = 5,
                  value = 1.74)
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("1. Understanding Polling Errors:"),
      plotOutput("barPlot"),
      p(""),
      p("From the mean we see that on average, actual Democratic advantage was slightly greater than last-wave polled Democratic advantage in House races. This means that the polls often underestimated Democratic winnability.
        We see that the histogram is very slightly right-skewed (arguably, almost a normal distribution); the median is just to the left of the mean. From this we can posit that there exist a handful of large values where polled Dem. adv. > actual Dem. adv. that are driving the mean slightly upwards.",
        style = "font-size : 10pt"),
      p("")
    )),
  
  
  # Sidebar with a select input 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "x",
        label = "X-Axis: Select Vote Shares from Recent Elections:",
        choices = c("Clinton, 2016" = "clinton16", "Trump, 2016" = "trump16", "Democrats in the House, 2016" = "demhouse16", "Republicans in the House, 2016" = "rephouse16", 
                    "Obama, 2012" = "obama12",
                    "Romney, 2012" = "romney12")
      ),
      
      checkboxInput("bestfit", label = "Show line of best fit.")
      
      ),
    
    # Show a plot of the correlations
    mainPanel(
      h3("2. Polling Errors in Context:"),
      plotOutput("scatterPlot"),
      p(""),
      p("Thus we see no strong correlation between the way people voted in the last House elections, or in the 2016 and 2012 Presidential elections, and polling errors for the 2018 midterms (all R<0.01).
         For example, a higher percentage of votes for Hillary Clinton has no notable correlation to a greater overestimation of Democratic candidates in 2018 House races. Similarly, there is no notable correlation between a higher percentage of votes for Democratic candidates in 2016 House races, and a greater overestimation of Democratic chances in 2018 House races.",
         style = "font-size : 9pt"),
      p("Furthermore, given that the p-values much higher than the commonly accepted significance cut-off of 0.05, we must reject our null hypothesis (that a statistically significant correlation between the x-axis variables and polling error exists), and so we cannot conclude that such a relationship exists.",
        style = "font-size : 10pt"),
      h4("")
    )),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "p_i",
        label = "Select an Ideological/Policy Stance:",
        choices = levels(new$stance)
      ),
      
      checkboxInput("bestfit2", label = "Show line of best fit.")
      
    ),
    
    # Show a plot of the correlations
    mainPanel(
      h3("3. Polling Errors and Policy/Ideology Factors:"),
      plotOutput("scatterPlot2"),
      p("Here we can see some strong and significant correlations between agreement with ideological stances and greater polling error. Opposition to the appointment of Kavanaugh to the Supreme Court, support for feminism, and support for instituting a Single-Payer healthcare system all have notable to strong correlations with the overestimation of democratic advantage in the polls.
        Therefore, the greater the percentage of respondents who expressed support for these stances (often classified as liberal or Democratic stances), the more likely the poll was to think Democrats would do far better than they actually did.",
        style = "font-size : 10pt")
    ))
  

)

# Define server logic required to draw plots
server <- function(input, output) { 
  
  
  output$barPlot <- renderPlot(
    
    if(input$choice == "Show mean, median, density histogram, and density curve.") {
      
      data %>%
        ggplot(aes_string(x = "error")) + geom_histogram(aes(y = ..density.., fill = ..density..), binwidth = input$bins) + 
        geom_density(alpha = .3, color = "darkblue", fill="darkblue") +
        theme_minimal() + 
        ggtitle("Density Distribution of Polling Errors (in %):") +
        labs(subtitle = "Polling error calculated as difference between polled Democratic advantage and actual Democratic advantage.") +
        xlab("Polling Error (in %)") +
        scale_y_continuous(name = "Density") +
        scale_fill_gradient("Density", low = "mediumblue", high = "turquoise2") + 
        geom_vline(aes_string(xintercept = mean(data$error)),col='magenta', size=2) +
        geom_vline(aes_string(xintercept = median(data$error)), col='yellow', size=2) +  
        theme(plot.title = element_text(size = 16, face = "bold"))
      
    }
    
  else {
      if(input$choice == "Show mean.") {
    
        data %>%
      ggplot(aes_string(x = "error")) + geom_histogram(aes(fill = ..count..), binwidth = input$bins, alpha = 0.9) + 
      theme_minimal() + 
      ggtitle("Frequency Distribution of Polling Errors (in %):") +
          labs(subtitle = "Polling error calculated as difference between polled Democratic advantage and actual Democratic advantage.") +
      xlab("Polling Error (in %)") +
          scale_y_continuous(name = "Count") +
      scale_fill_gradient("count",  low = "mediumblue", high = "turquoise2") +
      geom_vline(aes_string(xintercept = mean(data$error)),col='magenta', size=2) +  
          theme(plot.title = element_text(size = 16, face = "bold"))
          
    }
else {
  
  data %>%
    ggplot(aes_string(x = "error")) + geom_histogram(aes(fill = ..count..), binwidth = input$bins) + 
    theme_minimal() +
    ggtitle("Frequency Distribution of Polling Errors (in %):") +
    labs(subtitle = "Polling error calculated as difference between polled Democratic advantage and actual Democratic advantage.") +
    xlab("Polling Error (in %)") +
    scale_y_continuous(name = "Count") +
    scale_fill_gradient("Count", low = "mediumblue", high = "turquoise2") +  
    theme(plot.title = element_text(size = 16, face = "bold"))
  
}
}
    
  )
  
  output$scatterPlot <- renderPlot({
    
    x <- reactive({ 
      
      str_to_title(str_replace_all(input$x, c("clinton16" = "Clinton Vote Share in 2016", 
                                              "trump16" = "Trump Vote Share in 2016",
                                              "demhouse16" = "House Democrats Vote Share in 2016",
                                              "rephouse16" = "House Republicans Vote Share in 2016",
                                              "obama12" = "Obama Vote Share in 2012",
                                              "romney12" = "Romney Vote Share in 2012"))) 
      
    })
    
    
    if(input$bestfit == TRUE) {
      
      data %>%
        ggplot(aes_string(input$x, "error", color = "state")) + 
        geom_jitter(size = 3, alpha = 0.85) + 
        geom_smooth(aes_string(input$x, "error"), method = "lm", inherit.aes = F) +
        stat_cor(aes_string(input$x, "error"), method = "pearson", label.x = 50, label.y = 3, show.legend = NA,
                 inherit.aes = FALSE, geom = "text") +
        xlab(x()) + ylab("Polling Error (in %)") + 
        ggtitle("Correlation between 2018 Midterm Polling Errors and Voting Trends in Previous Elections:") + 
        labs(subtitle = "Polling error calculated as difference between polled Democratic advantage and actual Democratic advantage.") + 
        theme_linedraw() + labs(color = "State") +  
        theme(plot.title = element_text(size = 16, face = "bold"))
      
    }
    
    else {
      
      data %>%
        ggplot(aes_string(input$x, "error", color = "state")) + geom_jitter(size = 3, alpha = 0.85) +
        xlab(x()) + ylab("Polling Error (in %)") + 
        ggtitle("Correlation between 2018 Midterm Polling Errors and Voting Trends in Previous Elections:") + 
        labs(subtitle = "Polling error calculated as difference between polled Democratic advantage and actual Democratic advantage.") + 
        theme_linedraw() + labs(color = "State") +  
        theme(plot.title = element_text(size = 16, face = "bold"))
      
      
    }
    
  })
  
  output$scatterPlot2 <- renderPlot({
    
if (input$bestfit2 == TRUE) {
    new %>%
      filter(stance == input$p_i) %>%
      ggplot(aes_string("count", "error", color = "district")) + geom_point(size = 3) +
      xlab("No. of Respondents That Agree (in %)") + ylab("Polling Error (in %)") +
     geom_smooth(aes_string("count", "error"), method = "lm", inherit.aes = F) +
      stat_cor(aes_string("count", "error"), method = "pearson", show.legend = NA,
             inherit.aes = FALSE, geom = "text") +
      ggtitle("Correlation between 2018 Midterm Polling Errors and Policy/Ideological Positions Held by Poll Respondents:") + 
      labs(subtitle = "Polling error calculated as difference between polled Democratic advantage and actual Democratic advantage.") + 
      theme_linedraw() + labs(color = "District") +  
      theme(plot.title = element_text(size = 16, face = "bold"))
      
}
    
    else {
      
      new %>%
        filter(stance == input$p_i) %>%
        ggplot(aes_string("count", "error", color = "district")) + geom_point(size = 3) +
        xlab("No. of Respondents That Agree (in %)") + ylab("Polling Error (in %)") +
        ggtitle("Correlation between 2018 Midterm Polling Errors and Policy/Ideological Positions Held by Poll Respondents:") + 
        labs(subtitle = "Polling error calculated as difference between polled Democratic advantage and actual Democratic advantage.") + 
        theme_linedraw() + labs(color = "District") +  
        theme(plot.title = element_text(size = 16, face = "bold"))
      
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

