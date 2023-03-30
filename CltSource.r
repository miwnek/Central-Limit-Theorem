library(ggplot2)
library(maps)
library(shiny)
library(shinythemes)
library(comprehenr)

# Values in each distribution
n <- 10000

# Defining UI
ui <- fluidPage(theme = shinytheme("lumen"),
          titlePanel("Central Limit Theorem"),
          sidebarLayout(
            sidebarPanel(
              selectInput(inputId = "distribution", label = strong("Pick a distribution for the dataset:"),
                          choices = c("Uniform", "Exponential", "Beta"),
                          selected = 0),
              numericInput("sample_size", label = strong("Sample size"), value = 1000, min = 1, step = 1),
              numericInput("to_average", label = strong("Averages of how many values:"), value = 100, min = 1, step = 1),
              checkboxInput("will_regen" , label = strong("Check to generate a new random dataset with each change:"), value = FALSE)
              
          ),
          
          mainPanel(
            plotOutput(outputId = "fstplot",),
            plotOutput(outputId = "avgplot",),
          )
        )
      )

# Define server function
server <- function(input, output, session) {
  unif = as.array(runif(n, min = 0, max = 1))
  exp = rexp(n, 10)
  beta = rbeta(n, 0.5, 0.5)
  curr <- reactiveVal(unif)
  
  
  df <- reactive({
    # Re-calculate output for new input values
    if(input$will_regen){chosen <- switch(input$distribution, "Uniform" = runif(n, min = 0, max = 1), "Exponential" = rexp(n, 2), "Beta" = rbeta(n, 0.5, 0.5))}
    else {chosen <- switch(input$distribution, "Uniform" = unif, "Exponential" = exp, "Beta" = beta)}
    averages <- to_vec(for (i in 1 : input$sample_size) mean(sample(chosen, input$to_average, replace = TRUE)))
    curr(chosen)
    df <- data.frame(weight = averages)
  })
  
  # Change origninal distribution according to input values
  dff <- reactive({
    dff <- data.frame(wei  = curr())
  })
  
  
  # Plot original distribution
  output$fstplot <- renderPlot({
    color = "#434343"
    
    ggplot(dff(), aes(x = wei)) + geom_histogram(aes(y = ..density..), na.rm = TRUE, binwidth = 0.01) + geom_density(alpha=.2, fill="#FF6666") + ggtitle(paste("Original distribution - ", input$distribution, ":")) + xlab("x")
  })
  
  # Plot output
  output$avgplot <- renderPlot({
    color = "#434343"
      
    ggplot(df(), aes(x=weight)) + geom_histogram(aes(y = ..density..), na.rm = TRUE, binwidth = 0.01) + geom_density(alpha=.2, fill="#FF6666") + xlim(0,1) + ggtitle("Output: ") + xlab("x")
  })
  
}

            
shinyApp(ui = ui, server = server)   


          

