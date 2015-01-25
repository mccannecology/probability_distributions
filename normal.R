server <- function(input,output){
  
  ####################################### 
  # These are the reactive calculations # These do not get displayed 
  ####################################### 
  
  rand_samp <- reactive({
    rnorm(input$obs, input$mean, input$sd)
  })
  
  # calculate some stuff needed to define the quantiles 
  min_quant <- reactive({
    input$mean - 3*input$sd # set the minimum of the quantiles 
  })
  
  max_quant <- reactive({
    input$mean + 3*input$sd # set the maximum of the quantiles 
  })  
  
  quantiles <- reactive({
    seq(min_quant(), max_quant(), 0.01) 
  })
  
  # probability density 
  prob_dens <- reactive({
    dnorm(quantiles(), input$mean, input$sd) 
  })
  
  # cumulative probability 
  cuml_prob <- reactive({
    pnorm(quantiles(), input$mean, input$sd) 
  })
  
  # quantiles plot 
  quant_plot <- reactive({
    qnorm(seq(0,1,0.01), input$mean, input$sd)
  })
  
  
  ################################## 
  # These are the reactive outputs # These get displayed
  ################################## 
  
  # Plot the random sample 
  output$Plot_sample <-renderPlot({
    hist(rand_samp(),main="",xlab="Quantiles",col="grey")
  },
  width=400,height=400
  )
  
  # Mean 
  output$summary <- renderPrint({
    summary(rand_samp())
  })
  
  # Standard deviation 
  output$stdev <- renderPrint({
    sd(rand_samp())
  })
  
  # Plot the probability density
  output$Plot_prob_dens <-renderPlot({
    plot(quantiles(), prob_dens(), type="l", ylab="Probability density", xlab="Quantiles")
  },
  width=400,height=400
  )
  
  # Plot the cumulative probability 
  output$Plot_cuml_prob <-renderPlot({
    plot(quantiles(), cuml_prob(), type="l", ylab="Cumulative Probability", xlab="Quantiles")
  },
  width=400,height=400
  )
  
  # Plot the quantiles
  output$Plot_quantiles <-renderPlot({
    plot(seq(0,1,0.01),quant_plot(),type="l",ylab="Quantiles", xlab="Probability")
  },
  width=400,height=400
  )
  
}

ui <- shinyUI(pageWithSidebar(
  
  # Title
  headerPanel(""),
  
  sidebarPanel(
    numericInput("obs", 
                 label = "Number of observations:", 
                 min = 0, 
                 value = 1000),
    
    numericInput("mean",
                 "Mean:",
                 value=0,
                 step=0.01),
    
    numericInput("sd",
                 "Standard deviation:",
                 value=1,
                 step=0.01),
    
    hr(),
    helpText("Text Here")
  ),
  
  # GGPLOT
  
  mainPanel(
    tabsetPanel(
      tabPanel("Random Sample",
               plotOutput("Plot_sample"),
               h6("Summary:"),
               verbatimTextOutput("summary"),
               h6("Standard deviation:"),
               verbatimTextOutput("stdev")
      ),
      tabPanel("Probability Density",
               plotOutput("Plot_prob_dens"),
               h6("dnorm(quantiles, mean, sd)")
      ),
      tabPanel("Cumulative Probability",
               plotOutput("Plot_cuml_prob"),
               h6("pnorm(quantiles, mean, sd)")
      ),
      tabPanel("Quantile Plot",
               plotOutput("Plot_quantiles"),
               h6("qnorm(seq(0,1,0.01), mean, sd)")
      )  
    )
  )
  
))

shinyApp(ui = ui, server = server)