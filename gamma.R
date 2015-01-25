server <- function(input,output){
  
  ################################# 
  # These are the reactive inputs # These do not get displayed 
  ################################# 
  
  rand_samp <- reactive({
    rgamma(input$obs, input$shape, input$scale)
  })
  
  max_quant <- reactive({
    max(rand_samp())
  })
  
  quantiles <- reactive({
    seq(0, max_quant(), 0.01) 
  })
  
  prob_dens <- reactive({
    dgamma(quantiles(), input$shape, input$scale) 
  })
  
  cuml_prob <- reactive({
    pgamma(quantiles(), input$shape, input$scale) 
  })
  
  quant_plot <- reactive({
    qgamma(seq(0,1,0.01), input$shape, input$scale)
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
    
    numericInput("shape",
                 "Shape parameter:",
                 min=0,
                 max=1000,
                 value=3,
                 step=0.01),
    
    numericInput("scale",
                 "Scale parameter:",
                 min=0,
                 max=1000,
                 value=0.5,
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
               h6("dgamma(quantiles, shape, scale)")
      ),
      tabPanel("Cumulative Probability",
               plotOutput("Plot_cuml_prob"),
               h6("pgamma(quantiles, shape, scale)")
      ),
      tabPanel("Quantile Plot",
               plotOutput("Plot_quantiles"),
               h6("qgamma(seq(0,1,0.01), shape, scale)")
      )  
    )
  )
  
))