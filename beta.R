server <- function(input,output){
  
  ################################# 
  # These are the reactive inputs # These do not get displayed 
  ################################# 
  
  rand_samp <- reactive({
    rbeta(input$obs, input$shape1, input$shape2)
  })
  
  quantiles <- reactive({
    seq(0, 1, 0.01) 
  })
  
  prob_dens <- reactive({
    dbeta(quantiles(), input$shape1, input$shape2) 
  })
  
  cuml_prob <- reactive({
    pbeta(quantiles(), input$shape1, input$shape2) 
  })
  
  quant_plot <- reactive({
    qbeta(seq(0,1,0.01), input$shape1, input$shape2)
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
    sliderInput("obs", 
                label = "Number of observations:", 
                min = 1, 
                max = 1000,
                value = 500,
                step=1),

    sliderInput("shape1",
                 "Shape parameter 1:",
                 min=0.1,
                 max=10,
                 value=0.5,
                 step=0.1),
    
    sliderInput("shape2",
                 "Shape parameter 2:",
                 min=0.1,
                 max=10,
                 value=0.5,
                 step=0.1)
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
               h6("dbeta(seq(0,1,0.01), shape1, shape2)")
      ),
      tabPanel("Cumulative Probability",
               plotOutput("Plot_cuml_prob"),
               h6("pbeta(seq(0,1,0.01), shape1, shape2)")
      ),
      tabPanel("Quantile Plot",
               plotOutput("Plot_quantiles"),
               h6("qbeta(seq(0,1,0.01), shape1, shape2)")
      )  
    )
  )
  
))