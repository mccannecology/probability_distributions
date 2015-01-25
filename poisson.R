server <- function(input,output){
  
  ####################################### 
  # These are the reactive calculations # These do not get displayed 
  ####################################### 
  
  rand_samp <- reactive({
    rpois(input$obs, input$lambda)
  })
  
  
  # THIS WILL NEED TO GET CHANGED FOR POISSON
  # calculate some stuff needed to define the quantiles 
  min_quant <- reactive({
    min(rand_samp()) # set the minimum of the quantiles 
  })
  
  max_quant <- reactive({
    max(rand_samp()) # set the maximum of the quantiles 
  })  
  
  quantiles <- reactive({
    seq(min_quant(), max_quant(), 1) 
  })
  
  # probability density 
  prob_dens <- reactive({
    dpois(quantiles(), input$lambda) 
  })
  
  # cumulative probability 
  cuml_prob <- reactive({
    ppois(quantiles(), input$lambda) 
  })
  
  # quantiles plot 
  quant_plot <- reactive({
    qpois(seq(0,1,0.01), input$lambda)
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
    plot(quantiles(), prob_dens(), ylab="Probability density", xlab="Quantiles")
  },
  width=400,height=400
  )
  
  # Plot the cumulative probability 
  output$Plot_cuml_prob <-renderPlot({
    plot(quantiles(), cuml_prob(), ylab="Cumulative Probability", xlab="Quantiles")
  },
  width=400,height=400
  )
  
  # Plot the quantiles
  output$Plot_quantiles <-renderPlot({
    plot(seq(0,1,0.01), quant_plot(),ylab="Quantiles", xlab="Probability")
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
    
    numericInput("lambda",
                 "Lambda:",
                 value=3,
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
               h6("dpois(quantiles, lambda)")
      ),
      tabPanel("Cumulative Probability",
               plotOutput("Plot_cuml_prob"),
               h6("ppois(quantiles, lambda)")
      ),
      tabPanel("Quantile Plot",
               plotOutput("Plot_quantiles"),
               h6("qpois(seq(0,1,0.01), lambda)")
      )  
    )
  )
  
))