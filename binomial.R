server <- function(input,output){
  
  ####################################### 
  # These are the reactive calculations # These do not get displayed 
  ####################################### 
  
  rand_samp <- reactive({
    rbinom(input$obs, input$size, input$prob)
  })
  
  # calculate some stuff needed to define the quantiles   
  max_quant <- reactive({
    input$size + 1 # set the maximum of the quantiles 
  })  
  
  quantiles <- reactive({
    seq(0, max_quant(), 1) 
  })
  
  # probability density 
  prob_dens <- reactive({
    dbinom(quantiles(), input$size, input$prob) 
  })
  
  # cumulative probability 
  cuml_prob <- reactive({
    pbinom(quantiles(), input$size, input$prob) 
  })
  
  # quantiles plot 
  quant_plot <- reactive({
    qbinom(seq(0,1,0.01), input$size, input$prob)
  })
  
  
  ################################## 
  # These are the reactive outputs # These get displayed
  ################################## 
  
  # Plot the random sample 
  output$Plot_sample <-renderPlot({
    hist(rand_samp(),main="",xlab="# of successes",col="grey")
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
    plot(quantiles(), prob_dens(), type="h", lwd=5, ylab="Probability density", xlab="# of successes")
  },
  width=400,height=400
  )
  
  # Plot the cumulative probability 
  output$Plot_cuml_prob <-renderPlot({
    plot(quantiles(), cuml_prob(), type="h", lwd=5, ylab="Cumulative Probability", xlab="# of successes")
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
    
    sliderInput("size",
                "size:",
                min=1,
                max=100,
                value=1,
                step=1),
    
    sliderInput("prob",
                "Probability:",
                value=0.5,
                min=0,
                max=1,
                step=0.01)
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
               h6("dbinom(quantiles, size, prob)")
      ),
      tabPanel("Cumulative Probability",
               plotOutput("Plot_cuml_prob"),
               h6("pbinom(quantiles, size, prob)")
      ),
      tabPanel("Quantile Plot",
               plotOutput("Plot_quantiles"),
               h6("qbinom(seq(0,1,0.01), size, prob)")
      )  
    )
  )
  
))