server <- function(input,output){
  
  ####################################### 
  # These are the reactive calculations # These do not get displayed 
  ####################################### 
  
  rand_samp <- reactive({
    rlnorm(input$obs, input$mean, input$sd)
  })
  
  # calculate some stuff needed to define the quantiles   
  max_quant <- reactive({
    max(rand_samp()) # set the maximum of the quantiles 
  })  
  
  min_quant <- reactive({
    min(rand_samp()) # set the minimum of the quantiles 
  })  
  
  quantiles <- reactive({
    seq(min_quant(), max_quant(), ((max_quant()-min_quant())/1000)) 
  })
  
  # probability density 
  prob_dens <- reactive({
    dlnorm(quantiles(), input$mean, input$sd) 
  })
  
  # cumulative probability 
  cuml_prob <- reactive({
    plnorm(quantiles(), input$mean, input$sd) 
  })
  
  # quantiles plot 
  quant_plot <- reactive({
    qlnorm(seq(0,1,0.01), input$mean, input$sd)
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
    
    sliderInput("mean",
                "Mean:",
                min=-50,
                max=50,
                value=0,
                step=0.1),
    
    sliderInput("sd",
                "Standard deviation:",
                value=1,
                min=0.1,
                max=100,
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
               h6("dnorm(quantiles, mean, sd)")
      ),
      tabPanel("Cumulative Probability",
               plotOutput("Plot_cuml_prob"),
               h6("plnorm(quantiles, mean, sd)")
      ),
      tabPanel("Quantile Plot",
               plotOutput("Plot_quantiles"),
               h6("qlnorm(seq(0,1,0.01), mean, sd)")
      )  
    )
  )
  
))