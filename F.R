server <- function(input,output){
  
  ####################################### 
  # These are the reactive calculations # These do not get displayed 
  ####################################### 
  
  rand_samp <- reactive({
    rf(input$obs, input$df1, input$df2)
  })
  
  # probability density 
  prob_dens <- reactive({
    df(seq(0,8,0.1), input$df1, input$df2) 
  })
  
  # cumulative probability 
  cuml_prob <- reactive({
    pf(seq(0,8,0.1), input$df1, input$df2) 
  })
  
  # quantiles plot 
  quant_plot <- reactive({
    qf(seq(0,1,0.01), input$df1, input$df2)
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
    plot(seq(0,8,0.1), prob_dens(), type="l", ylab="Probability density", xlab="Quantiles")
  },
  width=400,height=400
  )
  
  # Plot the cumulative probability 
  output$Plot_cuml_prob <-renderPlot({
    plot(seq(0,8,0.1), cuml_prob(), type="l", ylab="Cumulative Probability", xlab="Quantiles")
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
    
    sliderInput("df1",
                "degrees of freedom 1:",
                min=1,
                max=100,
                value=6,
                step=1),
    
    sliderInput("df2",
                "degrees of freedom 2:",
                min=1,
                max=100,
                value=2,
                step=1)
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
               h6("df(quantiles, df1, df2)")
      ),
      tabPanel("Cumulative Probability",
               plotOutput("Plot_cuml_prob"),
               h6("pf(quantiles, df1, df2)")
      ),
      tabPanel("Quantile Plot",
               plotOutput("Plot_quantiles"),
               h6("qf(seq(0,1,0.01), df1, df2)")
      )  
    )
  )
  
))

shinyApp(ui = ui, server = server)