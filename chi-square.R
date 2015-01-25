server <- function(input,output){
  
  ####################################### 
  # These are the reactive calculations # These do not get displayed 
  ####################################### 
  
  rand_samp <- reactive({
    rchisq(input$obs, input$df)
  })
  
  max_quant <- reactive({
    max(rand_samp())
  })
  
  quantiles <- reactive({
    seq(0,max_quant(),(max_quant()/1000))
  })
  
  # probability density 
  prob_dens <- reactive({
    dchisq(quantiles(), input$df) 
  })
  
  # cumulative probability 
  cuml_prob <- reactive({
    pchisq(quantiles(), input$df) 
  })
  
  # quantiles plot 
  quant_plot <- reactive({
    qchisq(seq(0,1,0.01), input$df)
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
    
    sliderInput("df",
                "degrees of freedom:",
                min=1,
                max=100,
                value=6,
                step=1),
    
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
               h6("dchisq(quantiles, df1)")
      ),
      tabPanel("Cumulative Probability",
               plotOutput("Plot_cuml_prob"),
               h6("pchisq(quantiles, df1)")
      ),
      tabPanel("Quantile Plot",
               plotOutput("Plot_quantiles"),
               h6("qchisq(seq(0,1,0.01), df1)")
      )  
    )
  )
  
))

shinyApp(ui = ui, server = server)