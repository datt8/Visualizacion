
library(shiny)
library(ggplot2)
library(MASS)
library(plotly)

shinyServer(function(input, output){
  
  grafica <- reactiveValues(graf = ggplot())
  
  observeEvent(input$muestra, {
    grupo <- mvrnorm(n = input$tamano, mu = rep(0, 2), 
                    Sigma = matrix(c(1, input$correlacion, input$correlacion, 1), 
                                   nrow = 2, ncol = 2))
    grafica$graf <- ggplot(data = as.data.frame(grupo), aes(x = grupo[,1], y = grupo[,2])) +
      geom_point() +
      xlab("Variable X") + 
      ylab("variable Y")
  })
  
  observeEvent(input$modelos, {
    grafica$graf <- grafica$graf + stat_smooth(method = "lm", 
                                                 formula = y ~ poly(x, isolate(input$ordenpoli)), se = FALSE)
  })
  
  output$grafica <- renderPlot({
    grafica$graf
  })
})