
library(shiny)
library(ggplot2)
library(MASS)
library(dplyr)

shinyServer(function(input, output) {
  # Salida del GIF
  output$imagengif <- renderUI({
    tags$img(width = 480, height = 270, src = "https://media.giphy.com/media/8F94ADlcSJIvw86evZ/giphy.gif")
  })
# Cambio de los boxes entre modelos condicional
  output$cambiarmodelo <- renderUI({
    if (input$eleccion == "polinomial") {
      box(h4("Parámetros modelo polinomial"),
          numericInput("tamano", label = "Tamaño de la Muestra", value = 100),
          numericInput("correlacion", label = "Correlación entre las variables", value = 0, min = -1, max = 1, step = 0.01),
          actionButton("muestra", label = "Generar nueva muestra"),
          numericInput("ordenpoli", label = "Orden polinómico", value = 1, step = 1, min = 1),
          actionButton("modpoli", label = "Nuevo Modelo"))
    }
    
    else 
      box(h4("Parámetros modelo K-Means"),
          selectInput("varx", label = "Variable X", choices = names(iris[-5])),
          selectInput("vary", label = "Variable Y", choices = names(iris[-5])),
          numericInput("nclusters", label = "Número de clusters", min = 1, step = 1, value = 1),
          actionButton("modelokmeans", "Generar modelo"))
    })
  salida <- reactiveValues()
  # Se da función al boton de la muestra (que genera la muestra aleatoria)
  observeEvent(input$muestra, {
    parametros.normal <- mvrnorm(n = input$tamano, mu = rep(0, 2), 
                     Sigma = matrix(c(1, input$correlacion, input$correlacion, 1), 
                                    nrow = 2, ncol = 2))
    salida$grafico <- ggplot(data = as.data.frame(parametros.normal), 
                           aes(x = parametros.normal[,1], y = parametros.normal[,2])) +
      geom_point() +
      xlab("Variable X") + 
      ylab("variable Y")
  })
  
  # Se da función al botón del orden polinomial
  observeEvent(input$modpoli, {
    salida$grafico <- salida$grafico + stat_smooth(method = "lm", 
                                               formula = y ~ poly(x, isolate(input$ordenpoli)), se = FALSE)
  })
  # Se crea el modelo de k-means provocando que no cambie hasta que el boton no sea pulsado
  miModelo <- reactive({
    input$modelokmeans
    iris %>% 
      select(isolate(input$varx), isolate(input$vary)) %>%
      kmeans(isolate(input$nclusters))
  })
  # Se crea el gráfico, al pulsar el botón cambiará 
  observeEvent(input$modelokmeans, {
    salida$grafico <- ggplot(iris, aes_string(x = isolate(input$varx),
                                                 y = isolate(input$vary),
                                                 color = as.factor(miModelo()$cluster))) +
      geom_point()
  })
  # Se crea la salida final
  output$modelo <- renderPlot({
    salida$grafico
  })
  
})

