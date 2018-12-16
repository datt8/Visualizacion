#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Se define la interfaz para la aplicación
ui <- fluidPage(
   
   # Título
   titlePanel("Old Faithful Geyser Data"),
   
   # Se muestra en la barra lateral los parámetros del histograma y la activación del checkbox 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Número de barras:",
                     min = 1,
                     max = 50,
                     value = 30),
         
         checkboxInput(inputId = 'checkbox',
                       label = 'Ver cilindros',
                       value = FALSE)
      ),
      
      #Se usa el conditional panel para verificar la condición (checkbox o histograma)
      mainPanel(
         #plotOutput("distPlot")
        conditionalPanel(
          condition = "!input.checkbox",
          plotOutput("distPlot")
        ),
        conditionalPanel(
          condition = "input.checkbox",
          plotOutput("distPlot2")
        ) 
      )
   )
)

# Se define el servidor para pintar el histograma
server <- function(input, output) {
   
  # CheckBox
  output$value <- renderPrint({ input$checkbox })
  
    output$distPlot <- renderPlot({
      # Genera los parámetros del gráfico
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # Pinta el histograma con los parámetros antes creados
      hist(x, breaks = bins, col = 'darkgray', border = 'white', main="Histograma")
    })
    
    output$distPlot2 <- renderPlot({
      # Se crea el nuevo gráfico (checkbox marcado)
      ggplot(mpg, aes(x = cyl)) + geom_bar(fill = 'gray')+xlab("Cilindros por coche")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
