library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    titlePanel("Modelos Polinomiales. Daniel Tomé Gordo"),
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId = "tamano", label = "Tamaño de la Muestra", value = 100),
        numericInput(inputId = "correlacion", label = "Correlación entre las variables", 
                     value = 0, min = -1, max = 1, step = 0.01),
        actionButton(inputId = "muestra", label = "Generar nueva muestra"),
        numericInput(inputId = "ordenpoli", label = "Orden polinómico del modelo", 
                     value = 1, min = 1),
        actionButton(inputId = "modelos", label = "Nuevo modelo")
      ),
      mainPanel(
        plotOutput("grafica")
      )
    )
  )
)