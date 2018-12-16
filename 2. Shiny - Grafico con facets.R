
library(shiny)
library(ggplot2)
library(plotly)

ui <- shinyUI(
  fluidPage(
    titlePanel("Tarea 2 Shiny"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "ejex", 
                    label="Variable eje X", 
                    choices = names(mtcars)
                    ),
        selectInput(inputId = "ejey",
                    label="Variable eje Y",
                    choices = names(mtcars)
                    ),
        selectInput(inputId = "facet",
                    label="Variable facet",
                    choices=names(mtcars)
                    ),
        checkboxInput(inputId = "eleccion", label = "Clicar para facet", value = TRUE)
                    
      ), 
      mainPanel(
        conditionalPanel(
          condition = "!input.eleccion",
          plotOutput("grafico_yes") 
        ),
        conditionalPanel(
          condition="input.eleccion",
          plotOutput("grafico_no")
        )
      ) 
    )
  )
) 
server <- function(input, output){ 
  output$grafico_yes<-renderPlot({
    ggplot(mtcars, aes_string(x=input$ejex, y=input$ejey))+ 
      geom_point()
  })
  output$grafico_no<-renderPlot({
    ggplot(mtcars, aes_string(x=input$ejex, y=input$ejey))+
      geom_point()+
      facet_grid(as.formula(paste("~",input$facet)))
  })
}

shinyApp(ui, server)