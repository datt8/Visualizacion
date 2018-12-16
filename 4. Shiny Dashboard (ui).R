
library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Práctica 3. Daniel Tomé Gordo"),
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "GIF", tabName = "gif"),
        menuItem(text = "Modelos", tabName = "modelos")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "gif", h3("#GraciasFernando"), uiOutput("imagengif")),
        tabItem(tabName = "modelos", 
                fluidRow(
                  box(plotOutput("modelo")),
                  box(
                    h3("Seleccione el tipo de modelo"),
                    selectInput(inputId = "eleccion", label = "Tipo de gráfica",
                                choices = c("Polinomial" = "polinomial", "K-means" = "kmeans"))
                  ),
                  uiOutput("cambiarmodelo")
          )
        )
      )
    )
  )
)