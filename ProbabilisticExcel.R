library(shinydashboard)
library(shiny)
library(ggplot2)
library(FinancialMath)

ui <- 
  dashboardPage(
    dashboardHeader(title = "Probabilistic Excel"),
    dashboardSidebar(
      sidebarMenu(
        # Se crean las etiquetas del menú lateral
        menuItem(text = "Distribuciones", tabName = "tab_distrib"),
        menuItem(text = "Beneficios", tabName = "tab_beneficios"),
        menuItem(text = "Inversiones", tabName = "tab_inversiones")
      )
    ),
    dashboardBody(
      tabItems(
        # Este primera etiqueta se compone de un gráfico, la elección de la distribución con sus parámetros,
        # Además se añade una breve explicación de la distribución y una tabla resumen con los estadísticos principales
        tabItem(tabName = "tab_distrib",
                mainPanel(style = "text-align: justify; font-size: 16px;", width = 12,
                  h3("En esta sección usted puede probar distintos tipos de distribuciones que luego podrá usar en sus cálculos"),
                  p("Una distribución probabilística de una variable aleatoria es una función que asigna a cada suceso 
                     definido sobre la variable, la probabilidad de que dicho suceso ocurra. La distribución de probabilidad 
                     está definida sobre el conjunto de todos los sucesos y cada uno de los sucesos es el rango de valores 
                     de la variable aleatoria. También puede decirse que tiene una relación estrecha con las distribuciones 
                     de frecuencia. De hecho, una distribución de probabilidades puede comprenderse como una frecuencia teórica, 
                     ya que describe cómo se espera que varíen los resultados."),
                  textOutput("explicacion"),
                  tags$style("#explicacion {font-size: 16px; font-style: Italic;}")),
                fluidPage( 
                box(plotOutput("plotDistrib"), height = 450),
                box(selectInput(inputId = "distrib",
                                label = "Seleccione la distribución",
                                choices = c("Normal" = "normal", "Uniforme" = "unif",
                                            "Binomial" = "binom", "LogNormal" = "lognormal"))),
                box(uiOutput("parametros"), height = 350),
                box(tableOutput("resumen"), width = 12)
                )
              ),
        # En este segundo tab, se puede calcular la probabilidad de tener beneficios según los parámetros, 
        # Están los parámetros y el texto de la probabilidad
        tabItem(tabName = "tab_beneficios",
                mainPanel(style = "text-align: justify; font-size: 16px;", width = 12,
                          h3("En esta sección va poder calcular su probabilidad de tener beneficios según el método 
                             de Montecarlo, sujeto a los parámetros que decida"),
                          p("El método de Montecarlo es un tipo de simulación creado en escenarios inciertos para poder prever 
                            un suceso determinado, en este caso los beneficios de la empresa. Para ello, usted podrá elegir los
                            valores de sus ventas y costes, así como la distribución que siguen estos. Entre las distribuciones
                            se encuentran las anteriores, exceptuando la binomial."),
                          p("Este método realiza diversas simulaciones, que usted puede modificar, y tras realizarlas muestra la
                            probabilidad de que existan beneficios en esa situación")),
                fluidPage(
                  box(
                    selectInput(inputId = "distrib_calc",
                                label = "Distribución a seguir",
                                choices = c("Normal" = "normal", "Uniforme" = "unif", 
                                            "LogNormal" = "lognormal")),
                    numericInput(inputId = "uds_vendidas",
                                 label = "Unidades vendidas",
                                 value = 1000,
                                 step = 1,
                                 min = 1),
                    numericInput(inputId = "nsim",
                                 label = "Número de simulaciones",
                                 value = 100,
                                 step = 1,
                                 min = 1),
                    width = 12),
                  box(uiOutput("parametros_beneficios"), width = 6),
                  box(textOutput("prob_montecarlo"), width = 6, style = "text-align: justify; font-size: 16px;")
                  )
                ),
        # En este tercer tab, se podrá ver la conveniencia o no de la realización de una inversión o la tasa que se debe generar para que sea óptima
        tabItem(tabName = "tab_inversiones",
                mainPanel(style = "text-align: justify; font-size: 16px;", width = 12,
                          h3("En esta sección va a poder usted ver si le es razonable (o no) una inversión perpetua determinada"),
                          p("Una inversión perpetua es aquella que da un flujo de caja determinado, usted puede modificarlo, de manera
                            indefinida en el tiempo. Por ello, muchas veces es necesario saber su valor actual. Estas pueden ser prepagables
                            o pospagables, es decir a principio o final del período. En este caso serán siempre pospagables (a fin de período).
                            Además, se amplía esto con la posibilidad de estudiar la inversión vía método de Montecarlo y el posible 
                            crecimiento/decrecimiento de la renta (como en el caso de una pensión vitalicia)."),
                          p("Hay que tener en cuenta en el caso VAN creciente, que tasa de interés y de crecimiento/decrecimiento NO pueden ser iguales
                            pues provocaría una indeterminación en el cálculo y el resultado mostrado sería de 0 u.m")
                ),
                fluidPage(
                  box(width = 12,
                      numericInput(inputId = "inversion_inicial",
                                   label = "Inversión inicial",
                                   value = 1000,
                                   min = 1),
                      conditionalPanel(condition = "input.crecimiento",
                                       numericInput(inputId = "crecimiento_renta",
                                                    label = "Crecimiento/decrecimiento de la renta",
                                                    value = 0.03,
                                                    min = -1,
                                                    max = 1,
                                                    step = 0.001)),
                      checkboxInput(inputId = "montecarlo",
                                    label = "¿Desea usar el método de Montecarlo?"),
                      checkboxInput(inputId = "crecimiento",
                                    label = "¿Es su renta creciente/decreciente?")), 
                  box(uiOutput("parametros_inversiones")),
                  conditionalPanel(condition = "input.crecimiento && input.montecarlo",
                                   box(actionButton(inputId = "renta_creciente_montecarlo",
                                                label = "Generar Montecarlo creciente",
                                                icon = icon('refresh')),
                                   textOutput("van_renta_creciente_montecarlo"),
                                   style = "text-align: justify; font-size: 18px;")),
                  conditionalPanel(condition = "!input.crecimiento && input.montecarlo",
                                   box(actionButton(inputId = "renta_montecarlo",
                                                label = "Generar Montecarlo",
                                                icon = icon('refresh')),
                                   textOutput("van_renta_montecarlo"),
                                   style = "text-align: justify; font-size: 18px;")),
                  conditionalPanel(condition = "input.crecimiento && !input.montecarlo",
                                   box(actionButton(inputId = "renta_creciente",
                                                label = "Generar VAN creciente",
                                                icon = icon('refresh')),
                                   textOutput("van_renta_creciente"),
                                   style = "text-align: justify; font-size: 18px;")),
                  conditionalPanel(condition = "!input.crecimiento && !input.montecarlo",
                                   box(actionButton(inputId = "renta",
                                                label = "Generar VAN",
                                                icon = icon('refresh')),
                                   textOutput("van_renta"),
                                   style = "text-align: justify; font-size: 18px;"))
                  )
                )
              )
      )
    )

server <- function(input, output) {
  # En ese box aparecerán los parámetros según la elección del usuario. 
  # Para cada tipo de distribución se generan distintos parámetros
  output$parametros <- renderUI({
    if (input$distrib == "normal") {
      box(h4("Parámetros de la distribución normal"),
          column(width = 6,
            numericInput(inputId = "nobs_normal",
                         label = "Número de Observaciones",
                         value = 100,
                         step = 1),
            numericInput(inputId = "media_normal",
                         label = "Media de la Normal",
                         value = 0)),
          column(width = 6,
            numericInput(inputId = "desv_normal",
                         label = "Desviación Típica de la Normal",
                         value = 1),
            actionButton(inputId = "generar_normal",
                         label = "Generar",
                         icon('refresh'))),
          width = 12)
    }
    
    else if (input$distrib == "unif") {
      box(h4("Parámetros de la distribución Uniforme"),
          column(width = 6, height = 500,
                 numericInput(inputId = "nobs_unif",
                              label = "Número de Observaciones",
                              value = 100,
                              step = 1),
                 numericInput(inputId = "max_unif",
                              label = "Máximo valor",
                              value = 10),
                 numericInput(inputId = "min_unif",
                              label = "Mínimo valor",
                              value = 5)),
          p("El valor del mínimo SIEMPRE tiene que ser menor que el máximo", 
            style = "text-align: justify; font-size: 18px;"),
          column(width = 6, height = 500,
                 actionButton(inputId = "generar_unif",
                              label = "Generar",
                              icon('refresh'))),
          width = 12)
    }
    
    else if (input$distrib == "binom") {
      box(h4("Parámetros de la distribución Binomial"),
          column(width = 6,
                 numericInput(inputId = "nobs_binom",
                              label = "Número de Observaciones",
                              value = 100,
                              step = 1),
                 actionButton(inputId = "generar_binom",
                              label = "Generar",
                              icon('refresh'))),
          column(width = 6, 
                 numericInput(inputId = "prob_binom",
                       label = "Probabilidad",
                       value = 0.5,
                       min = 0,
                       max = 1,
                       step = 0.01)),
          width = 12)
    }
    
    else if (input$distrib == "lognormal") {
      box(h4("Parámetros de la distribución LogNormal"),
          column(width = 6,
                 numericInput(inputId = "nobs_lognormal",
                              label = "Número de Observaciones",
                              value = 100,
                              step = 1),
                 numericInput(inputId = "media_lognormal",
                              label = "Media",
                              value = 0)),
          column(width = 6,
                 numericInput(inputId = "desv_lognormal",
                              label = "Desviación Típica",
                              value = 1),
                 actionButton(inputId = "generar_lognormal",
                              label = "Generar",
                              icon('refresh'))),
          width = 12)
    }
  })
  
###################################
  
  # Si se toca el boton generar se aparece el gráfico y la muestra cambia
  observeEvent(input$generar_normal, {
      data <- data.frame(rnorm(input$nobs_normal, input$media_normal, input$desv_normal))
      names(data) <- c("x")
      data_normal <- data.frame(Media = mean(data$x), DesvTípica = sd(data$x), Min = min(data$x), 
                               Cuartil1 = quantile(data$x, 0.25), Mediana = median(data$x), 
                               Cuartil3 = quantile(data$x, 0.75), Max = max(data$x))
      output$plotDistrib <- renderPlot({
        ggplot(data, aes(x)) +
          geom_density(fill = "red")
      })
      
      # Se muestra la correspondiente explicación  
      output$explicacion <- renderText({
        print("La distribución normal (o de Gauss) es una de las distribuciones probabilísticas más usadas. Está caracterizada por una media y desviación típica dada que usted puede modificar")
      })
      
      # Se muestra el breve resumen (estadístico) de la muestra creada
      output$resumen <- renderTable({
        data_normal
      })
    })
  
  # Mismo proceso que antes
  observeEvent(input$generar_unif, {
    data <- data.frame(runif(input$nobs_unif, input$min_unif, input$max_unif))
    names(data) <- c("x")
    data_unif <- data.frame(Media = mean(data$x), DesvTípica = sd(data$x), Min = min(data$x), 
                             Cuartil1 = quantile(data$x, 0.25), Mediana = median(data$x), 
                             Cuartil3 = quantile(data$x, 0.75), Max = max(data$x))
    output$plotDistrib <- renderPlot({
      ggplot(data, aes(x)) +
        geom_density(fill = "lightblue")
    })
    
    output$explicacion <- renderText({
      print("La distribución uniforme discreta es una distribución de probabilidad que asume un número finito de valores con la misma probabilidad.")
    })
    
    output$resumen <- renderTable({
      data_unif
    })
  })
  
  # Mismo proceso que antes
  observeEvent(input$generar_binom, {
    data <- data.frame(rbinom(100, input$nobs_binom, input$prob_binom))
    names(data) <- c("x")
    data_binom <- data.frame(Media = mean(data$x), DesvTípica = sd(data$x), Min = min(data$x), 
                             Cuartil1 = quantile(data$x, 0.25), Mediana = median(data$x), 
                             Cuartil3 = quantile(data$x, 0.75), Max = max(data$x))
    output$plotDistrib <- renderPlot({
      ggplot(data, aes(x)) +
        geom_density(fill = "lightgreen")
          })
    
    output$explicacion <- renderText({
      print("La distribución binomial es una distribución de probabilidad discreta que cuenta el número de éxitos en una secuencia de n ensayos independientes entre sí, con una probabilidad fija p de ocurrencia del éxito entre los ensayos. A gran número de observaciones se parece a una normal.")
    })
    
    output$resumen <- renderTable({
      data_binom
    })
  })
  
  # Mismo proceso que antes
  observeEvent(input$generar_lognormal, {
    data <- data.frame(rlnorm(input$nobs_lognormal, input$media_lognormal, input$desv_lognormal))
    names(data) <- c("x")
    data_lognormal <- data.frame(Media = mean(data$x), DesvTípica = sd(data$x), Min = min(data$x), 
                             Cuartil1 = quantile(data$x, 0.25), Mediana = median(data$x), 
                             Cuartil3 = quantile(data$x, 0.75), Max = max(data$x))
    output$plotDistrib <- renderPlot({
      ggplot(data, aes(x)) +
        geom_density(fill = "orange")
    })
    
    output$explicacion <- renderText({
      print("La distribución normal logarítmica es una distribución de probabilidad de una variable aleatoria cuyo logaritmo está normalmente distribuido. 
            Es decir, si X es una variable aleatoria con una distribución normal, entonces exp(X) tiene una distribución log-normal")
    })
    
    output$resumen <- renderTable({
      data_lognormal
    })
  })

  #################################
  
  # Es una caso similar al anterior, dependiendo del input aparecen unos u otros parámetros
  output$parametros_beneficios <- renderUI({
    if (input$distrib_calc == "normal") {
      box(
        column(width = 4,
               numericInput(inputId = "media_ventas_norm",
                            label = "Media de las ventas",
                            value = 10,
                            min = 0),
               numericInput(inputId = "desv_ventas_norm",
                            label = "Desviación típica ventas",
                            value = 3,
                            min = 0),
               actionButton(inputId = "beneficio_normal",
                            label = "Generar",
                            icon = icon('refresh'))),
        column(width = 4,
               numericInput(inputId = "media_costes_norm",
                            label = "Media de los costes",
                            value = 5,
                            min = 0),
               numericInput(inputId = "desv_costes_norm",
                            label = "Desviación típica costes",
                            value = 2,
                            min = 0)),
        width = 12)
    }
    
    else if (input$distrib_calc == "unif") {
      box(
        column(width = 4,
               numericInput(inputId = "max_ventas_unif",
                            label = "Máximo de las ventas",
                            value = 10,
                            min = 0),
               numericInput(inputId = "min_ventas_unif",
                            label = "Mínimo de las ventas",
                            value = 3,
                            min = 0),
               actionButton(inputId = "beneficio_unif",
                            label = "Generar",
                            icon = icon('refresh'))),
        column(width = 4,
               numericInput(inputId = "max_costes_unif",
                            label = "Máximo de los costes",
                            value = 5,
                            min = 0),
               numericInput(inputId = "min_costes_unif",
                            label = "Mínimo de los costes",
                            value = 2,
                            min = 0)),
        column(width = 4, style = "text-align: center; font-size: 18px;",
               p("El valor del mínimo debe ser menor que el del máximo siempre")),
      width = 12)
    }
    
    else if (input$distrib_calc == "lognormal") {
      box(
        column(width = 4,
               numericInput(inputId = "media_ventas_log",
                            label = "Media de las ventas",
                            value = 1,
                            min = 0),
               numericInput(inputId = "desv_ventas_log",
                            label = "Desviación típica de las ventas",
                            value = 0.47,
                            min = 0),
               actionButton(inputId = "beneficio_log",
                            label = "Generar",
                            icon = icon('refresh'))),
        column(width = 4,
               numericInput(inputId = "media_costes_log",
                            label = "Media de los costes",
                            value = 0.7,
                            min = 0),
               numericInput(inputId = "desv_costes_log",
                            label = "Desviación típica de los costes",
                            min = 0,
                            value = 0.3)),
      width = 12)
    }
  })
  
  #################################
  
  # Se crean tres funciones (una por distribución) que generan la probabilidad de que se tengan beneficios
  montecarlo_norm <- function(media_vtas, desv_vtas, media_coste, desv_coste, uds, sims) {
    prob <- NULL
    for (i in 1:sims) {
      ventas <- data.frame(x = rnorm(uds, media_vtas, desv_vtas))
      costes <- data.frame(x = rnorm(uds, media_coste, desv_coste))
      beneficios <- ifelse(ventas - costes > 0, 1, 0)
      prob[i] <- mean(beneficios)
    }
    return(mean(prob))
  }
  
  montecarlo_unif <- function(max_vtas, min_vtas, max_coste, min_coste, uds, sims) {
    prob <- NULL
    for (i in 1:sims) {
      ventas <- data.frame(x = runif(uds, min_vtas, max_vtas))
      costes <- data.frame(x = runif(uds, min_coste, max_coste))
      beneficios <- ifelse(ventas - costes > 0, 1, 0)
      prob[i] <- mean(beneficios)
    }
    return(mean(prob))
  }
  
  montecarlo_log <- function(media_vtas, desv_vtas, media_coste, desv_coste, uds, sims) {
    prob <- NULL
    for (i in 1:sims) {
      ventas <- data.frame(x = rlnorm(uds, media_vtas, desv_vtas))
      costes <- data.frame(x = rlnorm(uds, media_coste, desv_coste))
      beneficios <- ifelse(ventas - costes > 0, 1, 0)
      prob[i] <- mean(beneficios)
    }
    return(mean(prob))
  }
  
  # Se crea el valor reactivo donde se guardarán las salidas
  
  res <- reactiveValues()
  
  # Valor para el caso normal, cada vez que se pulsa el botón se crea un nuevo escenario y se saca la probabilidad
  observeEvent(input$beneficio_normal, {
    res$benef_norm <- montecarlo_norm(input$media_ventas_norm, input$desv_ventas_norm, input$media_costes_norm,
                                      input$desv_costes_norm, input$uds_vendidas, input$nsim)
    output$prob_montecarlo <- renderText({
      paste0("La probabilidad de tener beneficios en el caso de la distribución normal es del ", 
             round(res$benef_norm * 100, 2), " %")
    })
  })
  
  # Caso de la uniformidad, es lo mismo que el caso de la normal
  observeEvent(input$beneficio_unif, {
    res$benef_unif <- montecarlo_unif(input$max_ventas_unif, input$min_ventas_unif, input$max_costes_unif,
                                      input$min_costes_unif, input$uds_vendidas, input$nsim)
    output$prob_montecarlo <- renderText({
      paste0("La probabilidad de tener beneficios en el caso de la distribución uniforme es del ", 
             round(res$benef_unif * 100, 2), " %")
    })
  })
  
  # Caso de la LogNormal, igual que los anteriores
  observeEvent(input$beneficio_log, {
    res$benef_log <- montecarlo_log(input$media_ventas_log, input$desv_ventas_log, input$media_costes_log,
                                      input$desv_costes_log, input$uds_vendidas, input$nsim)
    output$prob_montecarlo <- renderText({
      paste0("La probabilidad de tener beneficios en el caso de la distribución LogNormal es del ", 
             round(res$benef_log * 100, 2), " %")
    })
  })
  
  #################################
  
  output$parametros_inversiones <- renderUI({
    box(width = 12,
        conditionalPanel(condition = "input.montecarlo", 
                         column(width = 6,
                                numericInput(inputId = "media_van_montecarlo",
                                             label = "Media de la renta",
                                             value = 1000,
                                             min = 0),
                                numericInput(inputId = "desv_van_montecarlo",
                                             label = "Desviación típica de la renta",
                                             value = 500,
                                             min = 0),
                                numericInput(inputId = "nsim_montecarlo",
                                             label = "Número de simulaciones",
                                             value = 100,
                                             step = 1,
                                             min = 1)),
                         column(width = 6,
                                numericInput(inputId = "media_tipo_interes",
                                             label = "Tipo interés medio",
                                             value = 0.05,
                                             min = 0,
                                             max = 1,
                                             step = 0.001),
                                numericInput(inputId = "desv_tipo_interes",
                                             label = "Desviación típica tipo de interés",
                                             value = 0.005,
                                             min = 0,
                                             max = 1,
                                             step = 0.0001))),
        conditionalPanel(condition = "!input.montecarlo",
                         numericInput(inputId = "flujo_caja",
                                      label = "Renta",
                                      value = 100,
                                      min = 0),
                         numericInput(inputId = "tasa_interes",
                                      label = "Tipo de interés",
                                      value = 0.05,
                                      step = 0.001)))
  })
  
  #################################
  ## Se crean la fórmula, salida y reactividad de la inversión creciente según el método de Montecarlo
  
  VAN_montecarlo_creciente <- function(cuota_media, desv_media, interes_medio, desv_interes, inversion, nsim, crecimiento) {
    resultado <- NULL
    for (i in 1:nsim) {
      cuota <- rnorm(1, cuota_media, desv_media)
      interes <- rnorm(1, interes_medio, desv_interes)
      resultado[i] <- ifelse(-inversion + (cuota / (interes - crecimiento)) > 0, 1, 0)
    }
    return(mean(resultado))
  }
  
  observeEvent(input$renta_creciente_montecarlo, {
    res$renta_creciente_montecarlo <- VAN_montecarlo_creciente(input$media_van_montecarlo, input$desv_van_montecarlo, input$media_tipo_interes, 
                                          input$desv_tipo_interes, input$inversion_inicial, input$nsim_montecarlo,
                                          input$crecimiento_renta)
    output$van_renta_creciente_montecarlo <- renderText({
      paste0("La probabilidad de obtener beneficio en esta inversión es de  ", 
             round(res$renta_creciente_montecarlo * 100, 2), " %")
    })
  })
  
  
  
  #################################
  ## Se sigue el mismo proceso que antes
  
  VAN_montecarlo <- function(cuota_media, desv_media, interes_medio, desv_interes, inversion, nsim) {
    resultado <- NULL
    for (i in 1:nsim) {
      cuota <- rnorm(1, cuota_media, desv_media)
      interes <- rnorm(1, interes_medio, desv_interes)
      resultado[i] <- ifelse(-inversion + (cuota / interes) > 0, 1, 0)
    }
    return(mean(resultado))
  }
  
  observeEvent(input$renta_montecarlo, {
    res$renta_montecarlo <- VAN_montecarlo(input$media_van_montecarlo, input$desv_van_montecarlo,
                                           input$media_tipo_interes, input$desv_tipo_interes,
                                           input$inversion_inicial, input$nsim_montecarlo)
    output$van_renta_montecarlo <- renderText({
      paste0("La probabilidad de obtener beneficio en esta inversión es de ", round(res$renta_montecarlo * 100, 2), " %")
    })
  })
  
  
  
  #################################
  ## Se crea la función del VAN
  VAN <- function(cuota, interes, inversion) {
    resultado <- -inversion + (cuota / interes)
    return(resultado)
  }
  
  observeEvent(input$renta, {
    res$renta <- VAN(input$flujo_caja, input$tasa_interes, input$inversion_inicial)
    output$van_renta <- renderText({
      paste0("El Valor Actual Neto (VAN) de esta inversión es de ", round(res$renta, 2), " u.m.")
    })
  })
  
  
  
  #################################
  # Se crea la función del VAN creciente
  VAN_creciente <- function(cuota, interes, inversion, crecimiento) {
    resultado <- -inversion + (cuota / (interes - crecimiento))
    if (interes == crecimiento) {
      return(0)
    }
    else {
      return(resultado)
    }
  }
  
  observeEvent(input$renta_creciente, {
    res$renta_creciente <- VAN_creciente(input$flujo_caja, input$tasa_interes, 
                                         input$inversion_inicial, input$crecimiento_renta)
    output$van_renta_creciente <- renderText({
      paste0("El Valor Actual Neto (VAN) de esta inversión es de ", round(res$renta_creciente, 2), " u.m.")
    })
  })

}
shinyApp(ui, server)