library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stringr")
library("plotly")

setwd(".")

delitos <- read.csv("pnc_victimas_2016_2020.csv")
dic_delitos <- read.csv("delito_guatemala.csv")
dic_departamento <- read.csv("departamento_guatemala.csv")

delitos <- inner_join(delitos, dic_delitos)
delitos <- inner_join(delitos, dic_departamento)

delitos$fecha <- as.Date(paste(delitos$anio_ocu, paste(str_pad(delitos$mes_ocu,width=2,pad="0"), str_pad(delitos$día_ocu,width=2,pad="0"), sep="-"),sep="-"))
delitos$ocurrencia <- 1

ui <- fluidPage(
  
  # App title ----
  titlePanel("Crimen en Guatemala, 2016-2020"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      dateRangeInput(inputId = "fechas", "Seleccione el rango de fecha", start = min(delitos$fecha), end= max(delitos$fecha) ),
      selectInput(inputId = "departamento", label = "Departamento", choices = dic_departamento$Departamento),
      checkboxGroupInput(inputId = "delito", label = "Delito", choices = dic_delitos$Delito),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Grafico", h2("Rango de fechas"),
                           plotlyOutput(outputId = "grafica"),
                           h2("Hora en que ocurre"),
                           plotlyOutput(outputId = "grafica2"),),
                  tabPanel("Tabla", h2("Rango de fechas"),
                           dataTableOutput(outputId = "tabla"),
                           h2("Hora en que ocurre"),
                           dataTableOutput(outputId = "tabla2"),
                  )
      )

    )
  )
)

server <- function(input, output) {
  tabla_dia <-reactive({
    delitos %>% filter(Departamento == input$departamento) %>% 
      filter(Delito %in% input$delito) %>% filter(fecha >= input$fechas[1] & fecha <= input$fechas[2]) %>%
      group_by(Departamento, Delito, fecha) %>% summarise(Cantidad = sum(ocurrencia))})
  
  tabla_hora <-reactive({
    delitos %>% filter(Departamento == input$departamento) %>% 
      filter(Delito %in% input$delito) %>% filter(fecha >= input$fechas[1] & fecha <= input$fechas[2]) %>%
      group_by(Departamento, Delito, hora_ocu) %>% summarise(Cantidad = sum(ocurrencia))})
  
  muestra <- reactive({
    input$mostrar
  })

  output$grafica <- renderPlotly({
    p <- ggplot(tabla_dia(), aes(fecha, Cantidad))
    p <- p + geom_point(aes(color = factor(Delito)))
    ggplotly(p)
  })
  
  output$grafica2 <- renderPlotly({
    p <- ggplot(tabla_hora(), aes(hora_ocu, Cantidad))
    p <- p + geom_point(aes(color = factor(Delito))) + xlim(0,24)
    ggplotly(p)
  })
  
  output$tabla <- renderDataTable(tabla_dia())
  
  output$tabla2 <- renderDataTable(tabla_hora())
}

shinyApp(ui = ui, server = server)
