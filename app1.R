library(shiny)
library(ggplot2)
data(mtcars)

datos = mtcars

ui <- fluidPage (
  
  # Titulo de la app
  titlePanel("MTCARS y 'mpg'"),
  
  # Layout con panel lateral y panel principal
  sidebarLayout(
    sidebarPanel(
      # Menu para seleccionar un atributo
      selectInput(inputId = "attr",
                  "Elija un atributo para el eje Y:",
                  attributes(datos)$name[attributes(datos)$name != "mpg"],
                  selected="disp"),
      selectInput(inputId = "attr2",
                  "Elija un atributo para el size:",
                  attributes(datos)$name[attributes(datos)$name != "mpg"],
                  selected = "wt"),
      selectInput(inputId = "attr3",
                  "Elija un atributo para el color:",
                  attributes(datos)$name[attributes(datos)$name != "mpg"],
                  selected = "cyl"),
      actionButton(inputId = "histButton", label = "Mostrar/Esconder Histograma de mpg")
    ),
    
    # Panel principal con los widgets de salida
    mainPanel(
      conditionalPanel(
        condition = "input.histButton % 2 == 0",
        plotOutput(outputId = "scatterPlot"),
      ),
      conditionalPanel(
        condition = "input.histButton % 2 == 1",
        plotOutput(outputId="histogram")
      )
    )
  )
)

server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(data = datos,
           aes_string("mpg", input$attr, size=input$attr2, color=input$attr3)) +
      geom_point(alpha=0.75) + scale_size(range = c(1, 10)) + 
      labs(title = paste0("Correlacion entre '", input$attr ,"' y 'mpg'"))
  })
  
  output$histogram <- renderPlot({
    x <- datos$mpg
    bins <- seq(min(x), max(x), length.out = 6)
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "mpg",
         main = "Histogram of mpg")
  })
}

shinyApp(ui=ui,server=server)