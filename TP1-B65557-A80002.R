library(shiny)

ui <- fluidPage(
  
  titlePanel("Calculo de potencias"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha","Nivel de significancia:",
                  min = 0.01,
                  max = 0.1,
                  value = 0.05 ),
      numericInput("mH0","Hipotesis nula", 30, min = NA, max = NA, step = NA,
                   width = NULL),
      numericInput("mH1","Hipotesis alternativa", 35, min = NA, max = NA, step = NA,
                   width = NULL),
      numericInput("n","Muestra", 64, min = NA, max = NA, step = NA,
                   width = NULL),
      numericInput("sd", "Desviacion", 7, min = NA, max = NA, step = NA,
                   width = NULL)
    ),
    
    mainPanel(plotOutput("Potencia")
    )
  )
)

server <- function(input, output) {
  
  output$Potencia <- renderPlot({
    zAlpha <- qnorm(1-input$alpha)
    zCritico <- input$mH0 + zAlpha*(input$sd/sqrt(input$n))
    zBeta <<- (zCritico-input$mH1)/(input$sd/sqrt(input$n))
    Beta <- pnorm((zCritico-input$mH1)/(input$sd/sqrt(input$n)))
    potencia <- 1-Beta  
    
    x <- seq(0, 100, length.out = 200)
    y0 <- dnorm(x, mean = input$mH0, sd = input$sd)*input$sd
    y1 <- dnorm(x, mean = input$mH1, sd = input$sd)*input$sd
    plot(x, y0, 
         xlim = c(0,100),
         ylim = c(0,1), 
         type = "l", 
         main = "Potencia Estadistica (1 - Beta )",
         xlab = "Media Muestral",
         ylab = "fdp",asp = 50, 
         col= "deepskyblue") + 
      abline(h= 0, v = c(input$mH0,input$mH1,zCritico), col=c("dimgray", "gray0","coral"), lty="dashed", lwd= 1)
    lines(x, y1, col= "blue", lwd= 2)
  })
}

shinyApp(ui = ui, server = server)