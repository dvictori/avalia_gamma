#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(rgdal)
library(fitdistrplus)
library(ggmap)

# load data
bbox <- c(left = -75, bottom = -35, right = -33, top = 10)
map <- get_map(location = bbox)

chuva_dec_1 <- brick('data/prec_decendial_xavier_1980_2015.tif')
chuva_dec_2 <- brick('data/prec_decendial_acum_2_xavier_1980_2015.tif')
chuva_dec_3 <- brick('data/prec_decendial_acum_3_xavier_1980_2015.tif')
chuva_dec_4 <- brick('data/prec_decendial_acum_4_xavier_1980_2015.tif')
chuva_dec_5 <- brick('data/prec_decendial_acum_5_xavier_1980_2015.tif')
chuva_dec_6 <- brick('data/prec_decendial_acum_6_xavier_1980_2015.tif')

# define some funcs
get_gamma <- function(dado, decendio, lat, lon) {
    pt <- cbind(lon, lat)
    serie <- as.numeric(raster::extract(dado, pt))
    
    serie <- data.frame(dec = rep(1:36, 36),
                        prec = serie)
    
    fit <- fitdist(serie[serie$dec == decendio, 'prec'],
                   'gamma', method = 'mme')
    
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ajuste da distribuição Gamma para a precipitação"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'decendio', label = 'Decendio inicial',
                        choices = 1:36, selected = 1),
            selectInput(inputId = 'agregacao',
                         label = 'Período de agregação (decendios)',
                         choices = 1:6, selected = 1),
            numericInput(inputId = 'limiar', label = 'Limiar de chuva máxima',
                         value = 100, min = 0),
            plotOutput(outputId = 'mapOutput',
                       width=300, height=300,
                       click = 'plot_click')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("texto"),
           textOutput('coords'),
           plotOutput('plotGamma'),
           textOutput('prob_chuva_max')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mapOutput <- renderPlot({
        map_plot <- ggmap(map)
        map_plot + coord_cartesian()
    })
    
    output$texto <- renderText(
        paste('Distribuição gamma ajustada para precipitação acumulada de',
              input$agregacao, 'decendio(s), com início no decêndio',
              input$decendio)
    )
    
    output$coords <- renderText({
        paste0("x=", round(input$plot_click$x, 3),
               "\ty=", round(input$plot_click$y, 3))
    })
    
    modeloGamma <- reactive({
        
        chuva <- chuva_dec_1
        switch (input$agregacao,
                '1' = {chuva <- chuva_dec_1},
                '2' = {chuva <- chuva_dec_2},
                '3' = {chuva <- chuva_dec_3},
                '4' = {chuva <- chuva_dec_4},
                '5' = {chuva <- chuva_dec_5},
                '6' = {chuva <- chuva_dec_6}
        )
        
        ajustado <- get_gamma(chuva, input$decendio,
                              input$plot_click$y, input$plot_click$x)
    })
    
    output$plotGamma <- renderPlot(({
        plot(modeloGamma())
    }))
    
    output$prob_chuva_max <- renderText({
        shape <- modeloGamma()$estimate[1]
        rate <- modeloGamma()$estimate[2]
        prob <- pgamma(input$limiar,
                       shape = shape,
                       rate = rate,
                       lower.tail = FALSE)
        
        sprintf('Probabilidade de chuva exceder %i mm: %.2f %%',
                input$limiar, prob*100)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
