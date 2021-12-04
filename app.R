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
library(pdftools)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Roue wavelength"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fluidRow("Plutôt"),
            fluidRow(textOutput('gauche')), 
            fluidRow("ou"),
            fluidRow(textOutput('droite'))
            ),
        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    cartes_tirees <- tirer_cartes(cartes)

    output$text <- renderText(c("Plutôt", cartes_tirees$Gauche, "ou", cartes_tirees$Droite), sep="\n")
    output$gauche <- renderText(cartes_tirees$Gauche)
    output$droite <- renderText(cartes_tirees$Droite)
    
    output$distPlot <- renderPlot({
        zones <- c(
            "01", "21", "31", "41", "32", "22",
            "02", "23", "33", "42", "34", "24"
        )
        
        roue <- data.frame(
            zone = factor(zones, levels = zones, ordered = TRUE),
            largeur = c(10, 1, 1, 1, 1, 1, 10, 1, 1, 1, 1, 1)
        )
        
        v <- tirer_valeur()
        
        ggplot(roue, aes(x="", fill = zone, y = largeur)) + 
            geom_col() + 
            geom_polygon(
                aes(x, y), inherit.aes = FALSE, fill="black",
                data=data.frame(x = c(0.5, 1.5, 1.5, 0.5), y = c(tronque(7-30*v), tronque(7-30*v), tronque(23-30*v), tronque(23-30*v)))
            ) +
            geom_polygon(
                aes(x, y), inherit.aes = FALSE, fill="black",
                data=data.frame(x = c(0.5, 1.5, 1.5, 0.5), y = c(30-tronque(-7+30*v), 30-tronque(-7+30*v), 30-tronque(-23+30*v), 30-tronque(-23+30*v)))
            ) +
            coord_polar(theta="y", direction=-1, start = v*2*pi) +
            scale_fill_manual(values = c(rep(c("#f0f0f0", "yellow", "orange", "blue", "orange", "yellow"), 2))) +
            geom_label(inherit.aes=FALSE, x=0, y=22.5-30*v, label=cartes_tirees$Gauche) +
            geom_label(inherit.aes=FALSE, x=1, y=22.5-30*v, label=cartes_tirees$Droite) +
            theme_void() + 
            theme(legend.position = "none")
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
