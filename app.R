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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Roue wavelength"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("v",
                        "Angle:",
                        min = 0,
                        max = 1,
                        value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        zones <- c(
            "01", "21", "31", "41", "32", "22",
            "02", "23", "33", "42", "34", "24"
        )
        
        roue <- data.frame(
            zone = factor(zones, levels = zones, ordered = TRUE),
            largeur = c(10, 1, 1, 1, 1, 1, 10, 1, 1, 1, 1, 1)
        )
        
        v <- input$v
        ggplot(roue, aes(x="", fill = zone, y = largeur)) + 
            geom_col() + 
            geom_polygon(
                aes(x, y), inherit.aes = FALSE, fill="black",
                data=data.frame(x = c(0.5, 1.5, 1.5, 0.5), y = c(tronque(7.5-30*v), tronque(7.5-30*v), tronque(22.5-30*v), tronque(22.5-30*v)))
            ) +
            geom_polygon(
                aes(x, y), inherit.aes = FALSE, fill="black",
                data=data.frame(x = c(0.5, 1.5, 1.5, 0.5), y = c(30-tronque(-7.5+30*v), 30-tronque(-7.5+30*v), 30-tronque(-22.5+30*v), 30-tronque(-22.5+30*v)))
            ) +
            coord_polar(theta="y", direction=-1, start = v*2*pi) +
            scale_fill_manual(values = c(rep(c("white", "yellow", "orange", "blue", "orange", "yellow"), 2)))    })
}

# Run the application 
shinyApp(ui = ui, server = server)
