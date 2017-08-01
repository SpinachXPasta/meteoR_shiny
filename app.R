library(shiny)
library(ggplot2)
library(EDAWR)
library(dplyr)
library(tidyr)
library(gridExtra)
library(rworldmap)
library(ggmap)
library(maps)
library(maptools)
library(lubridate)

meteor <- read.csv("meteor_refined.csv")

world_map <- map_data("world")
p <- ggplot() + coord_fixed() +xlab("") + ylab("")

base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="white", fill="light green")

cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup


ui <- fluidPage(
      titlePanel("Meteor data"),
      sliderInput(inputId = "mass",label = "Choose mass in grams",value = c(0,max(meteor$mass..g.)), min = 0, max = max(meteor$mass..g.)),
      sliderInput(inputId = "Year",label = "Year",value = c(min(meteor$Year),max(meteor$Year)), min = min(meteor$Year), max = max(meteor$Year)),
      uiOutput("typeSelectOutput"),
      plotOutput("map", width = 1400, height = 1100),
      dataTableOutput("results")
)


server <- function(input, output) {
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput", "Fallen vs Found meteor",
                sort(unique(meteor$fall)),
                multiple = TRUE,
                selected = c("Fell", "Found"))
  })
  
  filtered <- reactive({
    if (is.null(input$mass)) {
      return(NULL)
    }    
    
    meteor %>%
      filter(mass..g. >= input$mass[1],
             mass..g. <= input$mass[2],
             Year >= input$Year[1],
             Year <= input$Year[2],
             fall == input$typeInput
      )
  })
 
  output$map <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    base_world + geom_point(data = filtered(),aes(x = reclong,y=reclat, color = fall),size = 1) + theme(legend.position = "bottom")
  })
  output$results <- renderDataTable({
    filtered()
  })
  
}

shinyApp(ui = ui, server = server)