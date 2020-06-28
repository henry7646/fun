# Separate R Script file containing server to be used for creating R Shiny Dashboard later. You can call this R Script file from the other R Script file via the following code:
# source("~/server.R")

server <- function(input,output){
  output$q <- renderText({
    paste("Your WHtR is",round(input$waist/input$height,2),".")
  })
  output$r <- renderPlot({
    ggplot(obesity_measure_with_WHtR,aes(WHtR))+geom_density(fill = "blue", alpha = 0.5) + geom_vline(xintercept = input$waist/input$height, color = "red") + scale_x_continuous("WHtR") + ggtitle("Distribution of WHtR")
  })
}
