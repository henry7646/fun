# Separate R Script file containing ui to be used for creating R Shiny Dashboard later. You can call this R Script file from the other R Script file via the following code:
# source("~/ui.R")

ui <- fluidPage(
  titlePanel("How Fat am I?: Comparing My Waist-Height Ratio(WHtR) with Others'"),
  sidebarLayout(
    sidebarPanel(
      numericInput("waist","Waist cirumference(cm):",20,145,.1),
      numericInput("height","Height(cm):",130,200,.1),
      ),
    mainPanel(
      textOutput("q"),
      plotOutput("r")
      )
  )
)
