library(shiny)
load('/home/piss/PissoortRepo/ValUSunSSN/data/Filled/DataSSN.RData')


shinyUI(fluidPage(

  # Application title
  titlePanel("Shiny app for Val-U-Sun project!"),

  # Sidebar with a slider input for the number of bins
  #sidebarLayout(
  # sidebarPanel(
  selectInput("stations",
              "Which Station ? (above)",
              colnames(z1[,1:52])),
  selectInput("stations2",
              "Which second Station ? (below)",
              colnames(z1[,1:52])),
  # width = "150px",
  #)
  checkboxInput("na", "Show only filled values?"),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot1", height = '500px', width = "900px")
  )
  #)
))
