library(shiny)

#load('/home/piss/PissoortRepo/ValUSunSSN/data/Filled/DataSSN.RData')
data("data.mat")
data("data.mat2.fin")
data("SSN_filled_all")

names_stations <- colnames(data.mat2.fin)


shinyUI(fluidPage(

  # Application title
  titlePanel("Shiny app for Val-U-Sun project!"),

  # Sidebar with a slider input for the number of bins
  #sidebarLayout(
  # sidebarPanel(
  selectInput("stations",
              "Which Station ? (1st row)",
              names_stations),
  selectInput("stations2",
              "Which second Station ? (2nd row)",
              names_stations),
  # width = "150px",
  #)
  checkboxInput("na", "Show only filled values?"),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot1", height = '500px', width = "900px")
  )
  #)
))
