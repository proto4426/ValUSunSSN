library(shiny)
library(ValUSunSSN)


data("data.mat2.fin")


names_stations <- colnames(data.mat2.fin)
names_stations <- gsub("-", "", names_stations)


shinyUI(fluidPage(

  titlePanel("Val-U-Sun project! Compare 2 methods by the residuals"),

  # Sidebar with a slider input for the number of bins
  fluidRow(
    column(3,
           selectInput("stations",
                       "Which Station ? (1st row)",
                       names_stations),
           selectInput("stations2",
                       "Which second Station ? (2nd row)",
                       names_stations)
    ),
    width = "100px",
    #)
    # checkboxInput("na", "Show only filled values?"),
    column(3, offset = 1,
           selectInput("na", "Compare interpolsvd with ?",
                       c("Filled from Chris" = "chris",
                         "Silso" = "silso",
                          "Filled with splines" = "splines")),
           checkboxInput("points", "Points ?"),
           checkboxInput("percent", "Nomalize by Silso ?")
           # numericInput("size", "Size of points", value = 0.2, min = 0, max = 1)
    ),

    # Show a plot of the generated distribution
    #mainPanel(
    plotOutput("plot1", height = '650px', width = "900px")
    #)

  )
))
