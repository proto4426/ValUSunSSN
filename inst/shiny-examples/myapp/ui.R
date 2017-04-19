library(shiny)

#load('/home/piss/PissoortRepo/ValUSunSSN/data/Filled/DataSSN.RData')
#data("data.mat")
data("data.mat2.fin")
#data("SSN_filled_all")
#data("silsoSSN")

names_stations <- colnames(data.mat2.fin)
names_stations <- gsub("-", "", names_stations)


shinyUI(fluidPage(

  # Application title
 titlePanel("Shiny app for Val-U-Sun project!"),


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
         selectInput("na", "Compare with ?",
                     c( "Raw data" = "raw", "Filled only" = "filled",
                        "Silso: same serie everywhere (green)" = "silso",
                        "Filled from Chris" = "chris")),
   checkboxInput("points", "Points ?")
  # numericInput("size", "Size of points", value = 0.2, min = 0, max = 1)
   ),

  # Show a plot of the generated distribution
   #mainPanel(
     plotOutput("plot1", height = '650px', width = "900px")
   #)

 )
))
