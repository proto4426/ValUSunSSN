library(shiny)
library(ValUSunSSN)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)
#options(shiny.error = browser)

load('/home/piss/PissoortRepo/ValUSunSSN/Data/DataSSN.RData')
#x <- data.mat2[, "wnUC2"]
z1 <- cbind(as.data.frame(zssn), Date = data.mat$Date, x = "Filled")
colnames(z1) <- c( colnames(data.mat2.fin), "Date", "x")

colnames(z1) <- gsub("-", "", colnames(z1))

rownames(y) <- rownames(zssn) <- 1:nrow(y)
y1 <- cbind(as.data.frame(data.mat2.fin), Date = data.mat$Date, x = "Raw")
colnames(y1) <- colnames(z1)



ui <- fluidPage(

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
)
