library(shiny)

load('/home/piss/Documents/Consu LSTAT2390 /Q2/data/matlab/MatlabR2016/DataSSN.RData') 
#x <- data.mat2[, "wnUC2"]
z1 <- cbind(as.data.frame(zssn), Date = data.mat$Date, x = "Filled")
colnames(z1) <- c(colnames(data.mat2.fin), "Date", "x")



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny app for Val-U-Sun project!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("stations",
                  "Which Station ?",
                  colnames(z1[,1:52]))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1")
    )
  )
)

library(ggplot2)
library(ValUSunSSN)
library(plotly)


f <- list(
  family = "Calibri",
  size = 16,
  color = "#33666C"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "black"
)
xl <- list(
  title = " Date",
  titlefont = f,
  tickfont = f2,
  showticklabels = TRUE
)
yl <- list(
  title = "SSN for wnUC2",
  titlefont = f,
  tickfont = f2,
  showticklabels = TRUE
)
tit <- list(
  title = "Dynamic plot of the SSN for  the station of Uccle",
  titlefont = f
)
l <- list(
  font = list(
    family = "sans-serif",
    size = 13,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$plot1 <- renderPlot({
    x    <- z1  
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    g <- ggplot(x) +
           geom_line(aes_string(x = "Date", y = input$stations)) +
           ggtitle("hist") + theme_piss()
    g
    # print(ggplotly(g))
    # plot_ly(data = x, x = ~Date, y = ~input$stations, type = "scattergl") %>%
    #         #color = ~x, colors = c("green", "blue", "red")) 
    #   layout(xaxis = xl, yaxis = yl, title = tit, legend = l)
  })
}

shinyApp(ui = ui, server = server)


runApp('/home/piss/Documents/Shiny/appTest')

