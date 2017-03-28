library(shiny)

load('/home/piss/Documents/Consu LSTAT2390 /Q2/data/matlab/MatlabR2016/DataSSN.RData') 
#x <- data.mat2[, "wnUC2"]
z1 <- cbind(as.data.frame(zssn), Date = data.mat$Date, x = "Filled")
colnames(z1) <- c( colnames(data.mat2.fin), "Date", "x")

colnames(z1) <- gsub("-", "", colnames(z1))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny app for Val-U-Sun project!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("stations",
                  "Which Station ?",
                  colnames(z1[,1:52])),
      selectInput("stations2",
                  "Which Station ?",
                  colnames(z1[,1:52]))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1", height = '800px')
    )
  )
)


library(ggplot2)
library(ValUSunSSN)
library(plotly)
library(gridExtra)

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
    # print(ggplotly(g))
    # plot_ly(data = x, x = ~Date, y = ~input$stations, type = "scattergl") %>%
    #         #color = ~x, colors = c("green", "blue", "red")) 
    #   layout(xaxis = xl, yaxis = yl, title = tit, legend = l)
    g2 <- ggplot(x) +
      geom_line(aes_string(x = "Date", y = input$stations2)) +
      ggtitle("hist") + theme_piss()

    #### Plot Raw data (without filling)
    
    rownames(y) <- rownames(zssn) <- 1:nrow(y)
    y1 <- cbind(as.data.frame(data.mat2.fin), Date = data.mat$Date, x = "Raw")
    
    gg <- ggplot(y1) + ggtitle("hist") + theme_piss() + 
      geom_line(aes_string(x = "Date", y = input$stations), col = "red") 
    gg2 <- ggplot(y1) + ggtitle("hist") + theme_piss() + 
      geom_line(aes_string(x = "Date", y = input$stations2), col = "red") 
    
    grid.arrange(g, gg, g2, gg2, ncol = 1, title = "Comparisons of")
  })

}

shinyApp(ui = ui, server = server)


runApp('/home/piss/Documents/Shiny/appTest')

