library(shiny)

load('/home/piss/Documents/Consu LSTAT2390 /Q2/data/matlab/MatlabR2016/DataSSN.RData') 
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
  sidebarLayout(
    sidebarPanel(
      selectInput("stations",
                  "Which Station ? (above)",
                  colnames(z1[,1:52])),
      selectInput("stations2",
                  "Which second Station ? (below)",
                  colnames(z1[,1:52])), 
      width = "150px"
    ),
    checkboxInput("na", "Show missing values ?"),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1", height = '600px', width = "900px")
    )
  )
)


library(ggplot2)
library(ValUSunSSN)
library(plotly)
library(gridExtra)
library(grid)

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
            theme_piss() + solar.cycle()
    # print(ggplotly(g))
    # plot_ly(data = x, x = ~Date, y = ~input$stations, type = "scattergl") %>%
    #         #color = ~x, colors = c("green", "blue", "red")) 
    #   layout(xaxis = xl, yaxis = yl, title = tit, legend = l)
    g2 <- ggplot(x) + 
      geom_line(aes_string(x = "Date", y = input$stations2)) +
      theme_piss() + solar.cycle()

    #### Plot Raw data (without filling)
    y <- y1 

    gg <- ggplot(y) + 
      geom_line(aes_string(x = "Date", y = input$stations), col = "red") +
      theme_piss() + solar.cycle() 
    gg2 <- ggplot(y) +  
      geom_line(aes_string(x = "Date", y = input$stations2), col = "red") +
      theme_piss() + solar.cycle() 
    
    grid.arrange(g, gg, g2, gg2, ncol = 2,
                 top = textGrob(expression(" Comparison of ns + solar cycle "),
                                                     gp = gpar(fontsize = 22, font = 3, 
                                                     col ="#33666C")))
  })

}

shinyApp(ui = ui, server = server)


runApp('/home/piss/Documents/Shiny/appTest')

