library(shiny)

#options(shiny.error = browser)

#load('/home/piss/PissoortRepo/ValUSunSSN/data/Filled/DataSSN.RData')
#x <- data.mat2[, "wnUC2"]

data("data.mat")
data("data.mat2.fin")
data("SSN_filled_all")


z1 <- cbind(as.data.frame(SSN_filled_all), Date = data.mat$Date, x = "Filled")
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

library(ValUSunSSN)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
 if (input$na == T) grid.arrange(g, g2, ncol = 2,
                                 top = textGrob(expression(" Comparison of SSN + solar cycle "),
                                                gp = gpar(fontsize = 22, font = 3,
                                                          col ="#33666C")))
    else {
    y <- y1

    gg <- ggplot(y) +
      geom_line(aes_string(x = "Date", y = input$stations), col = "red") +
      theme_piss() + solar.cycle() + ggtitle('Raw data ')
    gg2 <- ggplot(y) +
      geom_line(aes_string(x = "Date", y = input$stations2), col = "red") +
      theme_piss() + solar.cycle() + ggtitle('Raw data ')

    grid.arrange(g, gg, g2, gg2, ncol = 2,
                 top = textGrob(expression(" Comparison of SSN + solar cycle "),
                                gp = gpar(fontsize = 25, font = 3,
                                          col ="#33666C")))
    }
  })

}

shinyApp(ui = ui, server = server)


#runApp('/home/piss/PissoortRepo/ValUSunSSN/Shiny_visual_app/appTest')

