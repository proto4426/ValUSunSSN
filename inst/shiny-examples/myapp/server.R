library(ValUSunSSN)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)
#options(shiny.error = browser)

#load('/home/piss/PissoortRepo/ValUSunSSN/data/dataSSN_all.RData')

data("data.mat")
data("data.mat2.fin")
data("SSN_filled_all")
data("silsoSSN")


# z1 are the filled data
z1 <- cbind(as.data.frame(SSN_filled_all),
            Date = data.mat$Date, x = "Filled")
colnames(z1) <- c( colnames(data.mat2.fin), "Date", "x")

colnames(z1) <- gsub("-", "", colnames(z1))

# y1 are the unfilled data
y1 <- cbind(as.data.frame(data.mat2.fin), Date = data.mat$Date, x = "Raw")
rownames(y1) <- rownames(z1) <- 1:nrow(z1)
colnames(y1) <- colnames(z1)

silsoSSN <- cbind.data.frame(silsoSSN, Date = data.mat$Date)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$plot1 <- renderPlot({
    x    <- z1
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    g <- ggplot(x, aes_string(x = "Date", y = input$stations)) +
       theme_piss() +  ggtitle('Filled data from 1st station')
    # print(ggplotly(g))
    # plot_ly(data = x, x = ~Date, y = ~input$stations, type = "scattergl") %>%
    #         #color = ~x, colors = c("green", "blue", "red"))
    #   layout(xaxis = xl, yaxis = yl, title = tit, legend = l)
    g2 <- ggplot(x, aes_string(x = "Date", y = input$stations2)) +
       theme_piss() + ggtitle('Filled data from 2nd station')

    if(input$points == T){
      g2 <- g2 + geom_point(size = 0.2)   ;       g <- g + geom_point(size=0.2)
    }
   else{
    g2 <- g2 + geom_line()   ;      g <- g + geom_line()
   }

    g <- g + solar.cycle()    ;    g2 <- g2 + solar.cycle()

    #### Plot Raw data (without filling)
    if (input$na == "filled")
      grid.arrange(g, g2, ncol = 1,top = textGrob(expression(" Comparison of SSN + solar cycle "),
                                                  gp = gpar(fontsize = 22, font = 3, col ="#33666C")))
    if(input$na == "raw") {
      y <- y1

      gg <- ggplot(y, aes_string(x = "Date", y = input$stations)) +
        theme_piss() +  ggtitle('Raw data of 1st station')
      gg2 <- ggplot(y, aes_string(x = "Date", y = input$stations2)) +
        theme_piss() +  ggtitle('Raw data of 2nd station')


      if(input$points == T){
        gg2 <- gg2 + geom_point(col = "red", size = 0.2)
        gg <- gg + geom_point(col = "red", size=0.2)
      }
      else{
        gg2 <- gg2 + geom_line(col = "red")   ;    gg <- gg + geom_line( col = "red")
      }

      gg2 <- gg2 + solar.cycle()    ;    gg <- gg + solar.cycle()

      grid.arrange(g, gg, g2, gg2, nrow = 2,
                   top = textGrob(expression(" Comparison of SSN + solar cycle "),
                                  gp = gpar(fontsize = 25, font = 3, col ="#33666C")))

    }


    if(input$na == "silso"){
      g_silso <- ggplot(silsoSSN, aes_string(x = "Date", y = "SSN")) +
      theme_piss() +  ggtitle('SSN coming from Silso ')

      if(input$points == T){
        g_silso <- g_silso + geom_point(col = "green", size=0.2)
      }
      else{
        g_silso <- g_silso + geom_line(col = "green")
      }

      g_silso <- g_silso + solar.cycle()

    grid.arrange(g, g2, g_silso, g_silso, ncol = 2,
                 top = textGrob(expression(" Comparison of SSN + solar cycle"),
                                gp = gpar(fontsize = 25, font = 3, col ="#33666C")))
    }

    ## Data from Chris !



  })

})

#shinyApp(ui = ui, server = server)
