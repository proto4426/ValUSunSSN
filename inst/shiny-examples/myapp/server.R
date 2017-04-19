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
data("ssn_splines")

# z1 are the filled data
z1 <- cbind(as.data.frame(SSN_filled_all),
            Date = data.mat$Date, x = "Filled")

colnames(data.mat2.fin) <- gsub("-", "", colnames(data.mat2.fin))
colnames(z1) <- c( colnames(data.mat2.fin), "Date", "x")


# y1 are the unfilled data
y1 <- cbind(as.data.frame(data.mat2.fin), Date = data.mat$Date, x = "Raw")
rownames(y1) <- rownames(z1) <- 1:nrow(z1)
colnames(y1) <- colnames(z1)

silsoSSN <- cbind.data.frame(silsoSSN, Date = data.mat$Date)



## For comparisons with chris :
data("ssn_chris") # Data filled by Chris's method
data("z_final60") # Data filled by our method (from 1960)

ssn_chris <- ssn_chris[,3:61]
colnames(ssn_chris) <- substring(colnames(ssn_chris), 1, 7)
colnames(ssn_chris) <- gsub("-", "", colnames(ssn_chris))
colnames(ssn_chris) <- gsub("_", "", colnames(ssn_chris))
colnames(ssn_chris) <- gsub("[.]", "", colnames(ssn_chris))
colnames(ssn_chris)  <- gsub(".{0}d$", "", colnames(ssn_chris))
# Do manually some of the mismatches
colnames(ssn_chris['wnUC2ol']) <- 'wnUC2old'


z_chris <- cbind.data.frame(z_final60,
                            Date = data.mat[data.mat$decdate>1960 & data.mat$decdate<2015,]$Date)
colnames(z_chris) <- c( colnames(data.mat2.fin), "Date")

ssn_chris <- cbind.data.frame(ssn_chris, Date = z_chris$Date)



## For splines

splinesdf <- cbind(as.data.frame(ssn_splines), Date = data.mat$Date)
colnames(splinesdf) <- c( colnames(data.mat2.fin), "Date")




# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$plot1 <- renderPlot({
    x    <- z1
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    g <- ggplot(x, aes_string(x = "Date", y = input$stations)) +
      theme_piss() +  ggtitle('Filled  with interpolsvd_em() for 1st station')
    # print(ggplotly(g))
    # plot_ly(data = x, x = ~Date, y = ~input$stations, type = "scattergl") %>%
    #         #color = ~x, colors = c("green", "blue", "red"))
    #   layout(xaxis = xl, yaxis = yl, title = tit, legend = l)
    g2 <- ggplot(x, aes_string(x = "Date", y = input$stations2)) +
      theme_piss() + ggtitle('Filled with interpolsvd_em() for 2nd station')

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

    if(input$na == "chris") {
      gz1 <- ggplot(z_chris, aes_string(x = "Date", y = input$stations)) +
        theme_piss() +  ggtitle('Data from interpol_svd() of 1st station')
      gchris1 <- ggplot(ssn_chris, aes_string(x = "Date", y = input$stations)) +
        theme_piss() +  ggtitle('Data from Chris method of 1st station')

      gz2 <- ggplot(z_chris, aes_string(x = "Date", y = input$stations2)) +
        theme_piss() +  ggtitle('Data from interpol_svd() of 2nd station')
      gchris2 <- ggplot(ssn_chris, aes_string(x = "Date", y = input$stations2)) +
        theme_piss() +  ggtitle('Data from Chris method of 2nd station')

      if(input$points == T){
        gz1 <- gz1 + geom_point( size = 0.2)
        gchris1 <- gchris1 + geom_point(col = "red", size=0.2)

        gz2 <- gz2 + geom_point( size = 0.2)
        gchris2 <- gchris2 + geom_point(col = "red", size=0.2)
      }
      else{
        gz1 <- gz1 + geom_line() ; gchris1 <- gchris1 + geom_line( col = "red")
        gz2 <- gz2 + geom_line() ; gchris2 <- gchris2 + geom_line( col = "red")
      }

      gz1 <- gz1 + solar.cycle()    ;    gchris1 <- gchris1 + solar.cycle()
      gz2 <- gz2 + solar.cycle()    ;    gchris2 <- gchris2 + solar.cycle()


      grid.arrange(gz1, gchris1, gz2, gchris2, nrow = 2,
                   top = textGrob(expression(" Comparison of SSN + solar cycle "),
                                  gp = gpar(fontsize = 25, font = 3, col ="#33666C")))

    }

    # Comparisons with Splines method

    if(input$na == "splines") {
      y <- splinesdf

      g_splines <- ggplot(y, aes_string(x = "Date", y = input$stations)) +
        theme_piss() +  ggtitle('Filled with splines for 1st station')
      g_splines2 <- ggplot(y, aes_string(x = "Date", y = input$stations2)) +
        theme_piss() +  ggtitle('Filled with splines for 2nd station')


      if(input$points == T){
        g_splines2 <- g_splines2 + geom_point(col = "blue", size = 0.2)
        g_splines <- g_splines + geom_point(col = "blue", size=0.2)
      }
      else{
        g_splines2 <- g_splines2 + geom_line(col = "blue")
        g_splines <- g_splines + geom_line( col = "blue")
      }

      g_splines2 <- g_splines2 + solar.cycle()
      g_splines <- g_splines + solar.cycle()

      grid.arrange(g, g_splines, g2, g_splines2, nrow = 2,
                   top = textGrob(expression(" Comparison of SSN + solar cycle "),
                                  gp = gpar(fontsize = 25, font = 3, col ="#33666C")))
    }


  })

})

#shinyApp(ui = ui, server = server)
