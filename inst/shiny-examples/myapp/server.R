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
