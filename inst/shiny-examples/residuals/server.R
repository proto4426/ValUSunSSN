library(ValUSunSSN)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)
library(RColorBrewer)
#options(shiny.error = browser)


data("data.mat")
data("data.mat2.fin")
data("SSN_filled_all")
data("silsoSSN")
data("ssn_splines")

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



#### For the differences

# 1) with Chris 
colnames(z_final60) <- colnames(data.mat2.fin)
diff_chris <- as.data.frame(z_final60 - ssn_chris[,colnames(z_final60) %in% colnames(ssn_chris)])
# Keep only matched stations (problem with name encoding)
diff_chris <- cbind(diff_chris,
                    Date = data.mat[data.mat$decdate>1960 & data.mat$decdate<2015,]$Date)

# 2) With silso 
#diff_silso <- as.data.frame(sweep(SSN_filled_all, 2, silsoSSN$SSN,  "-"))
#diff_silsoo <- apply(SSN_filled_all, 2, function(x) x - silsoSSN$SSN)
diff_silso <- as.data.frame(SSN_filled_all - silsoSSN$SSN)
colnames(diff_silso) <- colnames(data.mat2.fin)
diff_silso <- cbind(diff_silso, Date = data.mat$Date)

# 3) with splines 
diff_splines <- as.data.frame(SSN_filled_all - ssn_splines)
colnames(diff_splines) <- colnames(data.mat2.fin)
diff_splines <- cbind(diff_splines,  Date = data.mat$Date)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    
    myPalette <- colorRampPalette(rev(brewer.pal(4, "Spectral")))
    
    
    ## 1) Data from Chris !
    
    if(input$na == "chris") {
      
      # Have the same coordinates for the compared graphs
      xylim <- coord_cartesian(ylim = c(min(diff_chris[,input$stations],
                                            diff_chris[,input$stations2]),
                                        max(diff_chris[,input$stations],
                                               diff_chris[,input$stations2])))
      
      
      gchris1 <- ggplot(diff_chris, aes_string(x = "Date", y = input$stations, 
                                               col = input$stations)) +
        theme_piss() +  ggtitle('Residuals with Chris method for 1st station') +
        xylim 
      gchris2 <- ggplot(diff_chris, aes_string(x = "Date", y = input$stations2,
                                               col = input$stations2)) +
        theme_piss() +  ggtitle('Residuals with Chris method for 2nd station') + 
        xylim

      
      
      if(input$points == T){
        gchris1 <- gchris1 + geom_point( size = 0.1)
        gchris2 <- gchris2 + geom_point(size=0.1)
      }
      else{
       gchris1 <- gchris1 + geom_line( )
       gchris2 <- gchris2 + geom_line()
      }
      
      gchris1 <- gchris1 + solar.cycle() +
        scale_colour_gradientn(colours = myPalette(10), limits=c(-100, 100))
      gchris2 <- gchris2 + solar.cycle() +
        scale_colour_gradientn(colours = myPalette(10), limits=c(-100, 100))
      
      grid.arrange(gchris1, gchris2, nrow = 2,
                   top = textGrob(expression(" Comparison of SSN methods + solar cycle "),
                                  gp = gpar(fontsize = 25, font = 3, col ="red")))
    }
    
    
    # 2) Silso :
    
    if(input$na == "silso"){
      
      xylim <- coord_cartesian(ylim = c(min(diff_silso[,input$stations],
                                            diff_silso[,input$stations2]),
                                        max(diff_silso[,input$stations],
                                               diff_silso[,input$stations2])))
      
      
      g_silso1 <- ggplot(diff_silso, aes_string(x = "Date", y = input$stations,
                                                col = input$stations)) +
        theme_piss() +  ggtitle('SSN coming from Silso ') + 
        xylim
      g_silso2 <- ggplot(diff_silso, aes_string(x = "Date", y = input$stations2,
                                                col = input$stations2)) +
        theme_piss() +  ggtitle('SSN coming from Silso ') + 
        xylim
      
      if(input$points == T){
        g_silso1 <- g_silso1 + geom_point( size=0.1)
        g_silso2 <- g_silso2 + geom_point(size=0.1)
        
      }
      else{
        g_silso1 <- g_silso1 + geom_line()
        g_silso2 <- g_silso2 + geom_line()
        
      }
      
      g_silso1 <- g_silso1 + solar.cycle() +
        scale_colour_gradientn(colours = myPalette(10), limits=c(-100, 100))
      g_silso2 <- g_silso2 + solar.cycle() +
        scale_colour_gradientn(colours = myPalette(10), limits=c(-100, 100))
      
      
      grid.arrange(g_silso1, g_silso2, ncol = 1,
                   top = textGrob(expression(" Comparison of SSN methods + solar cycle"),
                                  gp = gpar(fontsize = 25, font = 3, col ="red")))
    }
    
    
    # 3) Comparisons with Splines method
    
    if(input$na == "splines") {

      # Have the same coordinates for the compared graphs
      xylim <- coord_cartesian(ylim = c(min(diff_splines[,input$stations],
                                            diff_splines[,input$stations2]),
                                        max(diff_splines[,input$stations],
                                               diff_splines[,input$stations2])))
      
      
      g_splines <- ggplot(diff_splines, aes_string(x = "Date", y = input$stations,
                                                   col = input$stations)) +
        theme_piss() +  ggtitle('Filled with splines for 1st station') + 
        xylim #+ coord_cartesian(ylim = c(0,100))
      g_splines2 <- ggplot(diff_splines, aes_string(x = "Date", y = input$stations2,
                                                    col = input$stations2)) +
        theme_piss() +  ggtitle('Filled with splines for 2nd station') +
        xylim
      
      
      if(input$points == T){
        g_splines2 <- g_splines2 + geom_point(, size = 0.1)
        g_splines <- g_splines + geom_point( size=0.1)
      }
      else{
        g_splines2 <- g_splines2 + geom_line()
        g_splines <- g_splines + geom_line( )
      }
      
      g_splines2 <- g_splines2 + solar.cycle() +
        scale_colour_gradientn(colours = myPalette(10), limits=c(-100, 100))
      g_splines <- g_splines + solar.cycle() +
        scale_colour_gradientn(colours = myPalette(10), limits=c(-100, 100))
      
      grid.arrange(g_splines, g_splines2, nrow = 2,
                   top = textGrob(expression(" Comparison of SSN methods + solar cycle "),
                                  gp = gpar(fontsize = 25, font = 3, col ="red")))
    }
    
    
  })
  
})

#shinyApp(ui = ui, server = server)
