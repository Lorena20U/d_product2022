library(shiny)
library(ggplot2)
library(dplyr)

# on hover cambie el color a gris
# on click cambie el color a verde
# on doble click quite el color
# on brush cambie el color a los puntos que están dentro de la seleccion

# on click mostrar la información del punto en una tabla. check
# on brush mostrar todos los puntos seleccionados. check

shinyServer(function(input, output) {
  
  output$grafica_base_r <- renderPlot({
    plot(mtcars$wt,mtcars$mpg, xlab = "wt", ylab="millas por galon")
    
  })
  
  
  output$grafica_ggplot <- renderPlot({
    diamonds %>%
      ggplot(aes(x=carat,y=price,color=color))+
      geom_point()+
      ylab("Precio")+
      xlab("Kilates")+
      ggtitle("Precio de Diamantes por kilate")
  })
  
  
  
  output$click_data <- renderPrint({
    clk_msg <- NULL
    dclk_msg<- NULL
    mhover_msg <- NULL
    mbrush_msg <- NULL
    df <- matrix(ncol = 3)
    df <- data.frame(df)
    if(!is.null(input$clk$x) ){
      clk_msg<-
        paste0("click cordenada x= ", round(input$clk$x,2), 
               " click coordenada y= ", round(input$clk$y,2))
      df <- data.frame(X1 = round(input$clk$x,2),
                       X2 = round(input$clk$x,2),
                       X3 = "green")
      
    }
    if(!is.null(input$dclk$x) ){
      dclk_msg<-paste0("doble click cordenada x= ", round(input$dclk$x,2), 
                       " doble click coordenada y= ", round(input$dclk$y,2))
    }
    if(!is.null(input$mhover$x) ){
      mhover_msg<-paste0("hover cordenada x= ", round(input$mhover$x,2), 
                         " hover coordenada y= ", round(input$mhover$y,2))
    }
    
    
    if(!is.null(input$mbrush$xmin)){
      brushx <- paste0(c('(',round(input$mbrush$xmin,2),',',round(input$mbrush$xmax,2),')'),collapse = '')
      brushy <- paste0(c('(',round(input$mbrush$ymin,2),',',round(input$mbrush$ymax,2),')'),collapse = '')
      mbrush_msg <- cat('\t rango en x: ', brushx,'\n','\t rango en y: ', brushy)
    }
    
    cat(clk_msg,dclk_msg,mhover_msg,mbrush_msg,sep = '\n')
    
  })
  
  
  
  output$mtcars_tbl <- renderTable({
    df1 <- nearPoints(mtcars,input$clk,xvar='wt',yvar='mpg')
    df2 <- brushedPoints(mtcars,input$mbrush,xvar='wt',yvar='mpg')
    df3 <- nearPoints(mtcars,input$dclk,xvar='wt',yvar='mpg')
    df4 <- nearPoints(mtcars,input$mhover,xvar='wt',yvar='mpg')
    
    if(nrow(df1)!=0){
      df1
    } else {
      if(nrow(df2)!=0){
        df2
      } else {
        NULL
      }
    }
    
  })
  
  
  
  
  output$plot_click_options <- renderPlot({
    
    plot(mtcars$wt,mtcars$mpg, xlab = "wt", ylab="millas por galon", 
         points(mtcars$wt[1:5], mtcars$mpg[1:5], col="red", pch=16))
  })
  
  
})