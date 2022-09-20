library(shiny)
library(ggplot2)
library(dplyr)

# on hover cambie el color a gris
# on click cambie el color a verde
# on doble click quite el color
# on brush cambie el color a los puntos que están dentro de la seleccion

# on click mostrar la información del punto en una tabla. check
# on brush mostrar todos los puntos seleccionados. check

df11 <- NULL
df22 <- NULL
df33 <- NULL
df44 <- NULL
mtcars$n <- 1:nrow(mtcars)

shinyServer(function(input, output) {
  
  output$click_data <- renderPrint({
    clk_msg <- NULL
    mhover_msg <- NULL
    mbrush_msg <- NULL
    dclk_msg<- NULL
    
    if(!is.null(input$clk$x) ){
      clk_msg<-paste0("click cordenada x= ", round(input$clk$x,2), 
               " click coordenada y= ", round(input$clk$y,2))
      
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
  
  dfc <- reactive({
    df1 <- nearPoints(mtcars,input$clk,xvar='wt',yvar='mpg')
    
    if(is.null(df11)){
      df11 <<- rbind(df11, df1)
      return(df11[,0:11])
    } else {
      dfd <- df11 %>%
        filter_all(any_vars(. %in% df1$n))
      if(nrow(dfd)==0){
        df11 <<- rbind(df11, df1)
      }
      if(!is.null(input$dclk)){
        dfcd <- nearPoints(mtcars,input$dclk,xvar='wt',yvar='mpg')
        df11 <<- df11[!(df11$n %in% dfcd$n),]
        return(df11[,0:11])
      }
      return(df11[,0:11])
    }
    
    
    
  })
  
  dfm <- reactive({
    df2 <- brushedPoints(mtcars,input$mbrush,xvar='wt',yvar='mpg')
    if(is.null(df22)){
      df22 <<- rbind(df22, df2)
      return(df22[,0:11])
    } else {
      dfd <- df22 %>%
        filter_all(any_vars(. %in% df2$n))
      df2 <- df2[ !(df2$n %in% dfd$n), ]
      if(nrow(dfd)==0){
        df22 <<- rbind(df22, df2)
      }
      if(!is.null(input$dclk)){
        dfcd <- nearPoints(mtcars,input$dclk,xvar='wt',yvar='mpg')
        df22 <<- df22[!(df22$n %in% dfcd$n),]
        return(df22[,0:11])
      }
      return(df22[,0:11])
    }
  })
  
  dfh <- reactive({
    df3 <- nearPoints(mtcars,input$mhover,xvar='wt',yvar='mpg')
    
    if(is.null(df33)){
      df33 <<- rbind(df33, df3)
      return(df33[,0:11])
    } else {
      dfd <- df33 %>%
        filter_all(any_vars(. %in% df3$n))
      if(nrow(dfd)==0){
        df33 <<- rbind(df33, df3)
      }
      if(!is.null(input$dclk)){
        dfcd <- nearPoints(mtcars,input$dclk,xvar='wt',yvar='mpg')
        df33 <<- df33[!(df33$n %in% dfcd$n),]
        return(df33[,0:11])
      }
      return(df33[,0:11])
    }
  })
  
  dfdc <- reactive({
    df4 <- nearPoints(mtcars,input$dclk,xvar='wt',yvar='mpg')
    
    if(is.null(df44)){
      df44 <<- rbind(df44, df4)
      return(df44[,0:11])
    } else {
      dfd <- df44 %>%
        filter_all(any_vars(. %in% df4$n))
      if(nrow(dfd)==0){
        df44 <<- rbind(df44, df4)
      }
      return(df44[,0:11])
    }
  })
  
  
  #--------------------
  
  output$mtcars_tbl_clk <- renderTable({
    dfc()
  })
  
  output$mtcars_tbl_mbrush <- renderTable({
    dfm()
  })
  
  output$mtcars_tbl_dclk<- renderTable({
    dfdc()
  })
  
  output$mtcars_tbl_mhover<- renderTable({
    dfh()
  })
  
  dfg <- reactive({
    plot(mtcars$wt,mtcars$mpg, xlab = "wt", ylab="millas por galon")
    pc <- dfc()
    pb <- dfm()
    ph <- dfh()
    pdc <- dfdc()
    points(ph$wt, ph$mpg, col="gray", pch = 19)
    points(pc$wt, pc$mpg, col="green", pch = 19)
    points(pb$wt, pb$mpg, col="green", pch = 19)
    
  })
  
  
  output$plot_click_options <- renderPlot({
    dfg()
    #plot(mtcars$wt,mtcars$mpg, xlab = "wt", ylab="millas por galon")
  })
  
  
})
