library(shiny)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Graficas"),
  
  # Sidebar with a slider input for number of bins
  shiny::tabsetPanel(
    tabPanel("Plot Interactions",
             plotOutput("plot_click_options",
                        click = "clk",
                        dblclick = "dclk",
                        hover = 'mhover',
                        brush = 'mbrush' ),
             verbatimTextOutput("click_data"),
             titlePanel("Click"),
             tableOutput("mtcars_tbl_clk"),
             titlePanel("Doble"),
             tableOutput("mtcars_tbl_dclk"),
             titlePanel("Hover"),
             tableOutput("mtcars_tbl_mhover"),
             titlePanel("Brush"),
             tableOutput("mtcars_tbl_mbrush")
    )
    
  )
))
