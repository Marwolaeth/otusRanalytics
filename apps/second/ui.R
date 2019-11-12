shinyUI(fluidPage(
  titlePanel("Basic widgets"),
  
  fluidRow(
    
    column(3,
           h3("IQ", align = 'center'),
           sliderInput("slider_iq", label = '',
                       min = 40, max = 160, value = c(90, 110))
    )
  )  
  
))
