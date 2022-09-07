

WeeklyWeather <- read_excel('weeklyweather.xlsx')

ui <- fluidPage(
  
  titlePanel("Football Weekly Weather"),
  
  mainPanel(
    dataTableOutput("WeeklyWeather")
  )
)


server <- function(input, output){
  
  output$WeeklyWeather <- DT::renderDataTable(
    WeeklyWeather, 
    extensions = 'Buttons', 
    options = list(dom = 'Bfrtip',
                   pageLength = 360,
                   buttons = list('copy', 'excel', 'csv'))
  )
  
}

shinyApp(ui = ui, server = server)








