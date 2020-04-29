############################################################################################
# Shiny app for simple SEIR model implementation
#
# Miqdad Asaria
# April 2020
############################################################################################

library("shiny")
library("DT")
library("plotly")
library("leaflet")

source("ons_covid_deaths.R")
source("phe_covid_cases.R")

ui = fluidPage(
  titlePanel("ONS COVID-19 Deaths Explorer"),
  sidebarLayout(
    sidebarPanel(
      tags$h4(paste0("Deaths up to ", deaths_date)),
      tags$h3("Select local authority"),
      selectInput("local_authority", 
                  "Local authority:", 
                  c("England and Wales",get_la_list()), 
                  selected = c("England and Wales"), multiple = FALSE),
      checkboxInput("show_all_cause","Show all cause deaths"),
      tags$div(
        HTML("<small><small>
         <p>This site was produced by <a href='http://www.lse.ac.uk/lse-health/people/miqdad-asaria'>Miqdad Asaria</a> 
         </small></small>")
      )
    ),
    
    mainPanel(
      tabsetPanel(id="tabset",
                  tabPanel("Map", leafletOutput("la_map", height="600px")),
                  tabPanel("Deaths in local authority (Total)", plotOutput("ons_death_plot", height="600px")),
                  tabPanel("Deaths in local authority (weekly)", plotlyOutput("ons_weekly_death_plot",height="500px")),
                  tabPanel("Deaths by BAME population (%)", plotlyOutput("ons_bame_death_plot",height="500px")),
                  tabPanel("Cases by BAME population (%)", plotlyOutput("phe_bame_cases_plot",height="500px")),
                  tabPanel("Raw data", div(dataTableOutput("raw_data"), style = "font-size:70%"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server = function(input, output) {
  
  output$ons_death_plot = renderPlot({
    plot_la_deaths(input$local_authority, input$show_all_cause)
  })
  
  output$ons_weekly_death_plot = renderPlotly({
    plot_la_deaths_by_week(input$local_authority, input$show_all_cause)
  })
 
  output$ons_bame_death_plot = renderPlotly({
    plot_la_ethnicity_deaths(input$show_all_cause)
  }) 
  
  output$phe_bame_cases_plot = renderPlotly({
    plot_cases()
  }) 
  
  output$raw_data = renderDataTable({
    datatable(raw_data(),
              filter = "top",
              style = 'bootstrap',
              rownames = FALSE,
              options = list(pageLength = 20, autoWidth = TRUE, dom='ftrpi'))
  })
  
  output$la_map = renderLeaflet({
    choropleth_map()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
