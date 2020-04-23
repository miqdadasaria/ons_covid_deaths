############################################################################################
# Shiny app for simple SEIR model implementation
#
# Miqdad Asaria
# April 2020
############################################################################################

library("shiny")
library("DT")

source("ons_covid_deaths.R")

ui = fluidPage(
  titlePanel("ONS Covid Deaths Explorer"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Select local authority"),
      selectInput("local_authority", 
                  "Local authority:", 
                  get_la_list(), 
                  selected = c("Harrow"), multiple = FALSE),
      tags$div(
        HTML("<small><small>
         <p>This site was produced by <a href='http://www.lse.ac.uk/lse-health/people/miqdad-asaria'>Miqdad Asaria</a> 
         </small></small>")
      )
    ),
    
    mainPanel(
      tabsetPanel(id="tabset",
                  tabPanel("Deaths in local authority", plotOutput("ons_death_plot")),
                  tabPanel("Raw data", div(dataTableOutput("raw_data"), style = "font-size:70%"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server = function(input, output) {
  
  output$ons_death_plot = renderPlot({
    plot_la_deaths(input$local_authority)
  })
  
  output$raw_data = renderDataTable({
    datatable(deaths,
              style = 'bootstrap',
              rownames = FALSE,
              options = list(pageLength = 20, autoWidth = TRUE, dom='ftrpi'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
