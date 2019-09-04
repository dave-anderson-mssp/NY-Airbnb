

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "NYC Airbnb"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Neighborhoods", tabName = "neighborhoods", icon = icon("building")),
      menuItem("Boroughs", tabName = "boroughs", icon = icon("city")),
      menuItem("Individual",tabName = "individual",icon = icon("home"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "neighborhoods",
              fluidPage(
                leafletOutput("mymap")
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    neighborhoods <- readRDS("neighborhood.rds")
    neighborhoods <- neighborhoods %>% filter(total > 50)
    
    getColor <- function(neighborhoods) {
      sapply(neighborhoods$price, function(price) {
        if(price <= 100) {
          "green"
        } else if(price <= 200) {
          "orange"
        } else {
          "red"
        } })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(neighborhoods)
    )
    
    leaflet(neighborhoods) %>% addTiles() %>%
      addAwesomeMarkers(~long, ~lat, icon=icons, popup = paste("Neighborhood:", neighborhoods$neighbourhood, "<br>","Avg. Price:", round(neighborhoods$price,2)))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

