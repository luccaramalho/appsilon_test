library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(htmlwidgets)
library(dplyr)
library(data.table)
library(geojsonio)
library(jsonlite)
library(leaflet)
library(plotly)
library(scales)

#Import Data
occurencePL <<- fread("biodiversity-data/occurencePL.csv",encoding = 'UTF-8')
polandGeojson <<- geojson_read("geojson/poland.json")

ui <- fluidPage(

  useShinydashboard(),
  
  # Dashboard UI
  dashboardHeader(title = "Birds observation"),
  
  # Background image
  setBackgroundImage("background.jpg"),
  
  # Include CSS
  includeCSS("www/birds.css"),
  
  # Break Row
  tags$br(),
 
  # Map and sidebar UI Module
    mod_map_UI("map")
  
)

server <- function(input, output, session) {
  
  mod_map_server("map")
  
  
  # { MODAL DIALOG WHEN THE APP STARTS } ---------------------------------------
  welcome_modal <<- modalDialog(
    title = h3("Welcome to the birds observation app!"),
    h5(HTML("On this website, you'll find <b>birds observation registers</b> from <b>Poland</b> based on the <a href='https://www.gbif.org/'>Global biodiversity information facility</a> data.<br><br>
           <img src='bird_pin_md.png' align='right' alt='' />
            The app displays<b>",paste(comma(nrow(occurencePL)), HTML("</b>observations and <b>"),length(unique(occurencePL$family)), "</b>families.<br><br>"),"
           
            You can:
            <ul>
            <li><b>Search by vernacular/scientific name</b></li>
            <li><b>Set the date range</b> of the observations<br></li>
            <li><b>Click on the marker</b> to see more information about the observation.</li>
            <li><b>Locate yourself</b> by clickin on the target icon on the map.</li>
            <br>
            </ul>

            We've jumped ahead and selected a random bird specie for you to see. Enjoy the view!<br><br>

            <i><b>“The bird who dares to fall is the bird who learns to fly.”</i></b>")),
   
    easyClose = F,
    footer = tagList(
      actionButton("start", "Start exploring!")
    )
  )

  # Show modal message on startup
  showModal(welcome_modal)

  observeEvent(input$start, { removeModal() })
  
  # ------------------------------------------------------------------------------
  
}

shinyApp(ui, server)